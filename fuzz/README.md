# Diff performance fuzzer

`examples/perf-fuzz.rs` is a generative performance suite for finding inputs
that make Similar's diff heuristics unexpectedly expensive. It creates pairs of
readable text files, runs every `Algorithm` in an isolated worker process, and
records cases that exceed a wall-clock budget or crash.

The default budget is **2 seconds per algorithm**. That is intentionally much
larger than an interactive diff should normally take, while still making a
local campaign practical. Each generated file is capped at **10,000,000 bytes**.
The cap is enforced by both the generator and workers, including replay runs.

## Run

Always use a release build. The repository's release profile retains debug
information so process stack dumps have useful symbols.

```console
cargo run --release --example perf-fuzz
# equivalent: make perf-fuzz
```

Pass options through Make with `PERF_FUZZ_ARGS`, or invoke Cargo directly as in
the examples below.

A smaller smoke campaign:

```console
cargo run --release --example perf-fuzz -- \
  --cases 3 --max-bytes 250000 --timeout-ms 500 --seed 42
```

Useful options:

```text
--cases N                 number of generated pairs (default 12)
--seed N                  deterministic campaign seed
--timeout-ms N            stack/kill threshold for each algorithm (default 2000)
--max-bytes N             per-file generator cap (hard maximum 10000000)
--out DIR                 use an exact output directory
--keep-all                retain successful inputs in addition to findings
--no-stacks               disable process stack collection
--no-internal-deadline    test the no-deadline path (less safe for LCS)
```

The default run covers all current algorithms: Myers, Raw Myers, Patience,
LCS, Hunt, and Histogram. Raw Myers is intentionally unbounded apart from the
configured deadline and serves as an exact-search baseline.
Cases grow from 64 KiB to the configured maximum and rotate through
patterns intended to stress different heuristics:

- low-cardinality cycles and phase-shifted bands;
- mostly disjoint inputs with a few misleading common anchors;
- rotated and shuffled blocks;
- heavily unbalanced inputs with sparse overlap;
- long repeated runs with sparse separators; and
- periodic edits in repetitive inputs.

File loading and UTF-8 validation happen before a ready-file handshake. The
measured region includes line tokenization, integer remapping, and the algorithm
itself through `TextDiff::diff_lines`.

## Timeouts and stack dumps

Workers are separate processes so the controller can enforce a hard wall-clock
budget even if an algorithm does not check a deadline promptly. By default, the
worker also receives a library deadline later than the controller threshold.
This makes LCS table construction incremental and acts as a safety net if the
controller dies; the controller normally samples and kills the worker first.
Use `--no-internal-deadline` to exercise the exact no-deadline path, but be aware
that large LCS inputs can request enormous allocations.

At the timeout the controller tries:

- macOS: `sample`, then `lldb` as a fallback;
- Linux: `gdb`, `eu-stack`, `pstack`, then `lldb`.

If no dumper can attach, a `.stack.txt` file is still written with the failed
attempts and setup advice. On Linux the short-lived worker calls
`PR_SET_PTRACER_ANY` so a sibling debugger can attach under the common Yama
`ptrace_scope=1` policy; stricter host or container policies can still block it.

## Findings

By default each campaign is written below:

```text
fuzz/findings/run-<time>-<pid>-<seed>/
├── summary.md
├── results.tsv
└── case-<index>-<strategy>-<seed>/
    ├── README.md
    ├── recipe.txt
    ├── before.txt
    ├── after.txt
    ├── <algorithm>.stack.txt
    ├── <algorithm>.stderr.txt  # when non-empty
    └── <algorithm>.stdout.txt  # when non-empty for a non-completed run
```

Only timeout/crash inputs are retained unless `--keep-all` is supplied.
`results.tsv` contains the complete algorithm/case timing matrix. Every finding
README explains the generator recipe and contains a ready-to-copy replay
command.

To replay a recorded pair against all algorithms:

```console
cargo run --release --example perf-fuzz -- replay \
  --algorithm all --timeout-ms 2000 \
  --old fuzz/findings/<run>/<case>/before.txt \
  --new fuzz/findings/<run>/<case>/after.txt
```

To focus on one implementation, replace `all` with `myers`,
`raw-myers`, `patience`, `lcs`, `hunt`, or `histogram`.

## Agent workflow

Give an agent the finding directory, particularly `README.md`, the relevant
stack file, and the two inputs. A useful loop is:

1. inspect the hottest/repeating stack frames;
2. infer which input property defeats the existing preflight or anchor logic;
3. replay the finding before and after a heuristic change;
4. run the normal tests and benchmark suite; and
5. rerun the finding with a lower timeout or add its pattern as a regression
   benchmark.

Generated files are deliberately textual and recipes are recorded so findings
can be understood without decoding a binary fuzz corpus.
