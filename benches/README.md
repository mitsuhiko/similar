# Benchmarks

The benchmark suite separates algorithm cost from text tokenization:

- `diffs.rs` is a Criterion suite covering small text fixtures, large sparse and
  disjoint sequences, dense repeated inputs at multiple sizes, and end-to-end
  line tokenization.
- `allocations.rs` uses a tracking global allocator to report allocation calls,
  cumulative requested bytes, peak live requested bytes, and bytes retained by
  the result. Allocator metadata and fragmentation are intentionally excluded,
  which makes the output deterministic and useful for before/after comparisons.

Run the timing suite with:

```console
cargo bench --bench diffs
```

Run the allocation report with:

```console
cargo bench --bench allocations
```

Criterion filters can narrow a run, for example:

```console
cargo bench --bench diffs -- dense_repeated_scaling
```

Input generation and fixture loading happen outside timed iterations. Every
iteration includes construction and destruction of the resulting diff so that
allocation and cleanup costs are represented.
