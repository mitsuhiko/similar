# Upstream Tracker

Triage of [mitsuhiko/similar](https://github.com/mitsuhiko/similar) PRs and
issues against this fork.

## Upstream PRs

| PR | Description | Status |
|----|-------------|--------|
| [#73](https://github.com/mitsuhiko/similar/pull/73) | Add `wasm32_web_time` feature | cherry-picked |
| [#59](https://github.com/mitsuhiko/similar/pull/59) | Fix overlap bug in LCS | cherry-picked |
| [#42](https://github.com/mitsuhiko/similar/pull/42) | WebAssembly WIT bindings | not actionable — stale since 2022, WAPM ecosystem shifted |
| [#32](https://github.com/mitsuhiko/similar/pull/32) | Add histogram diff algorithm | not actionable — closed incomplete |
| [#86](https://github.com/mitsuhiko/similar/pull/86) | Make the configuration const compatible | open |
| [#87](https://github.com/mitsuhiko/similar/pull/87) | Exclude development scripts | open |

## Issues

| Issue | Description | Status |
|-------|-------------|--------|
| [#9](https://github.com/mitsuhiko/similar/issues/9) | Histogram diff algorithm | open |
| [#15](https://github.com/mitsuhiko/similar/issues/15) | Implement Myers heuristics | open — partial (deadline support exists) |
| [#19](https://github.com/mitsuhiko/similar/issues/19) | New trait bounds | open |
| [#24](https://github.com/mitsuhiko/similar/issues/24) | Improved inline highlighting | open — partial (word-level inline exists) |
| [#25](https://github.com/mitsuhiko/similar/issues/25) | Semantic cleanups for char diffs | open — partial (`Compact` exists) |
| [#27](https://github.com/mitsuhiko/similar/issues/27) | Custom comparison with `capture_diff_slices_by` | open |
| [#33](https://github.com/mitsuhiko/similar/issues/33) | `Index` trait bound blocks owned values | open |
| [#39](https://github.com/mitsuhiko/similar/issues/39) | WIT bindings + WAPM | not actionable — packaging, not library |
| [#44](https://github.com/mitsuhiko/similar/issues/44) | LCS isn't Hunt-McIlroy, possible correctness bug | open |
| [#45](https://github.com/mitsuhiko/similar/issues/45) | Support for `Bytes` from bytes crate | open — partial (`&[u8]` works via `bstr`) |
| [#46](https://github.com/mitsuhiko/similar/issues/46) | Compiling to WASM | fixed — `wasm32_web_time` feature |
| [#65](https://github.com/mitsuhiko/similar/issues/65) | Return owned diff with `TextDiff::configure().diff_lines` | fixed — `DiffableStrRef` trait |
| [#77](https://github.com/mitsuhiko/similar/issues/77) | Typo: "feataure" in docs | open |
| [#78](https://github.com/mitsuhiko/similar/issues/78) | `DiffOp` has incorrect `new_index` | open |
| [#79](https://github.com/mitsuhiko/similar/issues/79) | `TextDiff::from_lines` freezes on very large diffs | open — partial (deadline system exists) |
| [#80](https://github.com/mitsuhiko/similar/issues/80) | Continuous Delete operations disassembled | fixed — `Compact` algorithm |
| [#81](https://github.com/mitsuhiko/similar/issues/81) | Squash/compose two diffs | not actionable — niche, no demand |
| [#82](https://github.com/mitsuhiko/similar/issues/82) | Floating point comparison within eps/ulps | fixed — `capture_diff_slices_fp` |
| [#84](https://github.com/mitsuhiko/similar/issues/84) | Inline character-level diffs with `from_lines` | fixed — `iter_inline_changes` |
| [#85](https://github.com/mitsuhiko/similar/issues/85) | Gallery for outputs of examples | open |
