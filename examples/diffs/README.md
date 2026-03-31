# Diff input samples

All pairs use a consistent naming scheme:

- `caseNN_<scenario>.before.txt`
- `caseNN_<scenario>.after.txt`

## Cases

1. **case01_simple_edit**  
   Straightforward line edits and small value changes.

2. **case02_patience_reorder**  
   Section reorder + small edits. Useful for seeing how patience diff handles moved blocks.

3. **case03_repeated_lines**  
   Repeated near-identical lines with one replacement and one insertion. Good for ambiguous anchor behavior.

4. **case04_code_refactor**  
   Realistic code-like refactor (extract helper + behavior tweak via `dedup`).

5. **case05_whitespace_punctuation**  
   Case/style/punctuation adjustments plus version/number changes.

6. **case06_insertions_edges**  
   Insertions at file start and end (edge hunks).
