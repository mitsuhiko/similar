# Diff input samples

All pairs use a consistent naming scheme designed for lexicographic glob expansion (`caseNN.*.txt`):

- `caseNN.01.before_<scenario>.txt` (old)
- `caseNN.02.after_<scenario>.txt` (new)

## Cases

1. **case01** (`case01.01.before_simple_edit.txt` → `case01.02.after_simple_edit.txt`)  
   Straightforward line edits and small value changes.

2. **case02** (`case02.01.before_patience_reorder.txt` → `case02.02.after_patience_reorder.txt`)  
   Section reorder + small edits. Useful for seeing how patience diff handles moved blocks.

3. **case03** (`case03.01.before_repeated_lines.txt` → `case03.02.after_repeated_lines.txt`)  
   Repeated near-identical lines with one replacement and one insertion. Good for ambiguous anchor behavior.

4. **case04** (`case04.01.before_code_refactor.txt` → `case04.02.after_code_refactor.txt`)  
   Realistic code-like refactor (extract helper + behavior tweak via `dedup`).

5. **case05** (`case05.01.before_whitespace_punctuation.txt` → `case05.02.after_whitespace_punctuation.txt`)  
   Case/style/punctuation adjustments plus version/number changes.

6. **case06** (`case06.01.before_insertions_edges.txt` → `case06.02.after_insertions_edges.txt`)  
   Insertions at file start and end (edge hunks).
