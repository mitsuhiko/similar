# Changelog

All notable changes to similar are documented here.

## 0.5.0

* Add `DiffOp::apply_to_hook` to apply a captured op to a diff hook.
* Added virtual newline handling to `iter_changes`.
* Made unified diff support more flexible through the introduction of
  the `UnifiedDiff` type.
* Fixed grouped diff operation to return an empty result if the diff
  does not show any changes.

## 0.4.0

* Change `get_close_matches` to use Python's quick ratio optimization
  and order lexicographically when tied.

## 0.3.0

* Added grapheme and character level diffing utilities.
* `DiffOp::as_tag_tuple` is now taking the argument by reference.
* Added `TextDiff::ratio`.
* Added `get_close_matches`.

## 0.2.0

* Fixed a bug in the patience algorithm causing it not not work.

## 0.1.0

* Initial release.
