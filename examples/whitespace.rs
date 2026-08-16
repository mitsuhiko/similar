use similar::{TextDiff, WhitespaceMode};

const OLD: &str = "\
<foo>
  <bar/>
  <baz/>
</foo>
";

const NEW: &str = "\
<foo>
  <foo2>
    <bar/>
    <baz/>
  </foo2>
</foo>
";

fn main() {
    // Ignore changes in the amount of whitespace, like `git diff -b`.
    let diff = TextDiff::configure()
        .whitespace_mode(WhitespaceMode::IgnoreChanges)
        .diff_lines(OLD, NEW);

    print!("{}", diff.unified_diff());
}
