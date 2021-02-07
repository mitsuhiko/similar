use std::fmt;

use console::{style, Style};
use similar::{Algorithm, ChangeTag, TextDiff};

pub struct Diff {
    left: String,
    right: String,
}

fn to_debug<D: fmt::Debug>(d: &D) -> String {
    use fmt::Write;
    let mut buf = String::new();
    buf.write_fmt(format_args!("{:#?}", d))
        .expect("a Debug implementation returned an error unexpectedly");
    buf
}

impl Diff {
    pub fn from_debug<Left: fmt::Debug, Right: fmt::Debug>(left: &Left, right: &Right) -> Diff {
        let left = to_debug(left).into();
        let right = to_debug(right).into();
        Diff { left, right }
    }
}

impl fmt::Display for Diff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.left == self.right {
            writeln!(
                f,
                "{}: {}",
                style("Invisible differences").bold(),
                "the two values are the same in string form."
            )?;
            return Ok(());
        }

        let diff = TextDiff::configure()
            .algorithm(Algorithm::Patience)
            .diff_lines(&self.left, &self.right);
        writeln!(
            f,
            "  {} ({}|{}):",
            style("Differences").bold(),
            style("<left").red(),
            style(">right").green(),
        )?;
        for (idx, group) in diff.grouped_ops(4).into_iter().enumerate() {
            if idx > 0 {
                writeln!(f, "  ~~~")?;
            }
            for op in group {
                for change in diff.iter_inline_changes(&op) {
                    let (marker, style) = match change.tag() {
                        ChangeTag::Delete => ('<', Style::new().red()),
                        ChangeTag::Insert => ('>', Style::new().green()),
                        ChangeTag::Equal => (' ', Style::new().dim()),
                    };
                    write!(f, "   {}", style.apply_to(marker).dim().bold())?;
                    #[cfg(feature = "inline")]
                    {
                        for &(emphasized, value) in change.values() {
                            if emphasized {
                                write!(f, "{}", style.clone().underlined().bold().apply_to(value))?;
                            } else {
                                write!(f, "{}", style.apply_to(value))?;
                            }
                        }
                    }
                    #[cfg(not(feature = "inline"))]
                    {
                        write!(f, "{}", style.apply_to(change.value()))?;
                    }
                    if change.missing_newline() {
                        writeln!(f)?;
                    }
                }
            }
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! assert_eq {
    ($left:expr, $right:expr $(,)?) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    panic!("assertion failed: `(left == right)`'\
                           \n  left: `{:?}`\
                           \n right: `{:?}`\
                           \n\n{}\n",
                           &*left_val,
                           &*right_val,
                           $crate::Diff::from_debug(&*left_val, &*right_val));
                }
            }
        }
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    panic!("assertion failed: `(left == right)`: {}'\
                           \n  left: `{:?}`\
                           \n right: `{:?}`\
                           \n\n{}\n",
                           format_args!($($arg)*),
                           &*left_val,
                           &*right_val,
                           $crate::Diff::from_debug(&*left_val, &*right_val));
                }
            }
        }
    });
}

fn main() {
    let vec1: Vec<_> = (1..100).collect();
    let mut vec2 = vec1.clone();
    vec2[52] = 2;
    vec2[12] = 33;
    assert_eq!(vec1, vec2);
}
