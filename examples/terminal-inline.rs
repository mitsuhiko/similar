use console::Style;
use similar::text::{ChangeTag, TextDiff};

fn main() {
    let diff = TextDiff::from_lines(
        "schtzngrmm\nschtzngrmm\nt-t-t-t\nt-t-t-t\ngrrrmmmmm\nt-t-t-t\n\
         s---------c---------h\ntzngrmm\ntzngrmm\ntzngrmm\ngrrrmmmmm\n\
         schtzn\nschtzn\nt-t-t-t\nt-t-t-t\nschtzngrmm\nschtzngrmm\n\
         tssssssssssssss\ngrrt\ngrrrrrt\ngrrrrrrrrrt\nscht\nscht\n\
         t-t-t-t-t-t-t-t-t-t\nscht\ntzngrmm\ntzngrmm\nt-t-t-t-t-t-t-t-t-t\n\
         scht\nscht\nscht\nscht\nscht\ngrrrrrrrrrrrrrrrrrrrrrrrrrrrr\nt-tt",
        "schützengraben\nschützengraben\nt-t-t-t\nt-t-t-t\ngrrrmmmmm\nt-t-t-t\n\
         s---------c---------h\ntzngrmm\ntzngrmm\ntzngrmm\ngrrrmmmmm\nschützen\n\
         schützen\nt-t-t-t\nt-t-t-t\nschützengraben\nschützengraben\n\
         tssssssssssssss\ngrrt\ngrrrrrt\ngrrrrrrrrrt\nscht\nscht\n\
         t-t-t-t-t-t-t-t-t-t\nscht\ntzngrmm\ntzngrmm\nt-t-t-t-t-t-t-t-t-t\n\
         scht\nscht\nscht\nscht\nscht\ngrrrrrrrrrrrrrrrrrrrrrrrrrrrr\nt-tt",
    );

    for op in diff.ops() {
        for change in diff.iter_inline_changes(op) {
            let (sign, style) = match change.tag() {
                ChangeTag::Delete => ("-", Style::new().red()),
                ChangeTag::Insert => ("+", Style::new().green()),
                ChangeTag::Equal => (" ", Style::new()),
            };
            print!("{}", style.apply_to(sign).bold(),);
            for &(emphasized, value) in change.values() {
                if emphasized {
                    print!("{}", style.apply_to(value).underlined());
                } else {
                    print!("{}", style.apply_to(value));
                }
            }
            if change.is_missing_newline() {
                println!();
            }
        }
    }
}
