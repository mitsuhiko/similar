use std::fs::read_to_string;
use std::process::exit;

use similar::{Algorithm, EditAction, EditScript, TextDiff};

fn main() {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 3 {
        eprintln!("usage: edit-script [old] [new]");
        exit(1);
    }
    let old = read_to_string(&args[1]).unwrap();
    let new = read_to_string(&args[2]).unwrap();
    let diff = TextDiff::configure()
        .algorithm(Algorithm::Histogram)
        .diff_lines(&old, &new);
    let old_lines: Vec<_> = diff.iter_old_slices().collect();
    let new_lines: Vec<_> = diff.iter_new_slices().collect();

    for action in EditScript::new(&diff).iter() {
        println!("{action}");
        // moves and copies only show their first line to keep the output short
        let show = match action {
            EditAction::Move { .. } | EditAction::Copy { .. } => 1,
            _ => usize::MAX,
        };
        if !matches!(action, EditAction::Copy { .. }) {
            for line in action.old_range().take(show) {
                print!("  - {}", old_lines[line]);
            }
        }
        for line in action.new_range().take(show) {
            print!("  + {}", new_lines[line]);
        }
    }
}
