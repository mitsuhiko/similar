use std::fs::read_to_string;
use std::process::exit;

use similar::text::TextDiff;

fn main() {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 3 {
        eprintln!("usage: udiff [old] [new]");
        exit(1);
    }

    let old = read_to_string(&args[1]).unwrap();
    let new = read_to_string(&args[2]).unwrap();
    print!(
        "{}",
        TextDiff::from_lines(&old, &new).unified_diff().header(
            &args[1].as_os_str().to_string_lossy(),
            &args[2].as_os_str().to_string_lossy()
        )
    );
}
