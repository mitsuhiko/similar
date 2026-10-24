use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;

use super::*;

fn script(old: &str, new: &str) -> Vec<EditAction> {
    EditScript::from_lines(old, new).into_actions()
}

fn describe(old: &str, new: &str) -> Vec<String> {
    script(old, new).iter().map(|x| x.to_string()).collect()
}

#[test]
fn test_no_changes() {
    assert!(EditScript::from_lines("a\nb\n", "a\nb\n").is_empty());
    assert!(EditScript::from_lines("", "").is_empty());
}

#[test]
fn test_plain_insert_and_delete() {
    insta::assert_debug_snapshot!(describe("a\nb\nc\n", "a\nx\ny\nc\n"), @r#"
    [
        "delete line 2",
        "insert lines 2-3",
    ]
    "#);
}

#[test]
fn test_move_function() {
    let old = "\
def helper():
    work()
    more_work()

def main():
    run()
    cleanup()
";
    let new = "\
def main():
    run()
    cleanup()

def helper():
    work()
    more_work()
";
    insta::assert_debug_snapshot!(describe(old, new), @r#"
    [
        "move 3 lines from line 5 to line 1",
        "insert line 4",
        "delete line 4",
    ]
    "#);
}

#[test]
fn test_move_with_indentation_change() {
    let old = "\
def run():
    prepare()
    for item in items:
        process(item)
        report(item)
    finish()
";
    let new = "\
def run():
    prepare()
    if items:
        for item in items:
            process(item)
            report(item)
    finish()
";
    insta::assert_debug_snapshot!(describe(old, new), @r#"
    [
        "insert line 3",
        "move 3 lines from line 3 to line 4, indented by 4",
    ]
    "#);
}

#[test]
fn test_move_with_update() {
    let old = "\
first_function(alpha, beta)
second_function(gamma)
third_function(delta)
unchanged_one()
unchanged_two()
unchanged_three()
";
    let new = "\
unchanged_one()
unchanged_two()
unchanged_three()
first_function(alpha, beta)
second_function(gamma, omega)
third_function(delta)
";
    insta::assert_debug_snapshot!(describe(old, new), @r#"
    [
        "move 3 lines from line 1 to line 4",
        "update line 2 to line 5",
    ]
    "#);
}

#[test]
fn test_copy_block() {
    let old = "\
fn a() {
    let value = compute();
    let other = derive(value);
    store(value, other);
}

fn b() {
    unrelated();
}
";
    let new = "\
fn a() {
    let value = compute();
    let other = derive(value);
    store(value, other);
}

fn b() {
    unrelated();
    let value = compute();
    let other = derive(value);
    store(value, other);
}
";
    insta::assert_debug_snapshot!(describe(old, new), @r#"
    [
        "copy 3 lines from line 2 to line 9",
    ]
    "#);
    // two lines are below the default minimum for copies
    let short = EditScript::from_lines(
        "a()\nlet x = 1;\nlet y = 2;\nz()\n",
        "a()\nlet x = 1;\nlet y = 2;\nz()\nlet x = 1;\nlet y = 2;\n",
    );
    assert_eq!(
        short.actions(),
        &[EditAction::Insert {
            new_index: 4,
            new_len: 2
        }]
    );
    let short = EditScript::configure().min_copy_lines(2).diff_lines(
        "a()\nlet x = 1;\nlet y = 2;\nz()\n",
        "a()\nlet x = 1;\nlet y = 2;\nz()\nlet x = 1;\nlet y = 2;\n",
    );
    assert!(matches!(short.actions(), [EditAction::Copy { len: 2, .. }]));
}

#[test]
fn test_split_and_merge() {
    insta::assert_debug_snapshot!(
        describe("call(alpha, beta, gamma)\nend\n", "call(alpha,\n     beta,\n     gamma)\nend\n"),
        @r#"
    [
        "split line 1 into lines 1-3",
    ]
    "#
    );
    insta::assert_debug_snapshot!(
        describe("call(alpha,\n     beta,\n     gamma)\nend\n", "call(alpha, beta, gamma)\nend\n"),
        @r#"
    [
        "merge lines 1-3 into line 1",
    ]
    "#
    );
}

#[test]
fn test_update() {
    let old = "a\nlet total = price * count;\nb\n";
    let new = "a\nlet total = price * amount;\nb\n";
    assert_eq!(
        script(old, new),
        vec![EditAction::Update {
            old_index: 1,
            new_index: 1,
        }]
    );
}

#[test]
fn test_dissimilar_lines_are_not_updates() {
    let old = "a\nlet total = price * count;\nb\n";
    let new = "a\nreturn;\nb\n";
    assert_eq!(describe(old, new), vec!["delete line 2", "insert line 2"]);
}

#[test]
fn test_disabled_detection() {
    let old = "alpha one\nalpha two\nbeta one\nbeta two\nbeta three\n";
    let new = "beta one\nbeta two\nbeta three\nalpha one\nalpha two\n";
    let all = EditScript::from_lines(old, new);
    assert!(all.iter().any(|x| matches!(x, EditAction::Move { .. })));
    let none = EditScript::configure()
        .moves(false)
        .copies(false)
        .updates(false)
        .splits(false)
        .diff_lines(old, new);
    assert!(
        none.iter()
            .all(|x| matches!(x, EditAction::Delete { .. } | EditAction::Insert { .. }))
    );
}

#[test]
fn test_min_block_lines() {
    let old = "alpha one\nalpha two\nbeta one\nbeta two\nbeta three\n";
    let new = "beta one\nbeta two\nbeta three\nalpha one\nalpha two\n";
    assert!(
        EditScript::from_lines(old, new)
            .iter()
            .any(|x| matches!(x, EditAction::Move { .. }))
    );
    assert!(
        !EditScript::configure()
            .min_move_lines(3)
            .copies(false)
            .diff_lines(old, new)
            .iter()
            .any(|x| matches!(x, EditAction::Move { .. }))
    );
}

#[test]
fn test_display() {
    let actions = [
        EditAction::Delete {
            old_index: 0,
            old_len: 1,
        },
        EditAction::Insert {
            new_index: 1,
            new_len: 3,
        },
        EditAction::Update {
            old_index: 4,
            new_index: 5,
        },
        EditAction::Split {
            old_index: 0,
            new_index: 0,
            new_len: 2,
        },
        EditAction::Merge {
            old_index: 2,
            old_len: 2,
            new_index: 1,
        },
        EditAction::Move {
            old_index: 0,
            new_index: 9,
            len: 3,
            indent: -4,
        },
        EditAction::Copy {
            old_index: 0,
            new_index: 9,
            len: 1,
            indent: 2,
        },
    ];
    insta::assert_debug_snapshot!(actions.iter().map(|x| x.to_string()).collect::<Vec<_>>(), @r#"
    [
        "delete line 1",
        "insert lines 2-4",
        "update line 5 to line 6",
        "split line 1 into lines 1-2",
        "merge lines 3-4 into line 2",
        "move 3 lines from line 1 to line 10, dedented by 4",
        "copy 1 line from line 1 to line 10, indented by 2",
    ]
    "#);
}

/// Checks that every changed line is explained exactly once.
fn check_invariants(old: &str, new: &str, config: &EditScriptConfig) {
    let diff = TextDiff::configure()
        .algorithm(Algorithm::Histogram)
        .diff_lines(old, new);
    let script = config.build(&diff);

    let mut old_changed = vec![false; diff.old_len()];
    let mut new_changed = vec![false; diff.new_len()];
    for op in diff.ops() {
        if !matches!(op, DiffOp::Equal { .. }) {
            old_changed[op.old_range()].fill(true);
            new_changed[op.new_range()].fill(true);
        }
    }

    let block_new = script
        .iter()
        .filter(|x| matches!(x, EditAction::Move { .. } | EditAction::Copy { .. }))
        .flat_map(|x| x.new_range())
        .collect::<Vec<_>>();
    let mut old_count = vec![0; diff.old_len()];
    let mut new_count = vec![0; diff.new_len()];
    for action in script.iter() {
        let in_block =
            matches!(action, EditAction::Update { new_index, .. } if block_new.contains(new_index));
        if in_block {
            continue;
        }
        if !matches!(action, EditAction::Copy { .. }) {
            for l in action.old_range() {
                old_count[l] += 1;
            }
        }
        for r in action.new_range() {
            new_count[r] += 1;
        }
    }
    for (l, changed) in old_changed.iter().enumerate() {
        assert_eq!(old_count[l], *changed as usize, "old line {l}: {script:?}");
    }
    for (r, changed) in new_changed.iter().enumerate() {
        assert_eq!(new_count[r], *changed as usize, "new line {r}: {script:?}");
    }
    for action in script.iter() {
        if let EditAction::Move {
            old_index,
            new_index,
            len,
            indent,
        }
        | EditAction::Copy {
            old_index,
            new_index,
            len,
            indent,
        } = *action
        {
            let old_lines = old.lines().collect::<Vec<_>>();
            let new_lines = new.lines().collect::<Vec<_>>();
            for k in 0..len {
                let a = LineInfo::new(old_lines[old_index + k].as_bytes(), 4);
                let b = LineInfo::new(new_lines[new_index + k].as_bytes(), 4);
                assert_eq!(a.is_blank(), b.is_blank());
                if !a.is_blank() {
                    assert_eq!(b.indent as isize - a.indent as isize, indent);
                }
            }
        }
    }
}

#[test]
fn test_random_invariants() {
    let mut seed = 0x9e37_79b9_7f4a_7c15u64;
    let mut random = move |n: usize| {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        (seed % n as u64) as usize
    };
    let vocabulary = [
        "",
        "}",
        "foo(a, b)",
        "foo(a,",
        "b)",
        "    let x = value;",
        "    let y = value;",
        "    let x = other;",
        "fn main() {",
        "        nested();",
        "return result;",
    ];
    let configs = [
        EditScriptConfig::new(),
        EditScriptConfig::new().min_copy_lines(1).clone(),
        EditScriptConfig::new().block_updates(false).clone(),
    ];
    for _ in 0..1500 {
        let old = (0..random(25))
            .map(|_| vocabulary[random(vocabulary.len())])
            .collect::<Vec<_>>()
            .join("\n");
        let new = (0..random(25))
            .map(|_| vocabulary[random(vocabulary.len())])
            .collect::<Vec<_>>()
            .join("\n");
        for config in &configs {
            check_invariants(&old, &new, config);
        }
    }
}

#[test]
fn test_large_hunk() {
    let old = (0..400)
        .map(|idx| format!("let value_{idx} = compute({idx});\n"))
        .collect::<String>();
    let new = (0..400)
        .map(|idx| format!("let value_{idx} = compute({idx}, extra);\n"))
        .collect::<String>();
    check_invariants(&old, &new, &EditScriptConfig::new());
    // Every line is similar to many others here, so not every pairing is
    // found, but the bulk of the lines must be reported as updates.
    let script = EditScript::from_lines(&old, &new);
    let diagonal_updates = script
        .iter()
        .filter(
            |x| matches!(x, EditAction::Update { old_index, new_index } if old_index == new_index),
        )
        .count();
    assert!(diagonal_updates > 300, "{diagonal_updates}");
}
