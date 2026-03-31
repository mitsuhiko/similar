use std::hint::black_box;
use std::sync::OnceLock;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use similar::{Algorithm, TextDiff};

#[derive(Clone, Copy)]
enum DiffMode {
    Lines,
    Words,
}

#[derive(Clone, Copy)]
struct BenchCase {
    name: &'static str,
    mode: DiffMode,
    algorithm: Algorithm,
    old: &'static str,
    new: &'static str,
}

const SAMPLE_CASES: &[BenchCase] = &[
    BenchCase {
        name: "case01_simple_edit",
        mode: DiffMode::Lines,
        algorithm: Algorithm::Myers,
        old: include_str!("../examples/diffs/case01_simple_edit.before.txt"),
        new: include_str!("../examples/diffs/case01_simple_edit.after.txt"),
    },
    BenchCase {
        name: "case02_patience_reorder",
        mode: DiffMode::Lines,
        algorithm: Algorithm::Patience,
        old: include_str!("../examples/diffs/case02_patience_reorder.before.txt"),
        new: include_str!("../examples/diffs/case02_patience_reorder.after.txt"),
    },
    BenchCase {
        name: "case03_repeated_lines",
        mode: DiffMode::Lines,
        algorithm: Algorithm::Myers,
        old: include_str!("../examples/diffs/case03_repeated_lines.before.txt"),
        new: include_str!("../examples/diffs/case03_repeated_lines.after.txt"),
    },
    BenchCase {
        name: "case04_code_refactor",
        mode: DiffMode::Lines,
        algorithm: Algorithm::Myers,
        old: include_str!("../examples/diffs/case04_code_refactor.before.txt"),
        new: include_str!("../examples/diffs/case04_code_refactor.after.txt"),
    },
    BenchCase {
        name: "case05_whitespace_punctuation_words",
        mode: DiffMode::Words,
        algorithm: Algorithm::Myers,
        old: include_str!("../examples/diffs/case05_whitespace_punctuation.before.txt"),
        new: include_str!("../examples/diffs/case05_whitespace_punctuation.after.txt"),
    },
    BenchCase {
        name: "case06_insertions_edges",
        mode: DiffMode::Lines,
        algorithm: Algorithm::Myers,
        old: include_str!("../examples/diffs/case06_insertions_edges.before.txt"),
        new: include_str!("../examples/diffs/case06_insertions_edges.after.txt"),
    },
];

fn run_case(case: BenchCase) -> usize {
    let mut config = TextDiff::configure();
    config.algorithm(case.algorithm);

    let diff = match case.mode {
        DiffMode::Lines => config.diff_lines(case.old, case.new),
        DiffMode::Words => config.diff_words(case.old, case.new),
    };

    diff.ops().len()
}

fn bench_sample_cases(c: &mut Criterion) {
    let mut group = c.benchmark_group("sample_cases");
    group.sample_size(30);

    for &case in SAMPLE_CASES {
        group.throughput(Throughput::Bytes((case.old.len() + case.new.len()) as u64));
        group.bench_with_input(
            BenchmarkId::new(case.name, format!("{:?}", case.algorithm)),
            &case,
            |b, &case| {
                b.iter(|| black_box(run_case(case)));
            },
        );
    }

    group.finish();
}

fn large_sparse_files() -> &'static (String, String) {
    static DATA: OnceLock<(String, String)> = OnceLock::new();
    DATA.get_or_init(|| {
        let mut old = String::new();
        let mut new = String::new();

        new.push_str("// inserted header line 1\n");
        new.push_str("// inserted header line 2\n");

        for i in 0..60_000 {
            let base = format!("record {i:06}: alpha beta gamma delta epsilon\n");
            old.push_str(&base);

            if i % 8_000 == 0 {
                new.push_str(&format!(
                    "record {i:06}: alpha beta gamma DELTA epsilon (edited)\n"
                ));
                new.push_str(&format!("record {i:06}.1: inserted sibling record\n"));
            } else {
                new.push_str(&base);
            }
        }

        new.push_str("// inserted footer line\n");

        (old, new)
    })
}

fn pathological_repeated_files() -> &'static (String, String) {
    static DATA: OnceLock<(String, String)> = OnceLock::new();
    DATA.get_or_init(|| {
        let mut old = String::new();
        let mut new = String::new();

        for i in 0..20_000 {
            let old_bucket = i % 8;
            let new_bucket = (i + 1) % 8;

            old.push_str(&format!("bucket-{old_bucket}: repeated payload\n"));
            new.push_str(&format!("bucket-{new_bucket}: repeated payload\n"));

            if i % 4_000 == 0 {
                old.push_str(&format!("anchor-old-{i:05}\n"));
                new.push_str(&format!("anchor-new-{i:05}\n"));
            }
        }

        (old, new)
    })
}

fn bench_large_cases(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_cases");
    group.sample_size(10);

    let (old_sparse, new_sparse) = large_sparse_files();
    group.throughput(Throughput::Bytes(
        (old_sparse.len() + new_sparse.len()) as u64,
    ));
    group.bench_function("large_sparse_file_lines::Myers", |b| {
        b.iter(|| {
            let mut config = TextDiff::configure();
            config.algorithm(Algorithm::Myers);
            let diff = config.diff_lines(black_box(old_sparse), black_box(new_sparse));
            black_box(diff.ops().len())
        });
    });

    let (old_pathological, new_pathological) = pathological_repeated_files();
    group.throughput(Throughput::Bytes(
        (old_pathological.len() + new_pathological.len()) as u64,
    ));
    group.bench_function("pathological_repeated_file_lines::Myers", |b| {
        b.iter(|| {
            let mut config = TextDiff::configure();
            config.algorithm(Algorithm::Myers);
            let diff = config.diff_lines(black_box(old_pathological), black_box(new_pathological));
            black_box(diff.ops().len())
        });
    });

    group.finish();
}

criterion_group!(benches, bench_sample_cases, bench_large_cases);
criterion_main!(benches);
