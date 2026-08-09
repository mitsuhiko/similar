use std::fmt::Write as _;
use std::hint::black_box;
use std::sync::OnceLock;
use std::time::Duration;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use similar::{Algorithm, TextDiff, capture_diff_slices};

const ALL_ALGORITHMS: &[Algorithm] = &[
    Algorithm::Myers,
    Algorithm::Patience,
    Algorithm::Lcs,
    Algorithm::Hunt,
    Algorithm::Histogram,
];

const SCALABLE_ALGORITHMS: &[Algorithm] = &[
    Algorithm::Myers,
    Algorithm::Patience,
    Algorithm::Hunt,
    Algorithm::Histogram,
];

#[derive(Clone, Copy)]
enum DiffMode {
    Lines,
    Words,
}

#[derive(Clone, Copy)]
struct Fixture {
    name: &'static str,
    mode: DiffMode,
    old: &'static str,
    new: &'static str,
}

const FIXTURES: &[Fixture] = &[
    Fixture {
        name: "simple_edit_lines",
        mode: DiffMode::Lines,
        old: include_str!("../examples/diffs/case01.01.before_simple_edit.txt"),
        new: include_str!("../examples/diffs/case01.02.after_simple_edit.txt"),
    },
    Fixture {
        name: "patience_reorder_lines",
        mode: DiffMode::Lines,
        old: include_str!("../examples/diffs/case02.01.before_patience_reorder.txt"),
        new: include_str!("../examples/diffs/case02.02.after_patience_reorder.txt"),
    },
    Fixture {
        name: "repeated_lines",
        mode: DiffMode::Lines,
        old: include_str!("../examples/diffs/case03.01.before_repeated_lines.txt"),
        new: include_str!("../examples/diffs/case03.02.after_repeated_lines.txt"),
    },
    Fixture {
        name: "code_refactor_lines",
        mode: DiffMode::Lines,
        old: include_str!("../examples/diffs/case04.01.before_code_refactor.txt"),
        new: include_str!("../examples/diffs/case04.02.after_code_refactor.txt"),
    },
    Fixture {
        name: "whitespace_punctuation_words",
        mode: DiffMode::Words,
        old: include_str!("../examples/diffs/case05.01.before_whitespace_punctuation.txt"),
        new: include_str!("../examples/diffs/case05.02.after_whitespace_punctuation.txt"),
    },
    Fixture {
        name: "insertions_at_edges_lines",
        mode: DiffMode::Lines,
        old: include_str!("../examples/diffs/case06.01.before_insertions_edges.txt"),
        new: include_str!("../examples/diffs/case06.02.after_insertions_edges.txt"),
    },
];

fn fixture_diff(fixture: Fixture, algorithm: Algorithm) -> usize {
    let mut config = TextDiff::configure();
    config.algorithm(algorithm);
    let diff = match fixture.mode {
        DiffMode::Lines => config.diff_lines(fixture.old, fixture.new),
        DiffMode::Words => config.diff_words(fixture.old, fixture.new),
    };
    black_box(diff.ops().len())
}

fn sparse_unique(size: usize) -> (Vec<u32>, Vec<u32>) {
    let old = (0..size as u32).collect::<Vec<_>>();
    let mut new = old.clone();
    // Keep edits away from the boundaries so prefix/suffix trimming is measured.
    new[size / 5] = u32::MAX;
    new[size / 2] = u32::MAX - 1;
    new[size * 4 / 5] = u32::MAX - 2;
    (old, new)
}

fn repeated_shift(size: usize) -> (Vec<u32>, Vec<u32>) {
    let old = (0..size).map(|index| (index & 1) as u32).collect();
    let new = (0..size).map(|index| ((index + 1) & 1) as u32).collect();
    (old, new)
}

fn disjoint(size: usize) -> (Vec<u32>, Vec<u32>) {
    (
        (0..size as u32).collect(),
        (0..size as u32).map(|value| value + size as u32).collect(),
    )
}

fn large_sparse_text() -> &'static (String, String) {
    static DATA: OnceLock<(String, String)> = OnceLock::new();
    DATA.get_or_init(|| {
        let mut old = String::with_capacity(1_000_000);
        let mut new = String::with_capacity(1_000_000);
        writeln!(new, "// inserted header").unwrap();

        for index in 0..20_000 {
            writeln!(old, "record {index:06}: alpha beta gamma delta").unwrap();
            if index % 5_000 == 2_500 {
                writeln!(new, "record {index:06}: alpha beta gamma EDITED").unwrap();
            } else {
                writeln!(new, "record {index:06}: alpha beta gamma delta").unwrap();
            }
        }
        writeln!(new, "// inserted footer").unwrap();
        (old, new)
    })
}

fn late_edit_text() -> &'static (String, String) {
    static DATA: OnceLock<(String, String)> = OnceLock::new();
    DATA.get_or_init(|| {
        let mut old = String::with_capacity(1_000_000);
        let mut new = String::with_capacity(1_000_000);
        for index in 0..20_000 {
            writeln!(old, "record {index:06}: unchanged payload").unwrap();
            if index == 19_999 {
                writeln!(new, "record {index:06}: edited payload").unwrap();
            } else {
                writeln!(new, "record {index:06}: unchanged payload").unwrap();
            }
        }
        (old, new)
    })
}

fn bench_fixtures(c: &mut Criterion) {
    let mut group = c.benchmark_group("text_fixtures");
    group.sample_size(30);
    group.measurement_time(Duration::from_secs(1));

    for fixture in FIXTURES {
        group.throughput(Throughput::Bytes(
            (fixture.old.len() + fixture.new.len()) as u64,
        ));
        for &algorithm in ALL_ALGORITHMS {
            group.bench_with_input(
                BenchmarkId::new(fixture.name, format!("{algorithm:?}")),
                &(fixture, algorithm),
                |b, &(fixture, algorithm)| {
                    b.iter(|| fixture_diff(black_box(*fixture), algorithm));
                },
            );
        }
    }
    group.finish();
}

fn bench_algorithm_matrix(c: &mut Criterion) {
    let identical = (0..20_000u32).collect::<Vec<_>>();
    let sparse = sparse_unique(20_000);
    let disjoint = disjoint(20_000);
    let repeated = repeated_shift(2_000);

    let mut group = c.benchmark_group("algorithm_core");
    group.sample_size(20);
    group.measurement_time(Duration::from_secs(2));

    group.throughput(Throughput::Elements((identical.len() * 2) as u64));
    for &algorithm in ALL_ALGORITHMS {
        group.bench_with_input(
            BenchmarkId::new("identical", format!("{algorithm:?}/n={}", identical.len())),
            &algorithm,
            |b, &algorithm| {
                b.iter(|| {
                    black_box(capture_diff_slices(
                        algorithm,
                        black_box(&identical),
                        black_box(&identical),
                    ))
                });
            },
        );
    }

    for (name, (old, new)) in [("sparse_unique", &sparse), ("disjoint", &disjoint)] {
        group.throughput(Throughput::Elements((old.len() + new.len()) as u64));
        for &algorithm in SCALABLE_ALGORITHMS {
            group.bench_with_input(
                BenchmarkId::new(name, format!("{algorithm:?}/n={}", old.len())),
                &algorithm,
                |b, &algorithm| {
                    b.iter(|| {
                        black_box(capture_diff_slices(
                            algorithm,
                            black_box(old),
                            black_box(new),
                        ))
                    });
                },
            );
        }
    }

    group.throughput(Throughput::Elements(
        (repeated.0.len() + repeated.1.len()) as u64,
    ));
    for &algorithm in SCALABLE_ALGORITHMS {
        group.bench_with_input(
            BenchmarkId::new(
                "repeated_shift",
                format!("{algorithm:?}/n={}", repeated.0.len()),
            ),
            &algorithm,
            |b, &algorithm| {
                b.iter(|| {
                    black_box(capture_diff_slices(
                        algorithm,
                        black_box(&repeated.0),
                        black_box(&repeated.1),
                    ))
                });
            },
        );
    }
    group.finish();
}

fn bench_dense_scaling(c: &mut Criterion) {
    let mut group = c.benchmark_group("dense_repeated_scaling");
    group.sample_size(15);
    group.measurement_time(Duration::from_secs(2));

    for size in [128, 256, 512, 1024] {
        let (old, new) = repeated_shift(size);
        group.throughput(Throughput::Elements((size * 2) as u64));
        for &algorithm in ALL_ALGORITHMS {
            group.bench_with_input(
                BenchmarkId::new(format!("n={size}"), format!("{algorithm:?}")),
                &algorithm,
                |b, &algorithm| {
                    b.iter(|| {
                        black_box(capture_diff_slices(
                            algorithm,
                            black_box(&old),
                            black_box(&new),
                        ))
                    });
                },
            );
        }
    }
    group.finish();
}

fn bench_text_end_to_end(c: &mut Criterion) {
    let sparse = large_sparse_text();
    let late_edit = late_edit_text();
    let mut group = c.benchmark_group("text_end_to_end");
    group.sample_size(15);
    group.measurement_time(Duration::from_secs(2));

    for (name, (old, new)) in [
        ("sparse_20k_lines", sparse),
        ("late_edit_20k_lines", late_edit),
    ] {
        group.throughput(Throughput::Bytes((old.len() + new.len()) as u64));
        for &algorithm in SCALABLE_ALGORITHMS {
            group.bench_with_input(
                BenchmarkId::new(name, format!("{algorithm:?}")),
                &algorithm,
                |b, &algorithm| {
                    b.iter(|| {
                        let mut config = TextDiff::configure();
                        config.algorithm(algorithm);
                        black_box(config.diff_lines(black_box(old), black_box(new)))
                    });
                },
            );
        }
    }
    group.finish();
}

criterion_group!(
    benches,
    bench_fixtures,
    bench_algorithm_matrix,
    bench_dense_scaling,
    bench_text_end_to_end
);
criterion_main!(benches);
