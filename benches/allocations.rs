use std::alloc::{GlobalAlloc, Layout, System};
use std::fmt::Write as _;
use std::hint::black_box;
use std::sync::atomic::{AtomicUsize, Ordering};

use similar::{Algorithm, TextDiff, capture_diff_slices};

struct TrackingAllocator;

static CURRENT_BYTES: AtomicUsize = AtomicUsize::new(0);
static PEAK_BYTES: AtomicUsize = AtomicUsize::new(0);
static ALLOCATION_CALLS: AtomicUsize = AtomicUsize::new(0);
static ALLOCATED_BYTES: AtomicUsize = AtomicUsize::new(0);

#[global_allocator]
static ALLOCATOR: TrackingAllocator = TrackingAllocator;

fn record_alloc(size: usize) {
    ALLOCATION_CALLS.fetch_add(1, Ordering::Relaxed);
    ALLOCATED_BYTES.fetch_add(size, Ordering::Relaxed);
    let current = CURRENT_BYTES.fetch_add(size, Ordering::Relaxed) + size;
    let mut peak = PEAK_BYTES.load(Ordering::Relaxed);
    while current > peak {
        match PEAK_BYTES.compare_exchange_weak(peak, current, Ordering::Relaxed, Ordering::Relaxed)
        {
            Ok(_) => break,
            Err(actual) => peak = actual,
        }
    }
}

fn record_dealloc(size: usize) {
    CURRENT_BYTES.fetch_sub(size, Ordering::Relaxed);
}

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            record_alloc(layout.size());
        }
        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let ptr = unsafe { System.alloc_zeroed(layout) };
        if !ptr.is_null() {
            record_alloc(layout.size());
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        record_dealloc(layout.size());
        unsafe { System.dealloc(ptr, layout) };
    }

    unsafe fn realloc(&self, ptr: *mut u8, old: Layout, new_size: usize) -> *mut u8 {
        let new_ptr = unsafe { System.realloc(ptr, old, new_size) };
        if !new_ptr.is_null() {
            record_dealloc(old.size());
            record_alloc(new_size);
        }
        new_ptr
    }
}

#[derive(Debug)]
struct AllocationStats {
    calls: usize,
    allocated_bytes: usize,
    peak_live_bytes: usize,
    retained_bytes: usize,
}

fn measure<T>(f: impl FnOnce() -> T) -> AllocationStats {
    let start_current = CURRENT_BYTES.load(Ordering::Relaxed);
    let start_calls = ALLOCATION_CALLS.load(Ordering::Relaxed);
    let start_allocated = ALLOCATED_BYTES.load(Ordering::Relaxed);
    PEAK_BYTES.store(start_current, Ordering::Relaxed);

    let value = f();
    black_box(&value);

    let end_current = CURRENT_BYTES.load(Ordering::Relaxed);
    let stats = AllocationStats {
        calls: ALLOCATION_CALLS.load(Ordering::Relaxed) - start_calls,
        allocated_bytes: ALLOCATED_BYTES.load(Ordering::Relaxed) - start_allocated,
        peak_live_bytes: PEAK_BYTES.load(Ordering::Relaxed) - start_current,
        retained_bytes: end_current - start_current,
    };
    drop(value);
    stats
}

fn sparse_unique(size: usize) -> (Vec<u32>, Vec<u32>) {
    let old = (0..size as u32).collect::<Vec<_>>();
    let mut new = old.clone();
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

fn sparse_text(size: usize) -> (String, String) {
    let mut old = String::with_capacity(size * 48);
    let mut new = String::with_capacity(size * 48);
    writeln!(new, "// inserted header").unwrap();
    for index in 0..size {
        writeln!(old, "record {index:06}: alpha beta gamma delta").unwrap();
        if index == size / 2 {
            writeln!(new, "record {index:06}: alpha beta gamma EDITED").unwrap();
        } else {
            writeln!(new, "record {index:06}: alpha beta gamma delta").unwrap();
        }
    }
    writeln!(new, "// inserted footer").unwrap();
    (old, new)
}

fn print_stats(case: &str, algorithm: Algorithm, stats: AllocationStats) {
    println!(
        "{case:<24} {algorithm:<10?} {:>10} {:>14} {:>14} {:>14}",
        stats.calls, stats.allocated_bytes, stats.peak_live_bytes, stats.retained_bytes,
    );
}

fn run_slice_case(case: &str, algorithms: &[Algorithm], old: &[u32], new: &[u32]) {
    for &algorithm in algorithms {
        print_stats(
            case,
            algorithm,
            measure(|| capture_diff_slices(algorithm, black_box(old), black_box(new))),
        );
    }
}

fn main() {
    const ALL: &[Algorithm] = &[
        Algorithm::Myers,
        Algorithm::Patience,
        Algorithm::Lcs,
        Algorithm::Hunt,
        Algorithm::Histogram,
    ];
    const SCALABLE: &[Algorithm] = &[
        Algorithm::Myers,
        Algorithm::Patience,
        Algorithm::Hunt,
        Algorithm::Histogram,
    ];

    println!("requested allocation sizes; allocator bookkeeping overhead is not included");
    println!(
        "{:<24} {:<10} {:>10} {:>14} {:>14} {:>14}",
        "case", "algorithm", "allocs", "allocated", "peak live", "retained"
    );

    let identical = (0..20_000u32).collect::<Vec<_>>();
    run_slice_case("identical_20k", ALL, &identical, &identical);

    let sparse = sparse_unique(20_000);
    run_slice_case("sparse_unique_20k", SCALABLE, &sparse.0, &sparse.1);

    let disjoint = disjoint(20_000);
    run_slice_case("disjoint_20k", ALL, &disjoint.0, &disjoint.1);

    for size in [128, 256, 512, 1024] {
        let repeated = repeated_shift(size);
        run_slice_case(
            &format!("repeated_shift_{size}"),
            ALL,
            &repeated.0,
            &repeated.1,
        );
    }

    let (old_text, new_text) = sparse_text(20_000);
    for &algorithm in SCALABLE {
        print_stats(
            "text_sparse_20k",
            algorithm,
            measure(|| {
                let mut config = TextDiff::configure();
                config.algorithm(algorithm);
                config.diff_lines(black_box(&old_text), black_box(&new_text))
            }),
        );
    }
}
