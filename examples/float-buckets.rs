//! Demonstrates tolerant float diffing by projecting `f64` values into explicit
//! comparison buckets with [`similar::algorithms::CachedLookup`].
//!
//! This is a good fit when your domain can express approximate equality via a
//! stable normalization step (for example rounding sensor data to a tolerance
//! bucket) before diffing.
//!
//! Note that bucketing is not the same as pairwise `abs(a - b) <= epsilon`
//! comparison: it gives you a transitive, hashable key that works with the
//! regular diff APIs.

use similar::algorithms::CachedLookup;
use similar::{Algorithm, ChangeTag, DiffOp, capture_diff};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum FloatBucket {
    NegInf,
    Finite(i64),
    PosInf,
    Nan,
}

fn bucket_f64(value: f64, epsilon: f64) -> FloatBucket {
    assert!(epsilon.is_finite() && epsilon > 0.0);
    match value {
        v if v.is_nan() => FloatBucket::Nan,
        v if v == f64::INFINITY => FloatBucket::PosInf,
        v if v == f64::NEG_INFINITY => FloatBucket::NegInf,
        v => FloatBucket::Finite((v / epsilon).round() as i64),
    }
}

fn diff_with_buckets(old: &[f64], new: &[f64], epsilon: f64) -> Vec<DiffOp> {
    let old_keys = CachedLookup::new(old.len(), |idx| bucket_f64(old[idx], epsilon));
    let new_keys = CachedLookup::new(new.len(), |idx| bucket_f64(new[idx], epsilon));
    capture_diff(
        Algorithm::Myers,
        &old_keys,
        0..old_keys.len(),
        &new_keys,
        0..new_keys.len(),
    )
}

fn main() {
    println!("=== Sensor Reading Comparison ===");
    let baseline_readings = vec![23.1_f64, 24.5, 22.8, 25.2, 26.0];
    let current_readings = vec![23.12_f64, 24.48, 22.85, 25.18, 26.03];
    let ops = diff_with_buckets(&baseline_readings, &current_readings, 0.05);

    println!("Baseline: {:?}", baseline_readings);
    println!("Current:  {:?}", current_readings);
    println!("With bucket size 0.05: {} diff operations", ops.len());

    for op in &ops {
        for change in op.iter_changes(&baseline_readings, &current_readings) {
            match change.tag() {
                ChangeTag::Equal => println!("  ✓ Equal bucket: {}", change.value()),
                ChangeTag::Delete => println!("  - Remove: {}", change.value()),
                ChangeTag::Insert => println!("  + Add: {}", change.value()),
            }
        }
    }

    println!("\n=== Bucket Sensitivity ===");
    let old = vec![1.0_f64, 2.0, 3.0];
    let new = vec![1.001_f64, 2.0, 2.999];
    for &epsilon in &[0.0001_f64, 0.001, 0.01] {
        let ops = diff_with_buckets(&old, &new, epsilon);
        let all_equal = ops.len() == 1 && matches!(ops[0], DiffOp::Equal { len: 3, .. });
        println!(
            "  bucket size={epsilon}: {} ({})",
            ops.len(),
            if all_equal {
                "all equal"
            } else {
                "differences found"
            }
        );
    }

    println!("\n=== Edge Cases ===");
    let old_edge = vec![0.0_f64, -0.0, f64::NAN, f64::INFINITY, f64::NEG_INFINITY];
    let new_edge = vec![-0.0_f64, 0.0, f64::NAN, f64::INFINITY, f64::NEG_INFINITY];
    let ops = diff_with_buckets(&old_edge, &new_edge, 0.001);
    println!("Old: {:?}", old_edge);
    println!("New: {:?}", new_edge);
    println!("Edge case handling: {} operations", ops.len());

    println!("\n=== High Precision ===");
    let old_precise = vec![1.0000000000001_f64, 2.0, 3.141592653589793];
    let new_precise = vec![1.0000000000002_f64, 2.0, 3.141592653589794];
    let ops_precise = diff_with_buckets(&old_precise, &new_precise, 1e-12);
    println!(
        "f64 comparison with bucket size 1e-12: {} operations",
        ops_precise.len()
    );
}
