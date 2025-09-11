use likewise::{capture_diff_slices_fp, capture_diff_slices_fp_f64, Algorithm, ChangeTag};

fn main() {
    // Example 1: Sensor readings with measurement tolerance
    println!("=== Sensor Reading Comparison ===");
    let baseline_readings = vec![23.1, 24.5, 22.8, 25.2, 26.0];
    let current_readings = vec![23.12, 24.48, 22.85, 25.18, 26.03];

    // Strict comparison would show many differences due to measurement noise
    // FP comparison with 0.05 tolerance treats small variations as equal
    let ops = capture_diff_slices_fp(
        Algorithm::Myers, 
        &baseline_readings, 
        &current_readings, 
        0.05
    );

    println!("Baseline: {:?}", baseline_readings);
    println!("Current:  {:?}", current_readings);
    println!("With epsilon=0.05: {} diff operations", ops.len());

    for op in &ops {
        let changes: Vec<_> = op.iter_changes(&baseline_readings, &current_readings).collect();
        for change in changes {
            match change.tag() {
                ChangeTag::Equal => println!("  ✓ Equal: {}", change.value()),
                ChangeTag::Delete => println!("  - Remove: {}", change.value()),
                ChangeTag::Insert => println!("  + Add: {}", change.value()),
            }
        }
    }

    // Example 2: Different epsilon values
    println!("\n=== Epsilon Sensitivity ===");
    let old = vec![1.0, 2.0, 3.0];
    let new = vec![1.001, 2.0, 2.999];

    println!("Old: {:?}", old);
    println!("New: {:?}", new);

    for &epsilon in &[0.0001, 0.001, 0.01] {
        let ops = capture_diff_slices_fp(Algorithm::Myers, &old, &new, epsilon);
        let all_equal = ops.len() == 1 && matches!(ops[0], likewise::DiffOp::Equal { len: 3, .. });
        println!("  epsilon={}: {} ({})", epsilon, ops.len(), 
                if all_equal { "all equal" } else { "differences found" });
    }

    // Example 3: Edge cases with NaN and infinity
    println!("\n=== Edge Cases ===");
    let old_edge = vec![0.0, -0.0, f32::NAN, f32::INFINITY, f32::NEG_INFINITY];
    let new_edge = vec![-0.0, 0.0, f32::NAN, f32::INFINITY, f32::NEG_INFINITY];

    let ops = capture_diff_slices_fp(Algorithm::Myers, &old_edge, &new_edge, 0.001);
    println!("Old: {:?}", old_edge);
    println!("New: {:?}", new_edge);
    println!("Edge case handling: {} operations", ops.len());

    // Example 4: f64 precision
    println!("\n=== High Precision f64 ===");
    let old_f64 = vec![1.0000000000001, 2.0, 3.141592653589793];
    let new_f64 = vec![1.0000000000002, 2.0, 3.141592653589794];

    let ops_f64 = capture_diff_slices_fp_f64(Algorithm::Myers, &old_f64, &new_f64, 1e-12);
    println!("f64 comparison with epsilon=1e-12: {} operations", ops_f64.len());
}