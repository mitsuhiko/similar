//! Minimum cost bipartite assignment (Kuhn-Munkres / Hungarian method).
use alloc::vec;
use alloc::vec::Vec;

/// Solves the assignment problem for a rectangular cost matrix.
///
/// `cost` has `rows` rows of `cols` entries each and `rows <= cols`.
/// Returns for every row the assigned column such that the total cost is
/// minimal.  Runs in `O(rows^2 * cols)`.
pub(crate) fn solve(cost: &[f64], rows: usize, cols: usize) -> Vec<usize> {
    debug_assert!(rows <= cols);
    debug_assert_eq!(cost.len(), rows * cols);
    let inf = f64::INFINITY;
    // Potentials and the matching use 1-based indexes with 0 as a sentinel
    // column, following the usual formulation of the algorithm.
    let mut u = vec![0.0f64; rows + 1];
    let mut v = vec![0.0f64; cols + 1];
    let mut matched_row = vec![0usize; cols + 1];
    let mut way = vec![0usize; cols + 1];
    let mut min_v = vec![inf; cols + 1];
    let mut used = vec![false; cols + 1];

    for row in 1..=rows {
        matched_row[0] = row;
        let mut col0 = 0;
        min_v.fill(inf);
        used.fill(false);
        loop {
            used[col0] = true;
            let row0 = matched_row[col0];
            let mut delta = inf;
            let mut col1 = 0;
            for col in 1..=cols {
                if used[col] {
                    continue;
                }
                let current = cost[(row0 - 1) * cols + col - 1] - u[row0] - v[col];
                if current < min_v[col] {
                    min_v[col] = current;
                    way[col] = col0;
                }
                if min_v[col] < delta {
                    delta = min_v[col];
                    col1 = col;
                }
            }
            for col in 0..=cols {
                if used[col] {
                    u[matched_row[col]] += delta;
                    v[col] -= delta;
                } else {
                    min_v[col] -= delta;
                }
            }
            col0 = col1;
            if matched_row[col0] == 0 {
                break;
            }
        }
        loop {
            let col1 = way[col0];
            matched_row[col0] = matched_row[col1];
            col0 = col1;
            if col0 == 0 {
                break;
            }
        }
    }

    let mut rv = vec![0; rows];
    for col in 1..=cols {
        if matched_row[col] != 0 {
            rv[matched_row[col] - 1] = col - 1;
        }
    }
    rv
}

#[test]
fn test_solve() {
    let cost = [4.0, 1.0, 3.0, 2.0, 0.0, 5.0, 3.0, 2.0, 2.0];
    let assignment = solve(&cost, 3, 3);
    assert_eq!(assignment, vec![1, 0, 2]);

    let cost = [1.0, 10.0, 0.5, 10.0, 1.0, 10.0];
    assert_eq!(solve(&cost, 2, 3), vec![2, 1]);
}

#[test]
fn test_solve_matches_brute_force() {
    let mut seed = 99u64;
    let mut random = move || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        (seed % 1000) as f64 / 10.0
    };
    for size in 1..=5 {
        for _ in 0..50 {
            let cost = (0..size * size).map(|_| random()).collect::<Vec<_>>();
            let assignment = solve(&cost, size, size);
            let total = |a: &[usize]| {
                a.iter()
                    .enumerate()
                    .map(|(r, &c)| cost[r * size + c])
                    .sum::<f64>()
            };
            let mut best = f64::INFINITY;
            let mut perm = (0..size).collect::<Vec<_>>();
            permutations(&mut perm, 0, &mut |p| best = best.min(total(p)));
            assert!((total(&assignment) - best).abs() < 1e-9);
        }
    }

    fn permutations(items: &mut [usize], k: usize, f: &mut impl FnMut(&[usize])) {
        if k == items.len() {
            f(items);
            return;
        }
        for i in k..items.len() {
            items.swap(k, i);
            permutations(items, k + 1, f);
            items.swap(k, i);
        }
    }
}
