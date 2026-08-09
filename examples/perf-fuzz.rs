//! Generative, subprocess-isolated performance fuzzer for line diffs.
//!
//! See `fuzz/README.md` for usage and the finding format.

use std::env;
use std::fmt::Write as _;
use std::fs::{self, File};
use std::io::{self, Read as _, Write as _};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Output, Stdio};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use similar::{Algorithm, TextDiff};

const HARD_MAX_FILE_BYTES: usize = 10_000_000;
const DEFAULT_CASES: usize = 12;
const DEFAULT_TIMEOUT_MS: u64 = 2_000;
const DEFAULT_MIN_CASE_BYTES: usize = 64 * 1024;
const SETUP_TIMEOUT: Duration = Duration::from_secs(30);
const POLL_INTERVAL: Duration = Duration::from_millis(1);
const LINE_SUFFIX: &str = " token payload for similar performance fuzzing\n";

const ALL_ALGORITHMS: &[Algorithm] = &[
    Algorithm::Myers,
    Algorithm::Patience,
    Algorithm::Lcs,
    Algorithm::Hunt,
    Algorithm::Histogram,
];

#[derive(Debug)]
struct CliError(String);

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for CliError {}

impl From<io::Error> for CliError {
    fn from(error: io::Error) -> Self {
        Self(error.to_string())
    }
}

type Result<T> = std::result::Result<T, CliError>;

#[derive(Clone)]
struct RunConfig {
    cases: usize,
    seed: u64,
    timeout: Duration,
    max_bytes: usize,
    output: Option<PathBuf>,
    keep_all: bool,
    capture_stacks: bool,
    internal_deadline: bool,
}

struct ReplayConfig {
    old: PathBuf,
    new: PathBuf,
    algorithms: Vec<Algorithm>,
    timeout: Duration,
    capture_stacks: bool,
    internal_deadline: bool,
}

struct GeneratedCase {
    index: usize,
    seed: u64,
    strategy: &'static str,
    recipe: String,
    before: String,
    after: String,
    before_lines: usize,
    after_lines: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutcomeKind {
    Completed,
    TimedOut,
    Crashed,
    SetupTimedOut,
}

struct Outcome {
    algorithm: Algorithm,
    kind: OutcomeKind,
    elapsed: Duration,
    worker_elapsed: Option<Duration>,
    ops: Option<usize>,
    stdout: String,
    stderr: String,
    stack_path: Option<PathBuf>,
    stack_dumper: Option<String>,
    exit_status: Option<String>,
}

impl Outcome {
    fn is_interesting(&self) -> bool {
        self.kind != OutcomeKind::Completed
    }
}

#[derive(Clone, Copy)]
struct Rng(u64);

impl Rng {
    fn new(seed: u64) -> Self {
        Self(splitmix64(seed))
    }

    fn next_u64(&mut self) -> u64 {
        // xorshift64*; recipes record the seed, so this deliberately small RNG
        // is sufficient and keeps the fuzzer dependency-free.
        let mut value = self.0;
        value ^= value >> 12;
        value ^= value << 25;
        value ^= value >> 27;
        self.0 = value;
        value.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    fn usize(&mut self, start: usize, end: usize) -> usize {
        debug_assert!(start < end);
        start + (self.next_u64() as usize % (end - start))
    }

    fn shuffle<T>(&mut self, values: &mut [T]) {
        for index in (1..values.len()).rev() {
            values.swap(index, self.usize(0, index + 1));
        }
    }
}

fn splitmix64(mut value: u64) -> u64 {
    value = value.wrapping_add(0x9e37_79b9_7f4a_7c15);
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    value ^ (value >> 31)
}

fn main() {
    if let Err(error) = real_main() {
        eprintln!("error: {error}");
        std::process::exit(2);
    }
}

fn real_main() -> Result<()> {
    let mut args = env::args().skip(1).collect::<Vec<_>>();
    let command = if args.first().is_some_and(|arg| !arg.starts_with('-')) {
        args.remove(0)
    } else {
        "run".to_string()
    };

    match command.as_str() {
        "run" => run_campaign(parse_run_config(&args)?),
        "replay" => replay(parse_replay_config(&args)?),
        "__worker" => worker(&args),
        "help" => {
            print_usage();
            Ok(())
        }
        _ => Err(CliError(format!(
            "unknown command {command:?}; run with --help for usage"
        ))),
    }
}

fn print_usage() {
    println!(
        "\
Generative performance fuzzer for similar

USAGE:
  cargo run --release --example perf-fuzz -- [run] [OPTIONS]
  cargo run --release --example perf-fuzz -- replay --old FILE --new FILE [OPTIONS]

RUN OPTIONS:
  --cases N                 generated pairs (default: {DEFAULT_CASES})
  --seed N                  deterministic u64 seed (default: current time)
  --timeout-ms N            stack/kill threshold per algorithm (default: {DEFAULT_TIMEOUT_MS})
  --max-bytes N             maximum bytes in either file (default and hard max: {HARD_MAX_FILE_BYTES})
  --out DIR                 exact run output directory
  --keep-all                retain non-problematic generated inputs too
  --no-stacks               do not invoke sample/gdb/lldb/pstack on timeout
  --no-internal-deadline    do not give the worker a secondary safety deadline

REPLAY OPTIONS:
  --old FILE --new FILE     recorded input pair
  --algorithm NAME          Myers, Patience, Lcs, Hunt, Histogram, or all (default: all)
  --timeout-ms N            stack/kill threshold (default: {DEFAULT_TIMEOUT_MS})
  --no-stacks
  --no-internal-deadline
"
    );
}

fn parse_run_config(args: &[String]) -> Result<RunConfig> {
    if args.iter().any(|arg| arg == "--help" || arg == "-h") {
        print_usage();
        std::process::exit(0);
    }

    let mut config = RunConfig {
        cases: DEFAULT_CASES,
        seed: SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as u64,
        timeout: Duration::from_millis(DEFAULT_TIMEOUT_MS),
        max_bytes: HARD_MAX_FILE_BYTES,
        output: None,
        keep_all: false,
        capture_stacks: true,
        internal_deadline: true,
    };

    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--cases" => config.cases = parse_usize(value_after(args, &mut index)?, "--cases")?,
            "--seed" => config.seed = parse_u64(value_after(args, &mut index)?, "--seed")?,
            "--timeout-ms" => {
                config.timeout = Duration::from_millis(parse_u64(
                    value_after(args, &mut index)?,
                    "--timeout-ms",
                )?)
            }
            "--max-bytes" => {
                config.max_bytes = parse_usize(value_after(args, &mut index)?, "--max-bytes")?
            }
            "--out" => config.output = Some(PathBuf::from(value_after(args, &mut index)?)),
            "--keep-all" => config.keep_all = true,
            "--no-stacks" => config.capture_stacks = false,
            "--no-internal-deadline" => config.internal_deadline = false,
            option => return Err(CliError(format!("unknown run option {option:?}"))),
        }
        index += 1;
    }

    if config.cases == 0 {
        return Err(CliError("--cases must be greater than zero".into()));
    }
    validate_timeout(config.timeout)?;
    validate_max_bytes(config.max_bytes)?;
    Ok(config)
}

fn parse_replay_config(args: &[String]) -> Result<ReplayConfig> {
    if args.iter().any(|arg| arg == "--help" || arg == "-h") {
        print_usage();
        std::process::exit(0);
    }

    let mut old = None;
    let mut new = None;
    let mut algorithms = ALL_ALGORITHMS.to_vec();
    let mut timeout = Duration::from_millis(DEFAULT_TIMEOUT_MS);
    let mut capture_stacks = true;
    let mut internal_deadline = true;
    let mut index = 0;

    while index < args.len() {
        match args[index].as_str() {
            "--old" => old = Some(PathBuf::from(value_after(args, &mut index)?)),
            "--new" => new = Some(PathBuf::from(value_after(args, &mut index)?)),
            "--algorithm" => {
                let value = value_after(args, &mut index)?;
                algorithms = if value.eq_ignore_ascii_case("all") {
                    ALL_ALGORITHMS.to_vec()
                } else {
                    vec![parse_algorithm(value)?]
                };
            }
            "--timeout-ms" => {
                timeout = Duration::from_millis(parse_u64(
                    value_after(args, &mut index)?,
                    "--timeout-ms",
                )?)
            }
            "--no-stacks" => capture_stacks = false,
            "--no-internal-deadline" => internal_deadline = false,
            option => return Err(CliError(format!("unknown replay option {option:?}"))),
        }
        index += 1;
    }

    validate_timeout(timeout)?;
    Ok(ReplayConfig {
        old: old.ok_or_else(|| CliError("replay requires --old FILE".into()))?,
        new: new.ok_or_else(|| CliError("replay requires --new FILE".into()))?,
        algorithms,
        timeout,
        capture_stacks,
        internal_deadline,
    })
}

fn value_after<'a>(args: &'a [String], index: &mut usize) -> Result<&'a str> {
    *index += 1;
    args.get(*index)
        .map(String::as_str)
        .ok_or_else(|| CliError("option requires a value".into()))
}

fn parse_usize(value: &str, option: &str) -> Result<usize> {
    value
        .parse()
        .map_err(|_| CliError(format!("invalid value {value:?} for {option}")))
}

fn parse_u64(value: &str, option: &str) -> Result<u64> {
    value
        .parse()
        .map_err(|_| CliError(format!("invalid value {value:?} for {option}")))
}

fn validate_timeout(timeout: Duration) -> Result<()> {
    if timeout.is_zero() {
        Err(CliError("--timeout-ms must be greater than zero".into()))
    } else {
        Ok(())
    }
}

fn validate_max_bytes(max_bytes: usize) -> Result<()> {
    if max_bytes < 1024 {
        return Err(CliError("--max-bytes must be at least 1024".into()));
    }
    if max_bytes > HARD_MAX_FILE_BYTES {
        return Err(CliError(format!(
            "--max-bytes cannot exceed the hard {HARD_MAX_FILE_BYTES}-byte limit"
        )));
    }
    Ok(())
}

fn parse_algorithm(value: &str) -> Result<Algorithm> {
    match value.to_ascii_lowercase().as_str() {
        "myers" => Ok(Algorithm::Myers),
        "patience" => Ok(Algorithm::Patience),
        "lcs" => Ok(Algorithm::Lcs),
        "hunt" => Ok(Algorithm::Hunt),
        "histogram" => Ok(Algorithm::Histogram),
        _ => Err(CliError(format!("unknown algorithm {value:?}"))),
    }
}

fn algorithm_name(algorithm: Algorithm) -> &'static str {
    match algorithm {
        Algorithm::Myers => "myers",
        Algorithm::Patience => "patience",
        Algorithm::Lcs => "lcs",
        Algorithm::Hunt => "hunt",
        Algorithm::Histogram => "histogram",
        // Algorithm is non-exhaustive. Keep this loud so a newly selected
        // variant cannot silently produce an unusable worker command.
        _ => panic!("performance fuzzer does not know this algorithm"),
    }
}

fn run_campaign(config: RunConfig) -> Result<()> {
    let executable = env::current_exe()?;
    let started = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    let run_name = format!("run-{started}-{}-{:016x}", std::process::id(), config.seed);
    let root = config.output.clone().unwrap_or_else(|| {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("fuzz/findings")
            .join(&run_name)
    });
    let work = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/perf-fuzz")
        .join(&run_name);
    fs::create_dir_all(&root)?;
    fs::create_dir_all(&work)?;

    let results_path = root.join("results.tsv");
    let mut results = File::create(&results_path)?;
    writeln!(
        results,
        "case\tstrategy\tbefore_bytes\tafter_bytes\talgorithm\tstatus\telapsed_ms\tworker_ms\tops"
    )?;

    println!("seed: {}", config.seed);
    println!("timeout: {} ms per algorithm", config.timeout.as_millis());
    println!("output: {}", root.display());
    println!("work: {}", work.display());

    let mut finding_names = Vec::new();
    let mut completed_runs = 0usize;
    let mut timed_out_runs = 0usize;
    let mut crashed_runs = 0usize;

    for case_index in 0..config.cases {
        let target_bytes = case_target_bytes(case_index, config.cases, config.max_bytes);
        let case = generate_case(config.seed, case_index, target_bytes, config.max_bytes);
        let case_name = format!(
            "case-{:04}-{}-{:016x}",
            case.index, case.strategy, case.seed
        );
        let before_path = work.join(format!("{case_name}.before.txt"));
        let after_path = work.join(format!("{case_name}.after.txt"));
        fs::write(&before_path, case.before.as_bytes())?;
        fs::write(&after_path, case.after.as_bytes())?;
        validate_input_file(&before_path)?;
        validate_input_file(&after_path)?;

        println!(
            "\n[{}/{}] {}: {} / {} bytes, {} / {} lines",
            case_index + 1,
            config.cases,
            case.strategy,
            case.before.len(),
            case.after.len(),
            case.before_lines,
            case.after_lines
        );

        let mut outcomes = Vec::new();
        for &algorithm in ALL_ALGORITHMS {
            let stack_path = work.join(format!(
                "{case_name}.{}.stack.txt",
                algorithm_name(algorithm)
            ));
            print!("  {:<10} ... ", algorithm_name(algorithm));
            io::stdout().flush()?;
            let outcome = run_worker(
                &executable,
                algorithm,
                &before_path,
                &after_path,
                config.timeout,
                config.internal_deadline,
                config.capture_stacks,
                &stack_path,
            )?;
            println!(
                "{} ({:.1} ms)",
                outcome_label(outcome.kind),
                duration_ms(outcome.elapsed)
            );

            match outcome.kind {
                OutcomeKind::Completed => completed_runs += 1,
                OutcomeKind::TimedOut | OutcomeKind::SetupTimedOut => timed_out_runs += 1,
                OutcomeKind::Crashed => crashed_runs += 1,
            }
            write_result_row(&mut results, &case_name, &case, algorithm, &outcome)?;
            results.flush()?;
            outcomes.push(outcome);
        }

        let interesting = outcomes.iter().any(Outcome::is_interesting);
        if interesting {
            let finding_dir = root.join(&case_name);
            persist_case(
                &finding_dir,
                &case,
                &before_path,
                &after_path,
                &outcomes,
                &config,
            )?;
            finding_names.push(case_name.clone());
            println!("  recorded {}", finding_dir.display());
        }

        if config.keep_all {
            let corpus_dir = root.join("corpus").join(&case_name);
            fs::create_dir_all(&corpus_dir)?;
            fs::copy(&before_path, corpus_dir.join("before.txt"))?;
            fs::copy(&after_path, corpus_dir.join("after.txt"))?;
            fs::write(corpus_dir.join("recipe.txt"), &case.recipe)?;
        }

        remove_if_exists(&before_path)?;
        remove_if_exists(&after_path)?;
        for outcome in &outcomes {
            if let Some(path) = &outcome.stack_path {
                remove_if_exists(path)?;
            }
        }
    }

    write_summary(
        &root,
        &config,
        &finding_names,
        completed_runs,
        timed_out_runs,
        crashed_runs,
    )?;
    let _ = fs::remove_dir(&work);

    println!(
        "\nfinished: {completed_runs} completed, {timed_out_runs} timed out, {crashed_runs} crashed"
    );
    println!("summary: {}", root.join("summary.md").display());
    Ok(())
}

fn replay(config: ReplayConfig) -> Result<()> {
    validate_input_file(&config.old)?;
    validate_input_file(&config.new)?;
    let executable = env::current_exe()?;
    let work = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/perf-fuzz/replay")
        .join(format!("{}", std::process::id()));
    fs::create_dir_all(&work)?;

    for algorithm in config.algorithms {
        let stack_path = work.join(format!("{}.stack.txt", algorithm_name(algorithm)));
        let outcome = run_worker(
            &executable,
            algorithm,
            &config.old,
            &config.new,
            config.timeout,
            config.internal_deadline,
            config.capture_stacks,
            &stack_path,
        )?;
        println!(
            "{:<10} {} ({:.1} ms)",
            algorithm_name(algorithm),
            outcome_label(outcome.kind),
            duration_ms(outcome.elapsed)
        );
        if let Some(path) = outcome.stack_path {
            println!("  stack: {}", path.display());
            if let Some(dumper) = outcome.stack_dumper {
                println!("  dumper: {dumper}");
            }
        }
        if !outcome.stderr.trim().is_empty() {
            eprintln!("  stderr: {}", outcome.stderr.trim());
        }
    }
    Ok(())
}

fn worker(args: &[String]) -> Result<()> {
    if args.len() != 6 {
        return Err(CliError(
            "internal worker expects ALGORITHM OLD NEW READY START INTERNAL_DEADLINE_MS".into(),
        ));
    }
    let algorithm = parse_algorithm(&args[0])?;
    let old_path = Path::new(&args[1]);
    let new_path = Path::new(&args[2]);
    let ready_path = Path::new(&args[3]);
    let start_path = Path::new(&args[4]);
    let internal_deadline_ms = parse_u64(&args[5], "internal deadline")?;

    permit_stack_dumper_attach();
    validate_input_file(old_path)?;
    validate_input_file(new_path)?;
    let old = fs::read_to_string(old_path).map_err(|error| {
        CliError(format!(
            "could not read {} as UTF-8: {error}",
            old_path.display()
        ))
    })?;
    let new = fs::read_to_string(new_path).map_err(|error| {
        CliError(format!(
            "could not read {} as UTF-8: {error}",
            new_path.display()
        ))
    })?;

    // The worker waits for a release handshake so the controller's timeout
    // starts at the same point as TextDiff tokenization plus diffing.
    fs::write(ready_path, b"ready\n")?;
    let waiting_since = Instant::now();
    while !start_path.exists() {
        if waiting_since.elapsed() >= SETUP_TIMEOUT {
            return Err(CliError("controller did not release ready worker".into()));
        }
        thread::sleep(POLL_INTERVAL);
    }

    let started = Instant::now();
    let mut diff_config = TextDiff::configure();
    diff_config.algorithm(algorithm);
    if internal_deadline_ms != 0 {
        diff_config.timeout(Duration::from_millis(internal_deadline_ms));
    }
    let diff = diff_config.diff_lines(old.as_str(), new.as_str());
    let elapsed = started.elapsed();
    println!(
        "ok\t{}\t{}\t{}",
        algorithm_name(algorithm),
        elapsed.as_micros(),
        diff.ops().len()
    );
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn run_worker(
    executable: &Path,
    algorithm: Algorithm,
    old: &Path,
    new: &Path,
    timeout: Duration,
    internal_deadline: bool,
    capture_stacks: bool,
    stack_path: &Path,
) -> Result<Outcome> {
    let ready_path = stack_path.with_extension("ready");
    let start_path = stack_path.with_extension("start");
    remove_if_exists(&ready_path)?;
    remove_if_exists(&start_path)?;
    remove_if_exists(stack_path)?;
    let internal_ms = if internal_deadline {
        // The external controller should always get a chance to take a stack.
        // This later deadline is only a failsafe if controller cleanup fails.
        timeout
            .checked_mul(2)
            .unwrap_or(Duration::MAX)
            .max(timeout.saturating_add(Duration::from_secs(1)))
            .as_millis()
            .min(u64::MAX as u128) as u64
    } else {
        0
    };

    let mut child = Command::new(executable)
        .arg("__worker")
        .arg(algorithm_name(algorithm))
        .arg(old)
        .arg(new)
        .arg(&ready_path)
        .arg(&start_path)
        .arg(internal_ms.to_string())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|error| CliError(format!("could not spawn worker: {error}")))?;

    let setup_started = Instant::now();
    let started = loop {
        if ready_path.exists() {
            let started = Instant::now();
            fs::write(&start_path, b"start\n")?;
            break started;
        }
        if let Some(status) = child.try_wait()? {
            let output = collect_output(child)?;
            remove_if_exists(&ready_path)?;
            remove_if_exists(&start_path)?;
            return Ok(crashed_outcome(
                algorithm,
                setup_started.elapsed(),
                status,
                output,
            ));
        }
        if setup_started.elapsed() >= SETUP_TIMEOUT {
            let elapsed = setup_started.elapsed();
            let (stack_dumper, saved_stack) = if capture_stacks {
                let dumper = dump_stack(child.id(), stack_path)?;
                (Some(dumper), Some(stack_path.to_path_buf()))
            } else {
                (None, None)
            };
            let _ = child.kill();
            let output = collect_output(child)?;
            remove_if_exists(&ready_path)?;
            remove_if_exists(&start_path)?;
            return Ok(Outcome {
                algorithm,
                kind: OutcomeKind::SetupTimedOut,
                elapsed,
                worker_elapsed: None,
                ops: None,
                stdout: output_to_string(&output.stdout),
                stderr: output_to_string(&output.stderr),
                stack_path: saved_stack,
                stack_dumper,
                exit_status: Some(status_label(output.status)),
            });
        }
        thread::sleep(POLL_INTERVAL);
    };

    loop {
        if let Some(status) = child.try_wait()? {
            let elapsed = started.elapsed();
            let output = collect_output(child)?;
            remove_if_exists(&ready_path)?;
            remove_if_exists(&start_path)?;
            if status.success() {
                let stdout = output_to_string(&output.stdout);
                let (worker_elapsed, ops) = parse_worker_output(&stdout);
                return Ok(Outcome {
                    algorithm,
                    kind: OutcomeKind::Completed,
                    elapsed,
                    worker_elapsed,
                    ops,
                    stdout,
                    stderr: output_to_string(&output.stderr),
                    stack_path: None,
                    stack_dumper: None,
                    exit_status: Some(status_label(status)),
                });
            }
            return Ok(crashed_outcome(algorithm, elapsed, status, output));
        }

        if started.elapsed() >= timeout {
            let elapsed = started.elapsed();
            let (stack_dumper, saved_stack) = if capture_stacks {
                let dumper = dump_stack(child.id(), stack_path)?;
                (Some(dumper), Some(stack_path.to_path_buf()))
            } else {
                (None, None)
            };
            let _ = child.kill();
            let output = collect_output(child)?;
            remove_if_exists(&ready_path)?;
            remove_if_exists(&start_path)?;
            let stdout = output_to_string(&output.stdout);
            // A worker can finish while the debugger is sampling it. Preserve
            // its eventual timing and op count while still classifying the run
            // by the point at which the external threshold was crossed.
            let (worker_elapsed, ops) = parse_worker_output(&stdout);
            return Ok(Outcome {
                algorithm,
                kind: OutcomeKind::TimedOut,
                elapsed,
                worker_elapsed,
                ops,
                stdout,
                stderr: output_to_string(&output.stderr),
                stack_path: saved_stack,
                stack_dumper,
                exit_status: Some(status_label(output.status)),
            });
        }
        thread::sleep(POLL_INTERVAL);
    }
}

fn collect_output(child: Child) -> Result<Output> {
    child
        .wait_with_output()
        .map_err(|error| CliError(format!("could not collect worker output: {error}")))
}

fn crashed_outcome(
    algorithm: Algorithm,
    elapsed: Duration,
    status: ExitStatus,
    output: Output,
) -> Outcome {
    Outcome {
        algorithm,
        kind: OutcomeKind::Crashed,
        elapsed,
        worker_elapsed: None,
        ops: None,
        stdout: output_to_string(&output.stdout),
        stderr: output_to_string(&output.stderr),
        stack_path: None,
        stack_dumper: None,
        exit_status: Some(status_label(status)),
    }
}

fn output_to_string(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

fn status_label(status: ExitStatus) -> String {
    status.code().map_or_else(
        || "terminated by signal".into(),
        |code| format!("exit {code}"),
    )
}

fn parse_worker_output(output: &str) -> (Option<Duration>, Option<usize>) {
    let mut fields = output.trim().split('\t');
    if fields.next() != Some("ok") {
        return (None, None);
    }
    let _algorithm = fields.next();
    let elapsed = fields
        .next()
        .and_then(|value| value.parse::<u64>().ok())
        .map(Duration::from_micros);
    let ops = fields.next().and_then(|value| value.parse().ok());
    (elapsed, ops)
}

#[cfg(target_os = "linux")]
fn permit_stack_dumper_attach() {
    // With Yama ptrace_scope=1, the debugger launched by the controller is a
    // sibling rather than the worker's parent. Opt this short-lived worker into
    // debugger attachment so gdb/eu-stack can produce the promised stack.
    const PR_SET_PTRACER: i32 = 0x5961_6d61;
    const PR_SET_PTRACER_ANY: usize = usize::MAX;
    unsafe extern "C" {
        fn prctl(option: i32, ...) -> i32;
    }
    // SAFETY: PR_SET_PTRACER accepts one unsigned-long argument. Failure only
    // means that the later best-effort debugger attachment may not work.
    let _ = unsafe { prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0usize, 0usize, 0usize) };
}

#[cfg(not(target_os = "linux"))]
fn permit_stack_dumper_attach() {}

fn dump_stack(pid: u32, destination: &Path) -> Result<String> {
    let log_path = destination.with_extension("stack-command.log");
    remove_if_exists(destination)?;
    remove_if_exists(&log_path)?;
    let pid_string = pid.to_string();
    let mut failures = Vec::new();

    if cfg!(target_os = "macos") {
        let destination_string = destination.as_os_str().to_owned();
        let args = [
            std::ffi::OsString::from(pid_string.as_str()),
            std::ffi::OsString::from("1"),
            std::ffi::OsString::from("-mayDie"),
            std::ffi::OsString::from("-file"),
            destination_string,
        ];
        match run_dump_command("sample", &args, &log_path, Duration::from_secs(4)) {
            Ok(status) if status.success() && destination.exists() => {
                remove_if_exists(&log_path)?;
                return Ok("sample".into());
            }
            Ok(status) => failures.push(format!("sample: {}", status_label(status))),
            Err(error) => failures.push(format!("sample: {error}")),
        }
    }

    let attempts: Vec<(&str, Vec<std::ffi::OsString>)> = if cfg!(target_os = "linux") {
        vec![
            (
                "gdb",
                vec![
                    "--batch".into(),
                    "--quiet".into(),
                    "-ex".into(),
                    "set pagination off".into(),
                    "-ex".into(),
                    "thread apply all backtrace".into(),
                    "-p".into(),
                    pid_string.clone().into(),
                ],
            ),
            ("eu-stack", vec!["-p".into(), pid_string.clone().into()]),
            ("pstack", vec![pid_string.clone().into()]),
            (
                "lldb",
                vec![
                    "--batch".into(),
                    "--attach-pid".into(),
                    pid_string.clone().into(),
                    "-o".into(),
                    "thread backtrace all".into(),
                    "-o".into(),
                    "detach".into(),
                ],
            ),
        ]
    } else {
        vec![(
            "lldb",
            vec![
                "--batch".into(),
                "--attach-pid".into(),
                pid_string.into(),
                "-o".into(),
                "thread backtrace all".into(),
                "-o".into(),
                "detach".into(),
            ],
        )]
    };

    for (program, args) in attempts {
        match run_dump_command(program, &args, destination, Duration::from_secs(3)) {
            Ok(status) if status.success() && file_looks_like_stack(destination) => {
                remove_if_exists(&log_path)?;
                return Ok(program.into());
            }
            Ok(status) => failures.push(format!("{program}: {}", status_label(status))),
            Err(error) => failures.push(format!("{program}: {error}")),
        }
    }

    let mut explanation = String::from(
        "A timeout was observed, but no supported process stack dumper succeeded.\n\
         Install/permit `sample` on macOS or `gdb`, `eu-stack`, `pstack`, or `lldb` on Linux.\n\n",
    );
    for failure in &failures {
        writeln!(explanation, "- {failure}").unwrap();
    }
    if let Ok(mut log) = File::open(&log_path) {
        let mut contents = String::new();
        let _ = log.read_to_string(&mut contents);
        if !contents.trim().is_empty() {
            writeln!(explanation, "\nLast dumper output:\n{contents}").unwrap();
        }
    }
    fs::write(destination, explanation)?;
    remove_if_exists(&log_path)?;
    Ok("unavailable (see stack file)".into())
}

fn run_dump_command(
    program: &str,
    args: &[std::ffi::OsString],
    output_path: &Path,
    timeout: Duration,
) -> io::Result<ExitStatus> {
    let stdout = File::create(output_path)?;
    let stderr = stdout.try_clone()?;
    let mut child = Command::new(program)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::from(stdout))
        .stderr(Stdio::from(stderr))
        .spawn()?;
    let started = Instant::now();
    loop {
        if let Some(status) = child.try_wait()? {
            return Ok(status);
        }
        if started.elapsed() >= timeout {
            let _ = child.kill();
            let _ = child.wait();
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                format!("{program} exceeded its {timeout:?} timeout"),
            ));
        }
        thread::sleep(Duration::from_millis(10));
    }
}

fn file_looks_like_stack(path: &Path) -> bool {
    let Ok(contents) = fs::read_to_string(path) else {
        return false;
    };
    if contents.trim().is_empty() {
        return false;
    }
    let lower = contents.to_ascii_lowercase();
    !lower.contains("operation not permitted")
        && !lower.contains("attach failed")
        && !lower.contains("could not attach")
        && !lower.contains("no such process")
}

fn validate_input_file(path: &Path) -> Result<()> {
    let metadata = fs::metadata(path)
        .map_err(|error| CliError(format!("could not stat {}: {error}", path.display())))?;
    if metadata.len() > HARD_MAX_FILE_BYTES as u64 {
        return Err(CliError(format!(
            "{} is {} bytes; inputs are limited to {HARD_MAX_FILE_BYTES} bytes",
            path.display(),
            metadata.len()
        )));
    }
    Ok(())
}

fn case_target_bytes(index: usize, cases: usize, max_bytes: usize) -> usize {
    let minimum = DEFAULT_MIN_CASE_BYTES.min(max_bytes);
    if cases == 1 {
        return max_bytes;
    }
    minimum + (max_bytes - minimum) * index / (cases - 1)
}

fn generate_case(
    campaign_seed: u64,
    index: usize,
    target_bytes: usize,
    max_bytes: usize,
) -> GeneratedCase {
    let seed = splitmix64(campaign_seed ^ (index as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15));
    let mut rng = Rng::new(seed);
    let line_bytes = 8 + LINE_SUFFIX.len();
    let lines = (target_bytes / line_bytes).max(2);
    let base = (seed as u32).wrapping_mul(0x9e37_79b9);

    let (strategy, recipe, before_tokens, after_tokens) = match index % 8 {
        0 => {
            let alphabet = rng.usize(2, 33) as u32;
            let shift = rng.usize(1, alphabet as usize) as u32;
            let before = (0..lines)
                .map(|item| base.wrapping_add(item as u32 % alphabet))
                .collect::<Vec<_>>();
            let mut after = (0..lines)
                .map(|item| base.wrapping_add((item as u32 + shift) % alphabet))
                .collect::<Vec<_>>();
            let mutation_stride = rng.usize(257, 4097);
            for position in (mutation_stride / 2..after.len()).step_by(mutation_stride) {
                after[position] = base.wrapping_add(0x7000_0000 ^ position as u32);
            }
            (
                "dense-cycle",
                format!(
                    "low-cardinality periodic input; alphabet={alphabet}, shift={shift}, mutation_stride={mutation_stride}, target_bytes={target_bytes}"
                ),
                before,
                after,
            )
        }
        1 => {
            let before = (0..lines)
                .map(|item| base.wrapping_add(item as u32))
                .collect::<Vec<_>>();
            let mut after = (0..lines)
                .map(|item| {
                    base.wrapping_add(lines as u32)
                        .wrapping_add(item as u32 + 1)
                })
                .collect::<Vec<_>>();
            let anchors = rng.usize(1, 17).min(lines / 4).max(1);
            for anchor in 0..anchors {
                let old_position = (anchor + 1) * lines / (anchors + 1);
                let new_position = (anchors - anchor) * lines / (anchors + 1);
                after[new_position] = before[old_position];
            }
            (
                "sparse-crossed",
                format!(
                    "otherwise disjoint unique ranges with {anchors} common anchors in crossing order; target_bytes={target_bytes}"
                ),
                before,
                after,
            )
        }
        2 => {
            let block = rng.usize(8, 513).min(lines / 2).max(1);
            let before = (0..lines)
                .map(|item| base.wrapping_add(item as u32))
                .collect::<Vec<_>>();
            let blocks = lines.div_ceil(block);
            let rotation = rng.usize(1, blocks.max(2)).min(blocks - 1);
            let mut after = Vec::with_capacity(lines);
            for output_block in 0..blocks {
                let source_block = (output_block + rotation) % blocks;
                let start = source_block * block;
                let end = (start + block).min(lines);
                after.extend_from_slice(&before[start..end]);
            }
            after.truncate(lines);
            while after.len() < lines {
                after.push(base.wrapping_add(after.len() as u32));
            }
            (
                "block-rotation",
                format!(
                    "unique lines rotated in blocks; block_lines={block}, blocks={blocks}, rotation={rotation}, target_bytes={target_bytes}"
                ),
                before,
                after,
            )
        }
        3 => {
            let chunk = rng.usize(16, 1025).min(lines / 2).max(1);
            let before = (0..lines)
                .map(|item| base.wrapping_add((item / 3) as u32))
                .collect::<Vec<_>>();
            let chunks = lines.div_ceil(chunk);
            let mut order = (0..chunks).collect::<Vec<_>>();
            rng.shuffle(&mut order);
            let mut after = Vec::with_capacity(lines);
            for source_chunk in order {
                let start = source_chunk * chunk;
                let end = (start + chunk).min(lines);
                after.extend_from_slice(&before[start..end]);
            }
            (
                "chunk-shuffle",
                format!(
                    "duplicated adjacent lines shuffled in chunks; chunk_lines={chunk}, chunks={chunks}, target_bytes={target_bytes}"
                ),
                before,
                after,
            )
        }
        4 => {
            let alphabet = rng.usize(2, 9) as u32;
            let band = rng.usize(8, 257);
            let before = (0..lines)
                .map(|item| base.wrapping_add(item as u32 % alphabet))
                .collect::<Vec<_>>();
            let after = (0..lines)
                .map(|item| {
                    let phase = if (item / band) & 1 == 0 {
                        1
                    } else {
                        alphabet - 1
                    };
                    base.wrapping_add((item as u32 + phase) % alphabet)
                })
                .collect::<Vec<_>>();
            (
                "alternating-bands",
                format!(
                    "dense repeated values with alternating phase shifts; alphabet={alphabet}, band_lines={band}, target_bytes={target_bytes}"
                ),
                before,
                after,
            )
        }
        5 => {
            let short_lines = (lines / rng.usize(8, 33)).max(2);
            let before = (0..short_lines)
                .map(|item| base.wrapping_add(item as u32))
                .collect::<Vec<_>>();
            let mut after = (0..lines)
                .map(|item| base.wrapping_add(0x4000_0000).wrapping_add(item as u32))
                .collect::<Vec<_>>();
            let overlaps = rng.usize(1, 9).min(short_lines);
            for overlap in 0..overlaps {
                let old_position = overlap * short_lines / overlaps;
                let new_position = (overlap + 1) * lines / (overlaps + 1);
                after[new_position] = before[old_position];
            }
            (
                "unbalanced-overlap",
                format!(
                    "short unique input against a long mostly-disjoint input; short_lines={short_lines}, long_lines={lines}, overlaps={overlaps}"
                ),
                before,
                after,
            )
        }
        6 => {
            let run = rng.usize(4, 129);
            let separator = rng.usize(3, 33);
            let before = (0..lines)
                .map(|item| {
                    if item % (run * separator) == 0 {
                        base.wrapping_add(0x2000_0000)
                            .wrapping_add((item / run) as u32)
                    } else {
                        base.wrapping_add((item / run % 3) as u32)
                    }
                })
                .collect::<Vec<_>>();
            let after = (0..lines)
                .map(|item| {
                    let shifted = item.wrapping_add(run / 2 + 1);
                    if shifted % (run * separator) == 0 {
                        base.wrapping_add(0x2000_0000)
                            .wrapping_add((shifted / run) as u32)
                    } else {
                        base.wrapping_add((shifted / run % 3) as u32)
                    }
                })
                .collect::<Vec<_>>();
            (
                "run-aliases",
                format!(
                    "long repeated runs with sparse pseudo-unique separators; run_lines={run}, separator_period={separator}, target_bytes={target_bytes}"
                ),
                before,
                after,
            )
        }
        _ => {
            let period = rng.usize(17, 1025).min(lines).max(2);
            let edit_width = rng.usize(1, period).min(64);
            let before = (0..lines)
                .map(|item| base.wrapping_add((item % period) as u32))
                .collect::<Vec<_>>();
            let after = (0..lines)
                .map(|item| {
                    if item % period < edit_width {
                        base.wrapping_add(((item + period / 2) % period) as u32)
                    } else {
                        base.wrapping_add((item % period) as u32)
                    }
                })
                .collect::<Vec<_>>();
            (
                "periodic-windows",
                format!(
                    "repeated large period with a changed window each cycle; period={period}, edit_width={edit_width}, target_bytes={target_bytes}"
                ),
                before,
                after,
            )
        }
    };

    let before_lines = before_tokens.len();
    let after_lines = after_tokens.len();
    let before = render_tokens(&before_tokens, max_bytes);
    let after = render_tokens(&after_tokens, max_bytes);
    assert!(before.len() <= max_bytes && after.len() <= max_bytes);
    assert!(before.len() <= HARD_MAX_FILE_BYTES && after.len() <= HARD_MAX_FILE_BYTES);

    GeneratedCase {
        index,
        seed,
        strategy,
        recipe,
        before,
        after,
        before_lines,
        after_lines,
    }
}

fn render_tokens(tokens: &[u32], max_bytes: usize) -> String {
    let line_bytes = 8 + LINE_SUFFIX.len();
    let limit = tokens.len().min(max_bytes / line_bytes);
    let mut output = String::with_capacity(limit * line_bytes);
    for token in &tokens[..limit] {
        writeln!(
            output,
            "{token:08x} token payload for similar performance fuzzing"
        )
        .unwrap();
    }
    output
}

fn write_result_row(
    output: &mut File,
    case_name: &str,
    case: &GeneratedCase,
    algorithm: Algorithm,
    outcome: &Outcome,
) -> Result<()> {
    writeln!(
        output,
        "{}\t{}\t{}\t{}\t{}\t{}\t{:.3}\t{}\t{}",
        case_name,
        case.strategy,
        case.before.len(),
        case.after.len(),
        algorithm_name(algorithm),
        outcome_label(outcome.kind),
        duration_ms(outcome.elapsed),
        outcome
            .worker_elapsed
            .map(|duration| format!("{:.3}", duration_ms(duration)))
            .unwrap_or_default(),
        outcome
            .ops
            .map(|value| value.to_string())
            .unwrap_or_default(),
    )?;
    Ok(())
}

fn persist_case(
    destination: &Path,
    case: &GeneratedCase,
    before_path: &Path,
    after_path: &Path,
    outcomes: &[Outcome],
    config: &RunConfig,
) -> Result<()> {
    fs::create_dir_all(destination)?;
    fs::copy(before_path, destination.join("before.txt"))?;
    fs::copy(after_path, destination.join("after.txt"))?;
    fs::write(destination.join("recipe.txt"), &case.recipe)?;
    for outcome in outcomes {
        if let Some(source) = &outcome.stack_path {
            fs::copy(
                source,
                destination.join(format!("{}.stack.txt", algorithm_name(outcome.algorithm))),
            )?;
        }
        if !outcome.stdout.trim().is_empty() && outcome.kind != OutcomeKind::Completed {
            fs::write(
                destination.join(format!("{}.stdout.txt", algorithm_name(outcome.algorithm))),
                &outcome.stdout,
            )?;
        }
        if !outcome.stderr.trim().is_empty() {
            fs::write(
                destination.join(format!("{}.stderr.txt", algorithm_name(outcome.algorithm))),
                &outcome.stderr,
            )?;
        }
    }

    let mut report = String::new();
    writeln!(report, "# Performance fuzz finding\n").unwrap();
    writeln!(report, "- Strategy: `{}`", case.strategy).unwrap();
    writeln!(report, "- Case index: `{}`", case.index).unwrap();
    writeln!(
        report,
        "- Case seed: `{}` (`0x{:016x}`)",
        case.seed, case.seed
    )
    .unwrap();
    writeln!(report, "- Recipe: {}", case.recipe).unwrap();
    writeln!(
        report,
        "- Input: `before.txt` ({} bytes, {} lines)",
        case.before.len(),
        case.before_lines
    )
    .unwrap();
    writeln!(
        report,
        "- Input: `after.txt` ({} bytes, {} lines)",
        case.after.len(),
        case.after_lines
    )
    .unwrap();
    writeln!(
        report,
        "- External timeout: `{}` ms",
        config.timeout.as_millis()
    )
    .unwrap();
    writeln!(
        report,
        "- Internal safety deadline: `{}`\n",
        if config.internal_deadline {
            "enabled (later than the external timeout)"
        } else {
            "disabled"
        }
    )
    .unwrap();

    writeln!(
        report,
        "| Algorithm | Result | Controller time | Worker time | Ops | Stack |"
    )
    .unwrap();
    writeln!(report, "| --- | --- | ---: | ---: | ---: | --- |").unwrap();
    for outcome in outcomes {
        let worker_time = outcome
            .worker_elapsed
            .map(|duration| format!("{:.1} ms", duration_ms(duration)))
            .unwrap_or_else(|| "—".into());
        let ops = outcome
            .ops
            .map(|value| value.to_string())
            .unwrap_or_else(|| "—".into());
        let stack = outcome.stack_path.as_ref().map_or_else(
            || "—".into(),
            |_| {
                let dumper = outcome.stack_dumper.as_deref().unwrap_or("unknown");
                format!(
                    "[`{}.stack.txt`]({}.stack.txt) ({dumper})",
                    algorithm_name(outcome.algorithm),
                    algorithm_name(outcome.algorithm)
                )
            },
        );
        writeln!(
            report,
            "| {} | {} | {:.1} ms | {} | {} | {} |",
            algorithm_name(outcome.algorithm),
            outcome_label(outcome.kind),
            duration_ms(outcome.elapsed),
            worker_time,
            ops,
            stack
        )
        .unwrap();
    }

    writeln!(report, "\n## Replay\n").unwrap();
    writeln!(
        report,
        "Run from the repository root (replace the algorithm as needed):\n"
    )
    .unwrap();
    let old = destination.join("before.txt");
    let new = destination.join("after.txt");
    writeln!(report, "```console").unwrap();
    writeln!(
        report,
        "cargo run --release --example perf-fuzz -- replay --algorithm all --timeout-ms {} --old {} --new {}{}",
        config.timeout.as_millis(),
        shell_quote(&old),
        shell_quote(&new),
        if config.internal_deadline {
            ""
        } else {
            " --no-internal-deadline"
        }
    )
    .unwrap();
    writeln!(report, "```\n").unwrap();
    writeln!(
        report,
        "The worker uses `TextDiff::diff_lines`. File I/O and UTF-8 validation happen before the ready handshake; tokenization, integer remapping, and the selected algorithm are timed. The process stack is sampled at the external timeout and the worker is then killed."
    )
    .unwrap();

    for outcome in outcomes {
        if outcome.kind == OutcomeKind::Crashed {
            writeln!(
                report,
                "\n### {} crash\n\n- Status: `{}`\n- stderr: [`{}.stderr.txt`]({}.stderr.txt)",
                algorithm_name(outcome.algorithm),
                outcome.exit_status.as_deref().unwrap_or("unknown"),
                algorithm_name(outcome.algorithm),
                algorithm_name(outcome.algorithm)
            )
            .unwrap();
        }
    }
    fs::write(destination.join("README.md"), report)?;
    Ok(())
}

fn write_summary(
    root: &Path,
    config: &RunConfig,
    findings: &[String],
    completed: usize,
    timed_out: usize,
    crashed: usize,
) -> Result<()> {
    let mut summary = String::new();
    writeln!(summary, "# Similar performance fuzz run\n").unwrap();
    writeln!(summary, "- Campaign seed: `{}`", config.seed).unwrap();
    writeln!(summary, "- Generated cases: `{}`", config.cases).unwrap();
    writeln!(summary, "- Algorithms per case: `{}`", ALL_ALGORITHMS.len()).unwrap();
    writeln!(
        summary,
        "- Timeout per algorithm: `{}` ms",
        config.timeout.as_millis()
    )
    .unwrap();
    writeln!(summary, "- Maximum bytes per file: `{}`", config.max_bytes).unwrap();
    writeln!(summary, "- Completed runs: `{completed}`").unwrap();
    writeln!(summary, "- Timed out runs: `{timed_out}`").unwrap();
    writeln!(summary, "- Crashed runs: `{crashed}`").unwrap();
    writeln!(summary, "- Full matrix: [`results.tsv`](results.tsv)\n").unwrap();
    writeln!(summary, "## Recorded findings\n").unwrap();
    if findings.is_empty() {
        writeln!(summary, "No timeouts or crashes were observed.").unwrap();
    } else {
        for finding in findings {
            writeln!(summary, "- [`{finding}`]({finding}/README.md)").unwrap();
        }
    }
    fs::write(root.join("summary.md"), summary)?;
    Ok(())
}

fn outcome_label(kind: OutcomeKind) -> &'static str {
    match kind {
        OutcomeKind::Completed => "completed",
        OutcomeKind::TimedOut => "timed-out",
        OutcomeKind::Crashed => "crashed",
        OutcomeKind::SetupTimedOut => "setup-timed-out",
    }
}

fn duration_ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn shell_quote(path: &Path) -> String {
    format!("'{}'", path.display().to_string().replace('\'', "'\\''"))
}

fn remove_if_exists(path: &Path) -> Result<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error.into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generators_obey_hard_size_limit() {
        for index in 0..16 {
            let case = generate_case(42, index, HARD_MAX_FILE_BYTES, HARD_MAX_FILE_BYTES);
            assert!(case.before.len() <= HARD_MAX_FILE_BYTES);
            assert!(case.after.len() <= HARD_MAX_FILE_BYTES);
            assert!(case.before.is_ascii());
            assert!(case.after.is_ascii());
            assert_eq!(case.before.lines().count(), case.before_lines);
            assert_eq!(case.after.lines().count(), case.after_lines);
        }
    }

    #[test]
    fn target_sizes_reach_configured_maximum() {
        assert_eq!(case_target_bytes(0, 3, 1_000_000), DEFAULT_MIN_CASE_BYTES);
        assert_eq!(case_target_bytes(2, 3, 1_000_000), 1_000_000);
        assert_eq!(case_target_bytes(0, 1, 1_000_000), 1_000_000);
    }

    #[test]
    fn worker_output_is_parsed() {
        let (elapsed, ops) = parse_worker_output("ok\tmyers\t1234\t17\n");
        assert_eq!(elapsed, Some(Duration::from_micros(1234)));
        assert_eq!(ops, Some(17));
    }
}
