all: test

build:
	@cargo build --all-features

doc:
	@cargo doc --all-features

test:
	@cargo test
	@cargo test --all-features
	@cargo check --no-default-features
	@cargo check --no-default-features --features hashbrown
	@cargo check --no-default-features --features bytes

.PHONY: wasi-test
wasi-test:
	@cargo test --all-features --target=wasm32-wasip1 -- --nocapture

format:
	@rustup component add rustfmt 2> /dev/null
	@cargo fmt --all

format-check:
	@rustup component add rustfmt 2> /dev/null
	@cargo fmt --all -- --check

lint:
	@rustup component add clippy 2> /dev/null
	@cargo clippy

bench:
	@cargo bench --bench diffs

bench-memory:
	@cargo bench --bench allocations

perf-fuzz:
	@cargo run --release --example perf-fuzz -- $(PERF_FUZZ_ARGS)

.PHONY: all doc test format format-check lint bench bench-memory perf-fuzz
