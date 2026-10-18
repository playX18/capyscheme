# Benchmarks

## R7RS suite

The R7RS benchmarks are provided by the `r7rs-benchmarks` submodule. Initialize it after cloning:

```sh
git submodule update --init --recursive
```

Run every benchmark against every locally detected implementation:

```sh
make benchmark-r7rs
```

For a quick comparison, select benchmarks and implementations explicitly:

```sh
make benchmark-r7rs R7RS_BENCHMARKS="fib tak" R7RS_IMPLEMENTATIONS="capy guile gauche"
```

Adjust the number of timed runs or the per-run timeout with `R7RS_BENCH_RUNS` and `R7RS_BENCH_TIMEOUT`:

```sh
make benchmark-r7rs R7RS_BENCH_RUNS=7 R7RS_BENCH_TIMEOUT=600
```

The runner supports Capy, Chez, Chibi, Chicken, Gambit, Gauche, Guile, Kawa, MIT Scheme, Petite Chez, Racket, and Sagittarius when their required commands are installed. It uses the upstream suite's implementation-specific driver for non-Capy systems. 

Results are written to:

- `benchmarks/r7rs-results.csv` — raw timings
- `benchmarks/r7rs-benchmark.png` — comparison plot
- `benchmarks/r7rs-report.html` — timings, failures, and machine details

## CLOS suite

Seven object-system micro-benchmarks (single dispatch, 4-class dispatch, two-argument multi-methods, `next-method` chains, accessors, `make`/`make-instance`, and a recursive AST evaluator) run against Capy, Gauche, Guile (GOOPS), SBCL, ABCL and Clasp. The Scheme program is generated from `clos/src/body.scm` and the Lisp one from `clos/src/body.lisp`.

```sh
python3 benchmarks/clos/run.py     # writes benchmarks/results/clos.{json,csv}
```

