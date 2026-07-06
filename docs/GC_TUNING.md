# GC Tuning

CapyScheme uses MMTk for memory management and wraps MMTk's trigger callback
with Capy-specific heap heuristics. The same GC flags are available through the
`capy` runtime CLI and the `capyc` compiler CLI.

Use this guide when a workload needs a different heap limit, when bootstrap or CI
needs stable memory behavior, or when investigating GC frequency with trace logs.

## How CLI Flags Reach the Runtime

GC flags are handled before the Scheme runtime starts:

1. `capy/src/bin_support.rs` scans process arguments for known `--gc-*` flags.
2. Each flag is translated into an environment variable.
3. The flag is removed before control enters the Scheme CLI in `lib/boot/cli.scm`.
4. The GC builder and Capy trigger policy read those environment variables while
   initializing the runtime.

Both spellings are accepted:

```sh
capy --gc-max-heap=4G -s program.sps
capy --gc-max-heap 4G -s program.sps
```

Argument scanning stops after `--`, so flags after it are passed to the Scheme
program instead of configuring GC:

```sh
capy -s program.sps -- --gc-max-heap=4G
```

## Main Flags

| CLI flag | Environment variable | Default | Notes |
| --- | --- | --- | --- |
| `--gc-plan PLAN` | `MMTK_PLAN` | `StickyImmix` | Supported Capy plans are `StickyImmix`, `ConcurrentImmix`, `MarkSweep`, and `Immix`. Unsupported plans fall back to `StickyImmix`. |
| `--gc-trigger TRIGGER` | `MMTK_GC_TRIGGER` | `Delegated` | Passed to MMTk's trigger selector. CI uses values such as `DynamicHeapSize:1G,8G`. |
| `--gc-max-heap SIZE` | `CAPY_GC_MAX_HEAP` | `2G` | Sets the fixed heap size used by Capy's trigger policy. This takes precedence over heap sizes embedded in `MMTK_GC_TRIGGER`. |
| `--gc-heuristic MODE` | `CAPY_GC_HEURISTIC` | `adaptive` | Capy-specific policy. Accepted modes are listed below. |
| `--gc-min-free-percent PERCENT` | `CAPY_GC_MIN_FREE_PERCENT` | `10` | Minimum free heap percentage before Capy forces a collection. Values above `100` are ignored. |
| `--gc-init-free-percent PERCENT` | `CAPY_GC_INIT_FREE_PERCENT` | `70` | Learning-phase free heap threshold for adaptive mode. Values above `100` are ignored. |
| `--gc-allocation-threshold-percent PERCENT` | `CAPY_GC_ALLOCATION_THRESHOLD_PERCENT` | `0` | Allocation-since-last-GC threshold as a percentage of heap size. Compact mode defaults this to `10`. |
| `--gc-alloc-spike-percent PERCENT` | `CAPY_GC_ALLOC_SPIKE_PERCENT` | `5` | Allocation spike threshold used by shared heuristic state. Values above `100` are ignored. |
| `--gc-learning-steps STEPS` | `CAPY_GC_LEARNING_STEPS` | `5` | Number of GC cycles adaptive mode observes before relying on predictions. |
| `--gc-guaranteed-interval-ms MILLISECONDS` | `CAPY_GC_GUARANTEED_INTERVAL_MS` | `300000` | Maximum interval between proactive collections. `0` disables this interval. Compact mode defaults to `30000`. |

`SIZE` accepts an integer with an optional `b`, `k`, `kb`, `m`, `mb`, `g`, or
`gb` suffix. Suffixes use powers of 1024.

Invalid numeric values are ignored and the current default for that setting is
kept. Percentage values must be between `0` and `100`.

## Heuristic Modes

`CAPY_GC_HEURISTIC` is read case-insensitively. Unknown values fall back to
`adaptive`.

| Mode | Intent | Behavior |
| --- | --- | --- |
| `adaptive` | General-purpose default. | Starts conservatively during a learning phase, then predicts whether allocation pressure, short-term spikes, or accelerating allocation rates will consume remaining heap before a GC can finish. |
| `static` | Predictable baseline. | Uses the minimum-free threshold and guaranteed interval without adaptive prediction. |
| `compact` | Keep small heaps clean. | Uses the minimum-free threshold, collects after allocation since the previous GC exceeds a heap percentage, and has a shorter guaranteed interval. |
| `aggressive` | Stress GC paths or cap heap growth. | Requests a GC at every trigger evaluation unless a collection is already active. |
| `passive` | Disable proactive Capy triggers. | Lets MMTk plan fallbacks require GC, but Capy's heuristic does not proactively request cycles. |

## Advanced Environment-Only Knobs

These settings are not exposed as CLI flags. Use them only when debugging
adaptive behavior or building reproducible GC experiments.

| Environment variable | Default | Constraint |
| --- | --- | --- |
| `CAPY_GC_ADAPTIVE_CONFIDENCE` | `1.8` | Parsed as `f64`; invalid values are ignored. |
| `CAPY_GC_ADAPTIVE_SPIKE_THRESHOLD` | `1.8` | Parsed as `f64`; invalid values are ignored. |
| `CAPY_GC_ACCELERATION_SAMPLE_COUNT` | `8` | Parsed as `usize`; must be greater than `0`. |
| `CAPY_GC_MOMENTARY_SPIKE_SAMPLE_COUNT` | `3` | Parsed as `usize`; must be greater than `0`. |
| `CAPY_GC_ACCELERATION_SAMPLE_PERIOD_MS` | `15` | Parsed as milliseconds. |

Example:

```sh
CAPY_GC_ADAPTIVE_CONFIDENCE=2.4 \
CAPY_GC_ACCELERATION_SAMPLE_COUNT=12 \
capy --gc-max-heap=6G -s workload.sps
```

## Common Workflows

### Give a large bootstrap run more space

```sh
CAPY_GC_MAX_HEAP=8G make build
```

The bootstrap Makefile already uses `MMTK_PLAN=StickyImmix` and sets
`CAPY_GC_MAX_HEAP=8G` for stage-0 warm-up commands.

### Match CI heap behavior locally

```sh
MMTK_GC_TRIGGER=DynamicHeapSize:1G,8G make test
```

This mirrors the dynamic heap trigger shape used by the CI and nightly
workflows. Use `CAPY_GC_MAX_HEAP` when you need Capy's trigger policy to use a
specific maximum heap size regardless of the MMTk trigger string.

### Investigate GC trigger decisions

```sh
capy --log-trace --gc-heuristic=adaptive --gc-max-heap=2G -s workload.sps
```

`--log-trace` enables Capy GC trace logging and the Rust logger uses the default
filter `mmtk=trace,capy::gc=trace` unless `RUST_LOG` is already set. Trigger
messages include the mode, reason, heap totals, free/available bytes, and bytes
allocated since the previous GC.

## Troubleshooting

- **A flag appears in `program-arguments`:** place GC flags before `--`. The
  pre-parser intentionally stops scanning at `--`.
- **A heap limit does not change:** check whether both `MMTK_GC_TRIGGER` and
  `CAPY_GC_MAX_HEAP` are set. `CAPY_GC_MAX_HEAP` wins for Capy's trigger policy.
- **A percentage setting has no effect:** values above `100` are ignored.
- **An unsupported plan was requested:** Capy's MMTk builder falls back to
  `StickyImmix`.
- **Collections happen too often:** avoid `aggressive`, lower
  `--gc-min-free-percent` only with care, and check trace output for the accepted
  trigger reason before changing multiple knobs at once.
