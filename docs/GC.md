# GC tuning and diagnostics

CapyScheme uses MMTk for heap management and adds Capy-specific trigger heuristics in `capy/src/rsgc/heuristics.rs`. The `capy` launcher handles GC flags before the Scheme CLI starts, translating them into environment variables so both `capy` and `capyc` use the same runtime settings.

## CLI flags and environment variables

The mappings are defined in `capy/src/bin_support.rs`.

| CLI flag | Environment variable | Default shown by CLI | Purpose |
| --- | --- | --- | --- |
| `--gc-plan PLAN` | `MMTK_PLAN` | `StickyImmix` | Select the MMTk plan. CLI help lists `StickyImmix`, `ConcurrentImmix`, `MarkSweep`, and `Immix`. |
| `--gc-trigger TRIGGER` | `MMTK_GC_TRIGGER` | `Delegated` | Select the MMTk trigger policy. If unset, Capy installs its delegated trigger policy. |
| `--gc-max-heap SIZE` | `CAPY_GC_MAX_HEAP` | `2G` | Maximum heap size used by Capy's trigger heuristics. |
| `--gc-heuristic MODE` | `CAPY_GC_HEURISTIC` | `adaptive` | Capy trigger heuristic mode. |
| `--gc-min-free-percent PERCENT` | `CAPY_GC_MIN_FREE_PERCENT` | `10` | Minimum free-space threshold. Values above 100 are ignored. |
| `--gc-init-free-percent PERCENT` | `CAPY_GC_INIT_FREE_PERCENT` | `70` | Initial free-space threshold. Values above 100 are ignored. |
| `--gc-allocation-threshold-percent PERCENT` | `CAPY_GC_ALLOCATION_THRESHOLD_PERCENT` | `0` | Allocation-since-GC threshold used by compact mode. |
| `--gc-alloc-spike-percent PERCENT` | `CAPY_GC_ALLOC_SPIKE_PERCENT` | `5` | Extra heap headroom reserved while adaptive logic accounts for allocation spikes and GC-time penalties. |
| `--gc-learning-steps STEPS` | `CAPY_GC_LEARNING_STEPS` | `5` | Number of GC observations collected before adaptive predictions mature. |
| `--gc-guaranteed-interval-ms MILLISECONDS` | `CAPY_GC_GUARANTEED_INTERVAL_MS` | `300000` | Maximum interval before a heuristic-triggered GC. `0` disables this interval. |

Flags accept either `--flag value` or `--flag=value`. Parsing stops after `--`, so arguments after `--` are passed to the Scheme program unchanged.

## Heuristic modes

`CAPY_GC_HEURISTIC` accepts these values:

| Mode | Behavior |
| --- | --- |
| `adaptive` | Default. Uses GC-time and allocation-rate history to predict whether allocation will consume headroom before the next GC can complete. It also watches short-term spikes and accelerating allocation. |
| `static` | Uses the shared minimum-free threshold and guaranteed interval without adaptive prediction. |
| `compact` | Small-heap mode. Keeps the shared safeguards and also triggers after allocation since the previous GC exceeds a fixed share of the heap. Defaults to a 10% allocation threshold and a 30 second guaranteed interval. |
| `aggressive` | Requests GC at every trigger evaluation after active-cycle filtering. Useful for stress testing GC paths. |
| `passive` | Does not proactively trigger from Capy's heuristic; MMTk plan fallbacks can still require GC. |

## Diagnostics

Enable GC trace logging with `--log-trace`:

```sh
capy --log-trace --gc-max-heap=64M --gc-heuristic=adaptive -s benchmarks/adaptive-gc.scm
```

`--log-trace` raises the runtime log level to trace and enables GC logging before the MMTk builder is initialized. The GC logger records trigger decisions under the `capy::gc` target.

For comparison against MMTk's own trigger policies, pass `--gc-trigger`:

```sh
capy --log-trace --gc-trigger=FixedHeapSize:64M -s benchmarks/adaptive-gc.scm
capy --log-trace --gc-trigger=DynamicHeapSize:32M,64M -s benchmarks/adaptive-gc.scm
```

## Advanced environment-only knobs

Some adaptive internals are intentionally environment-only and are not exposed as CLI flags:

| Environment variable | Meaning |
| --- | --- |
| `CAPY_GC_ADAPTIVE_CONFIDENCE` | Multiplier used by adaptive prediction. |
| `CAPY_GC_ADAPTIVE_SPIKE_THRESHOLD` | Spike sensitivity used by adaptive prediction. |
| `CAPY_GC_ACCELERATION_SAMPLE_COUNT` | Number of samples used for acceleration tracking. |
| `CAPY_GC_ACCELERATION_SAMPLE_PERIOD_MS` | Sampling period for acceleration tracking. |
| `CAPY_GC_MOMENTARY_SPIKE_SAMPLE_COUNT` | Number of samples used for short-term spike tracking. |

Prefer the CLI flags for normal tuning. Use environment-only knobs when investigating adaptive trigger behavior or reproducing GC experiments.
