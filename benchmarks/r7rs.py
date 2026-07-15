#!/usr/bin/env python3
"""Run Capy and locally installed Schemes against ecraven/r7rs-benchmarks."""

import argparse
import csv
import html
import os
import platform
import re
import shutil
import statistics
import subprocess
import sys
import tempfile
from pathlib import Path

import matplotlib.pyplot as plt


ROOT = Path(__file__).resolve().parent.parent
SUITE = Path(__file__).resolve().parent / "r7rs-benchmarks"
CHEZ_LIBRARIES = Path(__file__).resolve().parent / "chez-libraries"
RESULT_RE = re.compile(r"^\+!CSVLINE!\+[^,]+,([^,]+),([\d.]+)$", re.MULTILINE)
IMPLEMENTATIONS = {
    "capy": ("capy",),
    "chez": ("chez",),
    "chibi": ("chibi-scheme",),
    "chicken": ("chicken-csc",),
    "gambitc": ("gsc",),
    "gauche": ("gosh",),
    "guile": ("guile",),
    "kawa": ("kawa",),
    "mit": ("mit-scheme",),
    "petite-chez": ("petite",),
    "racket": ("racket", "raco"),
    "sagittarius": ("sagittarius",),
}


def benchmark_sources(suite, names):
    sources = {
        source.stem: source
        for source in suite.joinpath("src").glob("*.scm")
        if "(define (run-benchmark)" in source.read_text()
    }
    missing = sorted(set(names) - sources.keys())
    if missing:
        raise ValueError(f"unknown benchmark(s): {', '.join(missing)}")
    return [sources[name] for name in names] if names else sorted(sources.values())


def run_capy(capy, suite, source, timeout):
    input_file = suite / "inputs" / f"{source.stem}.input"
    program = suite / "capy" / source.name
    if not program.exists():
        program.parent.mkdir(exist_ok=True)
        program.write_text(
            source.read_text()
            + (suite / "src" / "common.scm").read_text()
            + '\n(define (this-scheme-implementation-name) "capy")\n'
            + (suite / "src" / "common-postlude.scm").read_text()
        )

    print(f"Running {program} with {capy}")
    return subprocess.run(
        [str(capy), "-s", str(program)],
        cwd=suite,
        input=input_file.read_text(),
        capture_output=True,
        text=True,
        timeout=timeout,
    )


def run_upstream(implementation, suite, source, timeout):
    return subprocess.run(
        ["./bench", "-r", "1", implementation, source.stem],
        cwd=suite,
        capture_output=True,
        text=True,
        timeout=timeout,
    )


def elapsed_seconds(result, implementation, source):
    output = result.stdout + result.stderr
    if result.returncode:
        raise RuntimeError(f"{implementation} {source.stem} failed:\n{output}")
    matches = RESULT_RE.findall(output)
    if not matches:
        raise RuntimeError(f"{implementation} {source.stem} produced no timing:\n{output}")
    return float(matches[-1][1])


def write_plot(samples, output):
    implementations = sorted(samples)
    benchmarks = sorted(
        {benchmark for implementation_samples in samples.values() for benchmark in implementation_samples},
        key=lambda benchmark: min(
            statistics.mean(implementation_samples[benchmark])
            for implementation_samples in samples.values()
            if benchmark in implementation_samples
        ),
    )
    height = max(5, len(benchmarks) * (0.28 * len(implementations) + 0.15))
    figure, axis = plt.subplots(figsize=(12, height))
    bar_height = 0.8 / len(implementations)

    for index, implementation in enumerate(implementations):
        positions = []
        means = []
        deviations = []
        for position, benchmark in enumerate(benchmarks):
            times = samples[implementation].get(benchmark)
            if not times:
                continue
            positions.append(position + index * bar_height)
            means.append(statistics.mean(times))
            deviations.append(statistics.stdev(times) if len(times) > 1 else 0)
        axis.barh(positions, means, bar_height, xerr=deviations, capsize=2, label=implementation)

    axis.set_yticks(
        [position + bar_height * (len(implementations) - 1) / 2 for position in range(len(benchmarks))],
        benchmarks,
    )
    axis.invert_yaxis()
    axis.set_xscale("log")
    axis.set_xlabel("time (seconds; lower is better)")
    axis.set_title("R7RS benchmarks")
    axis.grid(axis="x", linestyle="--", alpha=0.4)
    axis.legend()
    figure.tight_layout()
    figure.savefig(output, dpi=150)


def machine_specs(capy):
    cpu = platform.processor() or platform.machine()
    memory = "unknown"
    try:
        for line in Path("/proc/cpuinfo").read_text().splitlines():
            if line.startswith("model name"):
                cpu = line.split(":", 1)[1].strip()
                break
        for line in Path("/proc/meminfo").read_text().splitlines():
            if line.startswith("MemTotal:"):
                memory = f"{int(line.split()[1]) / 1024 / 1024:.1f} GiB"
                break
    except OSError:
        pass
    return [
        ("Operating system", platform.platform()),
        ("CPU", cpu),
        ("Logical CPUs", str(os.cpu_count())),
        ("Memory", memory),
        ("Python", platform.python_version()),
        ("Capy executable", str(capy)),
    ]


def write_report(samples, failures, specs, output, plot):
    rows = []
    for implementation, implementation_samples in sorted(samples.items()):
        for benchmark, times in sorted(implementation_samples.items()):
            deviation = statistics.stdev(times) if len(times) > 1 else 0
            rows.append(
                "<tr>"
                f"<td>{html.escape(implementation)}</td>"
                f"<td>{html.escape(benchmark)}</td>"
                f"<td>{statistics.mean(times):.6f}</td>"
                f"<td>{deviation:.6f}</td>"
                f"<td>{len(times)}</td>"
                "</tr>"
            )
    failed_rows = [
        "<tr>"
        f"<td>{html.escape(implementation)}</td>"
        f"<td>{html.escape(benchmark)}</td>"
        f"<td>{html.escape(error)}</td>"
        "</tr>"
        for implementation, benchmark, error in failures
    ]
    spec_rows = "".join(
        f"<tr><th>{html.escape(name)}</th><td>{html.escape(value)}</td></tr>"
        for name, value in specs
    )
    plot_link = os.path.relpath(plot, output.parent)
    output.write_text(
        "<!doctype html><html><head><meta charset=\"utf-8\">"
        "<title>R7RS benchmark report</title>"
        "<style>body{font:16px system-ui,sans-serif;margin:2rem;max-width:1200px}"
        "table{border-collapse:collapse;margin:1rem 0}th,td{border:1px solid #bbb;padding:.4rem .6rem;text-align:left}"
        "th{background:#eee}img{max-width:100%;height:auto}</style></head><body>"
        "<h1>R7RS benchmark report</h1><h2>Machine</h2><table>"
        f"{spec_rows}</table><h2>Plot</h2><img src=\"{html.escape(plot_link)}\" alt=\"R7RS benchmark plot\">"
        "<h2>Results</h2><table><tr><th>Implementation</th><th>Benchmark</th>"
        "<th>Mean seconds</th><th>Std. dev. seconds</th><th>Runs</th></tr>"
        f"{''.join(rows)}</table>"
        + (
            "<h2>Failures</h2><table><tr><th>Implementation</th><th>Benchmark</th><th>Error</th></tr>"
            f"{''.join(failed_rows)}</table>"
            if failed_rows
            else ""
        )
        + "</body></html>"
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--benchmark", action="append", default=[], help="benchmark to run; repeatable (default: all)")
    parser.add_argument("--implementation", action="append", default=[], help="Scheme to run; repeatable or comma-separated (default: installed implementations)")
    parser.add_argument("--capy", type=Path, default="capy", help="Capy executable")
    parser.add_argument("--runs", type=int, default=5, help="timed runs per benchmark (default: 5)")
    parser.add_argument("--timeout", type=int, default=300, help="per-run timeout in seconds (default: 300)")
    parser.add_argument("--csv", type=Path, default=ROOT / "benchmarks" / "r7rs-results.csv", help="raw result CSV")
    parser.add_argument("--output", type=Path, default=ROOT / "benchmarks" / "r7rs-benchmark.png", help="output plot path")
    parser.add_argument("--report", type=Path, default=ROOT / "benchmarks" / "r7rs-report.html", help="HTML report path")
    parser.add_argument("--self-test", action="store_true", help="check the result parser")
    args = parser.parse_args()

    if args.self_test:
        assert RESULT_RE.search("+!CSVLINE!+capy,fib:40:5,1.234567")
        return
    if args.runs < 1 or args.timeout < 1:
        parser.error("--runs and --timeout must be positive")
    if not SUITE.joinpath("bench").is_file():
        parser.error(f"R7RS submodule is not initialized: {SUITE}")

    implementations = [name for names in args.implementation for name in names.split(",")]
    unknown_implementations = sorted(set(implementations) - IMPLEMENTATIONS.keys())
    if unknown_implementations:
        parser.error(f"unknown implementation(s): {', '.join(unknown_implementations)}")
    implementations = implementations or [
        name for name, commands in IMPLEMENTATIONS.items() if name == "capy" or all(shutil.which(command) for command in commands)
    ]
    if "capy" in implementations and not args.capy.is_file():
        parser.error(f"Capy executable not found: {args.capy}")
    args.capy = args.capy.resolve()
    samples = {implementation: {} for implementation in implementations}
    failures = []

    with tempfile.TemporaryDirectory(prefix="r7rs-benchmarks-") as directory:
        suite = Path(directory) / "suite"
        shutil.copytree(SUITE, suite, ignore=shutil.ignore_patterns("results.*", "all.csv", "out"))
        if "chez" in implementations:
            shutil.copytree(CHEZ_LIBRARIES, suite / "chez-libraries")
            bench = suite / "bench"
            bench.write_text(
                bench.read_text().replace("/home/nex/scheme/chez", str(suite / "chez-libraries"))
            )
        for source in benchmark_sources(suite, args.benchmark):
            print(f"{source.stem}:", flush=True)
            for implementation in implementations:
                times = []
                print(f"  {implementation}:", flush=True)
              
                for run in range(args.runs):
                    print(f"    run {run + 1}/{args.runs}...", end="", flush=True)
                    try:
                        result = (
                            run_capy(args.capy, suite, source, args.timeout)
                            if implementation == "capy"
                            else run_upstream(implementation, suite, source, args.timeout)
                        )
                        seconds = elapsed_seconds(result, implementation, source)
                    except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
                        print(f" FAILED: {error}", file=sys.stderr)
                        lines = [line.strip() for line in str(error).splitlines() if line.strip()]
                        summary = next((line for line in lines if line.startswith("Exception:")), lines[0])
                        failures.append((implementation, source.stem, summary))
                        break
                    times.append(seconds)
                    print(f" {seconds:.6f}s")
                if times:
                    samples[implementation][source.stem] = times
                    deviation = statistics.stdev(times) if len(times) > 1 else 0
                    print(f"    mean {statistics.mean(times):.6f}s (std {deviation:.6f}s)")

    samples = {implementation: times for implementation, times in samples.items() if times}
    if not samples:
        print("no successful runs", file=sys.stderr)
        return 1

    args.csv.parent.mkdir(parents=True, exist_ok=True)
    with args.csv.open("w", newline="") as file:
        writer = csv.writer(file)
        writer.writerow(["implementation", "benchmark", "run", "seconds"])
        for implementation, implementation_samples in samples.items():
            for benchmark, times in implementation_samples.items():
                writer.writerows(
                    (implementation, benchmark, run + 1, f"{seconds:.6f}")
                    for run, seconds in enumerate(times)
                )

    args.output.parent.mkdir(parents=True, exist_ok=True)
    write_plot(samples, args.output)
    args.report.parent.mkdir(parents=True, exist_ok=True)
    write_report(samples, failures, machine_specs(args.capy), args.report, args.output)
    print(f"wrote {args.csv}")
    print(f"wrote {args.output}")
    print(f"wrote {args.report}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
