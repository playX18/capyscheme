#!/usr/bin/env python3
"""Rank the reasons the optimizing tier gave up on a procedure across the r7rs benchmarks.

Runs each benchmark once under CAPY_JIT_STATS=1, adds up the `tier 2 bailout:` lines the runtime
prints, and prints the reasons by total count (docs/REMEDIATION-PLAN.md, J5). The ranking orders
the OSR and inlining work in docs/JIT.md ("Known gaps").

    benchmarks/jit-bailouts.py [--capy stage-2/capy] [--timeout 120] [benchmark ...]
"""

import argparse
import collections
import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SUITE = Path(__file__).resolve().parent / "r7rs-benchmarks"
LINE = re.compile(r"tier 2 bailout:\s+(\d+)\s+(.*)$")
EVICT = re.compile(r"jit queue evictions:\s+(\d+)")


def program(source: Path) -> Path:
    out = SUITE / "capy" / source.name
    if not out.exists():
        out.parent.mkdir(exist_ok=True)
        out.write_text(
            source.read_text()
            + (SUITE / "src" / "common.scm").read_text()
            + '\n(define (this-scheme-implementation-name) "capy")\n'
            + (SUITE / "src" / "common-postlude.scm").read_text()
        )
    return out


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--capy", default=str(ROOT / "stage-2" / "capy"))
    ap.add_argument("--timeout", type=int, default=120)
    ap.add_argument("benchmarks", nargs="*")
    args = ap.parse_args()

    sources = {p.stem: p for p in (SUITE / "src").glob("*.scm") if (SUITE / "inputs" / f"{p.stem}.input").exists()}
    names = args.benchmarks or sorted(sources)
    totals = collections.Counter()
    evictions = 0
    for name in names:
        if name not in sources:
            print(f"skip {name}: no such benchmark", file=sys.stderr)
            continue
        env = dict(os.environ, CAPY_JIT_STATS="1")
        try:
            run = subprocess.run(
                [args.capy, "-s", str(program(sources[name]))],
                cwd=SUITE,
                input=(SUITE / "inputs" / f"{name}.input").read_text(),
                capture_output=True,
                text=True,
                timeout=args.timeout,
                env=env,
            )
        except subprocess.TimeoutExpired:
            print(f"timeout {name}", file=sys.stderr)
            continue
        found = 0
        for line in (run.stderr + run.stdout).splitlines():
            if m := LINE.search(line):
                totals[m.group(2).strip()] += int(m.group(1))
                found += 1
            elif m := EVICT.search(line):
                evictions += int(m.group(1))
        print(f"{name}: {found} bailout reasons", file=sys.stderr)
    print(f"queue evictions: {evictions}")
    print(f"{'count':>8}  reason")
    for reason, count in totals.most_common():
        print(f"{count:8d}  {reason}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
