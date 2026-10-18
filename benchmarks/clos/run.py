#!/usr/bin/env python3
import argparse, csv, json, os, platform, re, shutil, statistics, subprocess, sys, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
BUILD = HERE / "build"
OUT = ROOT / "benchmarks" / "results"

SCM_PRELUDES = {
    "capy": "(import (capy) (scheme time))\n(define <boolean> <bool>)\n"
            "(define (now) (/ (exact->inexact (current-jiffy)) (jiffies-per-second)))\n",
    "guile": "(use-modules (oop goops))\n"
             "(define (now) (/ (get-internal-real-time) (exact->inexact internal-time-units-per-second)))\n",
    "gauche": "(define (now) (/ (current-microseconds) 1e6))\n",
}

LISP_PRELUDE = "(in-package :cl-user)\n"
LISP_MAIN = "\n(main)\n"


def wrap_slot_lists(src):
    """Guile spreads slot specs after the superclass list; Capy and Gauche
    take them as one list. Rewrite every define-class accordingly."""
    out, i = [], 0
    while True:
        j = src.find("(define-class ", i)
        if j < 0:
            out.append(src[i:])
            return "".join(out)

        def skip_form(k):  # index just past the form starting at src[k]
            while src[k].isspace():
                k += 1
            if src[k] != "(":
                while not src[k].isspace() and src[k] not in "()":
                    k += 1
                return k
            depth = 0
            while True:
                depth += {"(": 1, ")": -1}.get(src[k], 0)
                k += 1
                if depth == 0:
                    return k

        k = j + len("(define-class")
        k = skip_form(k)  # name
        k = skip_form(k)  # supers
        end = skip_form(j)  # whole form, one past ')'
        out.append(src[i:k])
        out.append(" (" + src[k:end - 1].strip() + "))")
        i = end


def scheme_program(impl):
    body = (HERE / "src" / "body.scm").read_text()
    if impl != "guile":
        body = wrap_slot_lists(body)
    if impl == "gauche":
        body = body.replace("#:", ":")
        # (define-method (f (a <c>) b) ...) -> (define-method f ((a <c>) b) ...)
        param = r"(?:\([^()\s]+\s+[^()\s]+\)|[^()\s]+)"
        body = re.sub(
            r"\(define-method \((\S+)((?:\s+%s)*)\)" % param,
            lambda m: "(define-method %s (%s)" % (m.group(1), m.group(2).strip()),
            body,
        )
    return SCM_PRELUDES[impl] + body


def build():
    BUILD.mkdir(exist_ok=True)
    for impl in SCM_PRELUDES:
        (BUILD / f"clos-{impl}.scm").write_text(scheme_program(impl))
    lisp = LISP_PRELUDE + (HERE / "src" / "body.lisp").read_text() + LISP_MAIN
    (BUILD / "clos.lisp").write_text(lisp)


def commands():
    lisp_driver = lambda ext: (
        '(load (compile-file "%s" :output-file "%s"))' % (BUILD / "clos.lisp", BUILD / f"clos-{ext}.fasl")
    )
    return {
        "capy": ["capy", "-L", str(ROOT / "lib"), "-s", str(BUILD / "clos-capy.scm")],
        "gauche": ["gosh", str(BUILD / "clos-gauche.scm")],
        "guile": ["guile", "-q", str(BUILD / "clos-guile.scm")],
        "sbcl": ["sbcl", "--noinform", "--non-interactive", "--eval", lisp_driver("sbcl")],
        "clasp": ["clasp", "--noinform", "--non-interactive", "--eval", lisp_driver("clasp")],
        "abcl": ["abcl", "--noinform", "--noinit", "--batch", "--eval", lisp_driver("abcl")],
    }


def version(impl):
    cmd = {"capy": ["capy", "--version"], "gauche": ["gosh", "-V"], "guile": ["guile", "--version"],
           "sbcl": ["sbcl", "--version"], "clasp": ["clasp", "--version"],
           "abcl": ["abcl", "--version"]}[impl]
    try:
        out = subprocess.run(cmd, capture_output=True, text=True, timeout=60).stdout
        lines = [l for l in out.splitlines() if l.strip() and not l.startswith("Failed")]
        return lines[0].strip() if lines else "?"
    except Exception:
        return "?"


def run_once(impl, cmd, timeout):
    t0 = time.time()
    p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout, cwd=ROOT)
    res = {}
    for line in p.stdout.splitlines():
        m = re.match(r"RESULT (\S+) (\S+) (\S+)", line.strip())
        if m:
            res[m.group(1)] = (float(m.group(2)), m.group(3))
    if p.returncode != 0 or not res:
        sys.stderr.write(f"[{impl}] exit={p.returncode}\n{p.stdout[-1500:]}\n{p.stderr[-1500:]}\n")
    return res, time.time() - t0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--runs", type=int, default=3)
    ap.add_argument("--only", default="")
    ap.add_argument("--timeout", type=int, default=1200)
    args = ap.parse_args()

    build()
    cmds = commands()
    wanted = [i for i in cmds if not args.only or i in args.only.split(",")]
    data = {}
    for impl in wanted:
        if not shutil.which(cmds[impl][0]):
            print(f"skip {impl}: not on PATH")
            continue
        runs = []
        for r in range(args.runs):
            res, wall = run_once(impl, cmds[impl], args.timeout)
            print(f"{impl} run {r + 1}/{args.runs}: {len(res)} benchmarks, {wall:.1f}s wall", flush=True)
            runs.append(res)
        names = {n for r in runs for n in r}
        data[impl] = {
            n: {
                "seconds": statistics.median(r[n][0] for r in runs if n in r),
                "samples": [r[n][0] for r in runs if n in r],
                "checksum": next(r[n][1] for r in runs if n in r),
            }
            for n in names
        }

    # Cross-implementation checksum validation (numeric compare: Lisp prints ints, Schemes too).
    bad = []
    for n in sorted({n for d in data.values() for n in d}):
        sums = {impl: d[n]["checksum"] for impl, d in data.items() if n in d}
        if len(set(sums.values())) > 1:
            bad.append((n, sums))
    for n, sums in bad:
        print(f"CHECKSUM MISMATCH {n}: {sums}")

    OUT.mkdir(parents=True, exist_ok=True)
    meta = {
        "machine": platform.platform(), "cpu": platform.machine(),
        "versions": {i: version(i) for i in data}, "runs": args.runs,
        "date": time.strftime("%Y-%m-%d"),
    }
    (OUT / "clos.json").write_text(json.dumps({"meta": meta, "results": data}, indent=1))
    with open(OUT / "clos.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["implementation", "benchmark", "median_seconds", "checksum"])
        for impl, d in data.items():
            for n, v in d.items():
                w.writerow([impl, n, f"{v['seconds']:.6f}", v["checksum"]])
    print("wrote", OUT / "clos.json")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
