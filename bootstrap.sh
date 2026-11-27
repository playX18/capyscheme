#!/usr/bin/env bash
# Builds the CapyScheme toolchain from the Rust runtime, in stages:
#
#   ./bootstrap.sh                                      build stage-0, stage-1, stage-2
#   (cd pkgs && ../stage-2/capy test --workspace)       run the test suite
#   stage-2/capy install --toolchain stage-2 --prefix P install it
#   stage-2/capy dist --toolchain stage-2 --format portable|deb|rpm
#
# Options:
#
#   --clean              remove the stage directories first
#   --skip-stage-0       the runtime of stage-0 is current: do not run cargo for it
#   --no-fhs             do not build the installed runtime (stage-2/fhs, below)
#   --profile P          cargo profile: release (default) or debug
#   --target TRIPLE      cross-compile (with `cross`)
#   --sysroot DIR        the fallback prefix compiled into the installed runtime (default /usr)
#   -j N, --jobs N       how many files are compiled at once (the number of CPUs by default)
#
# PROFILE, TARGET, SYSROOT and JOBS in the environment are the defaults of the options.
#
# The stages:
#
#   stage-0   the runtime built by cargo with the `bootstrap` feature. It has no compiled
#             libraries: it runs the Scheme sources of the packages (stage-0/<pkg>/src).
#   stage-1   stage-0's runtime runs the driver (pkgs/capy-pkg) from source, in toolchain mode:
#             `capy build --toolchain stage-1` compiles core, std, the extras and the driver
#             itself into stage-1/<pkg>/fasl, writes stage-1/capy.libmap and links stage-1/capy.
#   stage-2   the same build with stage-1's driver and runtime, into stage-2.
#   stage-2/fhs
#             the runtime that is installed and packaged:
#             capy-rt, libcapy and libcapy_static. `capy install --toolchain` and `capy dist
#             --toolchain` take it from there. 

set -euo pipefail
cd "$(dirname "$0")"

usage() { sed -n '2,/^# The stages:/p' "$0" | sed '$d' | sed 's/^# \{0,1\}//'; }

clean=0
skip0=0
fhs=1
PROFILE=${PROFILE:-release}
TARGET=${TARGET:-}
SYSROOT=${SYSROOT:-/usr}
JOBS=${JOBS:-}

while [ $# -gt 0 ]; do
  case "$1" in
    --clean) clean=1 ;;
    --skip-stage-0) skip0=1 ;;
    --no-fhs) fhs=0 ;;
    --profile) PROFILE=${2:?--profile needs a value}; shift ;;
    --target) TARGET=${2:?--target needs a value}; shift ;;
    --sysroot) SYSROOT=${2:?--sysroot needs a value}; shift ;;
    -j|--jobs) JOBS=${2:?--jobs needs a value}; shift ;;
    -h|--help|help) usage; exit 0 ;;
    *) echo "error: unknown option '$1'" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

HOST_TARGET=$(rustc --print host-tuple)
TARGET=${TARGET:-$HOST_TARGET}
JOBS=${JOBS:-$( (sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null) || echo 4)}
VERSION=$(awk -F '"' '/^version *=/ {print $2; exit}' Cargo.toml)
case "$TARGET" in
  *apple*) LIBCAPY=libcapy.dylib ;;
  *) LIBCAPY=libcapy.so ;;
esac
LIBCAPY_STATIC=libcapy_static.a
LIBCAPY_STATIC_LIBS=libcapy_static.libs

if [ "$HOST_TARGET" = "$TARGET" ]; then cargo_bin=cargo; else cargo_bin=cross; fi


TARGET_ROOT=$PWD/target
TARGET_PATH_BOOTSTRAP=$TARGET_ROOT/bootstrap/$TARGET/$PROFILE
TARGET_PATH_FHS=$TARGET_ROOT/fhs/$TARGET/$PROFILE

export MMTK_PLAN=${MMTK_PLAN:-Immix}
export CAPY_GC_MAX_HEAP=${CAPY_GC_MAX_HEAP:-2G}
export RUST_MIN_STACK=${RUST_MIN_STACK:-134217728}
export CAPY_TIME_LOADS=0

PKGS="core std common closer-mop r7rs-drafts capy-pkg"


build_static() {
  local sub=$1 out=$2; shift 2
  # The note is parsed below, so cargo must not colour it (CI sets CARGO_TERM_COLOR=always).
  CAPY_LIB_DIR=$out "$cargo_bin" rustc --color never --target-dir "$TARGET_ROOT/$sub" --profile "$PROFILE" \
    --target "$TARGET" -p capy-static --lib "$@" -- --print native-static-libs 2>&1 \
    | tee /dev/stderr | sed -n 's/^note: native-static-libs: //p' \
    | sed 's/-lcapy //g; s/ -lcapy$//' | tail -1 > "$out/$LIBCAPY_STATIC_LIBS"
  [ -s "$out/$LIBCAPY_STATIC_LIBS" ] || { echo "error: cargo printed no native-static-libs for capy-static" >&2; exit 1; }
}

build_runtime_bootstrap() {
  echo "Building CapyScheme $VERSION with profile '$PROFILE' for target '$TARGET'"
  "$cargo_bin" build --target-dir "$TARGET_ROOT/bootstrap" --profile "$PROFILE" --target "$TARGET" -p capy --features portable,bootstrap
  CAPY_LIB_DIR=$TARGET_PATH_BOOTSTRAP "$cargo_bin" build --target-dir "$TARGET_ROOT/bootstrap" --profile "$PROFILE" --target "$TARGET" -p capy-cli
  build_static bootstrap "$TARGET_PATH_BOOTSTRAP" --features portable --features bootstrap
}

build_runtime_fhs() {
  CAPY_SYSROOT=$SYSROOT "$cargo_bin" build --target-dir "$TARGET_ROOT/fhs" --no-default-features --profile "$PROFILE" --target "$TARGET" -p capy
  CAPY_LIB_DIR=$TARGET_PATH_FHS CAPY_SYSROOT=$SYSROOT "$cargo_bin" build --target-dir "$TARGET_ROOT/fhs" --profile "$PROFILE" --target "$TARGET" -p capy-cli
  CAPY_SYSROOT=$SYSROOT build_static fhs "$TARGET_PATH_FHS" --no-default-features
  rm -rf stage-2/fhs
  mkdir -p stage-2/fhs
  cp "$TARGET_PATH_FHS/capy-rt" "$TARGET_PATH_FHS/$LIBCAPY" \
    "$TARGET_PATH_FHS/$LIBCAPY_STATIC" "$TARGET_PATH_FHS/$LIBCAPY_STATIC_LIBS" stage-2/fhs/
}

stage0() {
  build_runtime_bootstrap
  mkdir -p stage-0
  local p
  for p in $PKGS; do
    mkdir -p "stage-0/$p"
    ln -sfn "../../pkgs/$p/src" "stage-0/$p/src"
  done
  rm -f stage-0/capy-rt "stage-0/$LIBCAPY" "stage-0/$LIBCAPY_STATIC" "stage-0/$LIBCAPY_STATIC_LIBS"
  cp "$TARGET_PATH_BOOTSTRAP/capy-rt" "$TARGET_PATH_BOOTSTRAP/$LIBCAPY" \
    "$TARGET_PATH_BOOTSTRAP/$LIBCAPY_STATIC" "$TARGET_PATH_BOOTSTRAP/$LIBCAPY_STATIC_LIBS" stage-0/
  ln -sf capy-rt stage-0/capyc
  ln -sf capy-rt stage-0/capy
}

populate() { # stage, runtime-directory
  local stage=$1 from=$2
  mkdir -p "$stage"
  [ -f "$from/$LIBCAPY_STATIC" ] || { echo "error: $from/$LIBCAPY_STATIC is missing (stage-0 was not built)" >&2; exit 1; }
  rm -f "$stage/capy-rt" "$stage/$LIBCAPY" "$stage/capyc" "$stage/$LIBCAPY_STATIC" "$stage/$LIBCAPY_STATIC_LIBS"
  cp "$from/capy-rt" "$from/$LIBCAPY" "$from/$LIBCAPY_STATIC" "$from/$LIBCAPY_STATIC_LIBS" "$stage/"
  ln -s capy-rt "$stage/capyc"
}

compiled() { (cd "$1" && find . \( -name '*.fasl' -o -name '*.wpo' \) -type f -not -path './cache/*' -not -path './fhs/*' -not -path './*/fasl/capy/c/gen/*' -not -path './std/fasl/capy/net/sys.*' | sort); }

check_psyntax() {
  gen=pkgs/core/tools/compile-psyntax.scm
  src=pkgs/core/src/boot/psyntax.scm
  exp=pkgs/core/src/boot/psyntax-exp.scm
  tmp=stage-1/.psyntax-exp.scm
  XDG_CACHE_HOME=stage-1/cache stage-1/capy-rt -s "$gen" "$src" "$tmp" > /dev/null 2>&1 || {
    echo "error: regenerating psyntax-exp.scm failed; run it by hand:" >&2
    echo "  stage-1/capy-rt -s $gen $src $exp" >&2
    exit 1
  }
  if ! cmp -s "$tmp" "$exp"; then
    rm -f "$tmp"
    echo "error: $exp is not the expansion of $src. Regenerate it with:" >&2
    echo "  stage-1/capy-rt -s $gen $src $exp" >&2
    exit 1
  fi
  rm -f "$tmp"
  echo "psyntax-exp.scm is up to date"
}

compare_stages() {
  local differ count f
  compiled stage-1 > stage-1/.compiled
  compiled stage-2 > stage-2/.compiled
  differ=$(
    { diff stage-1/.compiled stage-2/.compiled | sed -n 's/^[<>] /missing in one stage: /p'
      while read -r f; do cmp -s "stage-1/$f" "stage-2/$f" || echo "${f#./}"; done < stage-1/.compiled
      XDG_CACHE_HOME=stage-2/cache stage-2/capy-rt -s scripts/compare-libmaps.scm -- stage-1/capy.libmap stage-2/capy.libmap 2>&1 | grep -v 'INFO(' | sed 's/^/capy.libmap: /'
    } || true
  )
  count=$(wc -l < stage-1/.compiled | tr -d ' ')
  rm -f stage-1/.compiled stage-2/.compiled
  if [ -n "$differ" ]; then
    echo "error: stage-1 and stage-2 differ:" >&2
    echo "$differ" | sed 's/^/  /' >&2
    exit 1
  fi
  echo "stage-1 and stage-2 are byte-identical ($count compiled files (.fasl and .wpo) and the libmap)"
}

if [ "$clean" = 1 ]; then
  rm -rf stage-1 stage-2
  [ "$skip0" = 1 ] || rm -rf stage-0
fi

if [ "$skip0" = 0 ]; then
  echo "== stage-0"
  stage0
fi

echo "== layering"
XDG_CACHE_HOME=stage-0/cache stage-0/capy-rt -s scripts/check-layering.scm pkgs 2>&1 | grep -v 'INFO(' || true

echo "== stage-1"
populate stage-1 stage-0
XDG_CACHE_HOME=stage-1/cache stage-0/capy-rt \
  -s pkgs/capy-pkg/src/capy/pkg/main.scm -- build --toolchain stage-1 --packages pkgs -j "$JOBS"

echo "== psyntax-exp.scm is the expansion of psyntax.scm?"
check_psyntax

echo "== stage-2"
populate stage-2 stage-1
XDG_CACHE_HOME=stage-2/cache stage-1/capy build --toolchain stage-2 --packages pkgs -j "$JOBS"

echo "== stage-1 and stage-2 are the same?"
compare_stages

if [ "$fhs" = 1 ]; then
  echo "== stage-2/fhs (the installed runtime)"
  build_runtime_fhs
fi
