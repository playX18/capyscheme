# CapyScheme Makefile (ported from Justfile)
#
# Usage examples:
#   make build                 # build runtime + bootstrap stages 0/1/2
#   make build-runtime         # build the static Rust capy launcher
#   make stage-0
#   make stage-1
#   make stage-2
#   make install-portable      # installs into $(PREFIX)/capy/$(VERSION)
#   make dist-portable         # produces dist tarball (no install)
#   make install               # FHS-ish layout under $(PREFIX) (uses sudo if needed)
#
# Override knobs:
#   make PROFILE=debug
#   make TARGET=x86_64-unknown-linux-gnu
#   make PORTABLE=0            # default 1
#   make PREFIX=/usr/local
#

SHELL := /usr/bin/env bash

# Fail fast helper: $(call require_var,OUT)
require_var = $(if $(strip $($(1))),,$(error $(1) is required))

PROFILE ?= release
TARGET  ?= $(shell rustc --print host-tuple)
HOST_TARGET := $(shell rustc --print host-tuple)

TARGET_DIR  := target/$(TARGET)
TARGET_PATH := $(TARGET_DIR)/$(PROFILE)

# Compile psyntax during stage-0 creation. By default set to 0 as
# it's not strictly needed for bootstrapping unless you change
# psyntax.scm.
COMPILE_PSYNTAX ?= 0

PREFIX ?= $(HOME)/.local/share

# Try to match the old Justfile version discovery, but allow override.
VERSION ?= $(shell (cargo info capy 2>/dev/null | awk '/^version:/ {print $$2}' | head -n 1) || true)
ifeq ($(strip $(VERSION)),)
VERSION := $(shell awk -F '"' '/^version\s*=/{print $$2; exit}' capy/Cargo.toml)
endif

UNAME_S := $(shell uname -s)
COMPILED_SCM_EXT := fasl

ARCH := $(shell rustc -vV | awk '/^host:/ {print $$2}' | cut -d- -f1)

# Use cross when target differs.
ifeq ($(HOST_TARGET),$(TARGET))
CARGO_BIN := cargo
else
CARGO_BIN := cross
endif

PORTABLE ?= 1

# Environment used when invoking capyc to compile libraries (mirrors Justfile defaults)
MMTK_PLAN ?= StickyImmix
XDG_CACHE_HOME = stage-0/cache
CAPY_LOAD_PATH ?= ./lib
CAPY_ENV = \
	MMTK_PLAN="$(MMTK_PLAN)" \
	XDG_CACHE_HOME="$(XDG_CACHE_HOME)" \
	CAPY_LOAD_PATH="$(CAPY_LOAD_PATH)"


BOOT_SRCS := \
	lib/boot/prim.scm \
	lib/boot/control.scm \
	lib/boot/modules.scm \
	lib/boot/records.scm \
	lib/boot/conditions.scm \
	lib/boot/violations.scm \
	lib/boot/raise.scm \
	lib/boot/exceptions.scm \
	lib/boot/expand.scm \
	lib/boot/interpreter.scm \
	lib/boot/enums.scm \
	lib/boot/sys.scm \
	lib/boot/osdep.scm \
	lib/boot/iosys.scm \
	lib/boot/iosys2.scm \
	lib/boot/iosys3.scm \
	lib/boot/portio.scm \
	lib/boot/bytevectorio.scm \
	lib/boot/fileio.scm \
	lib/boot/conio.scm \
	lib/boot/stringio.scm \
	lib/boot/stdio.scm \
	lib/boot/utf16.scm \
	lib/boot/customio.scm \
	lib/boot/print.scm \
	lib/boot/format.scm \
	lib/boot/log.scm \
	lib/boot/match-syntax.scm \
	lib/boot/psyntax-exp.scm \
	lib/boot/str2num.scm \
	lib/boot/num2str.scm \
	lib/boot/reader.scm \
	lib/boot/eval.scm \
	lib/boot/base.scm \
	lib/boot/compiler.scm \
	lib/boot/libraries.scm \
	lib/boot/match.scm

CORE_SRCS := \
	lib/core/parameters.scm \
	lib/core/io.scm \
	lib/core/files.scm \
	lib/core/exceptions.scm \
	lib/core/arithmetic.scm \
	lib/core/sorting.scm \
	lib/core/bytevectors.scm \
	lib/core/r5rs.scm \
	lib/core/control.scm \
	lib/core/optargs.scm \
	lib/core/lists.scm \
	lib/core/records.scm \
	lib/core/conditions.scm \
	lib/core/bytevector-transcoders.scm \
	lib/core/hashtables.scm \
	lib/core/enums.scm \
	lib/core/struct.scm \
	lib/core/unicode.scm \
	lib/core/repl.scm \
	lib/core/fancy-repl.scm \
	lib/core/io/assistants.scm \
	lib/core/io/process.scm \
	lib/core/foreign.scm \
	lib/core/foreign-library.scm \
	lib/core/threading.scm

RNRS_SRCS := \
	lib/rnrs/base.scm \
	lib/rnrs/unicode.scm \
	lib/rnrs/bytevectors.scm \
	lib/rnrs/lists.scm \
	lib/rnrs/sorting.scm \
	lib/rnrs/control.scm \
	lib/rnrs/records/syntactic.scm \
	lib/rnrs/records/procedural.scm \
	lib/rnrs/records/inspection.scm \
	lib/rnrs/exceptions.scm \
	lib/rnrs/conditions.scm \
	lib/rnrs/io/ports.scm \
	lib/rnrs/io/simple.scm \
	lib/rnrs/files.scm \
	lib/rnrs/programs.scm \
	lib/rnrs/arithmetic/fixnums.scm \
	lib/rnrs/arithmetic/flonums.scm \
	lib/rnrs/arithmetic/bitwise.scm \
	lib/rnrs/syntax-case.scm \
	lib/rnrs/hashtables.scm \
	lib/rnrs/enums.scm \
	lib/rnrs/mutable-pairs.scm

CAPY_SRCS := \
	lib/capy/prelims.scm \
	lib/capy/pretty-print.scm \
	lib/capy/session.scm \
	lib/capy/deque.scm \
	lib/capy/channel.scm \
	lib/capy/term/private/ansi.scm \
	lib/capy/term/command.scm \
	lib/capy/term/style.scm \
	lib/capy/term/event-parser.scm \
	lib/capy/term/event.scm \
	lib/capy/term/terminal.scm \
	lib/capy/term/cursor.scm \
	lib/capy/term/clipboard.scm \
	lib/capy/term/tty.scm \
	lib/capy/term/line.scm \
	lib/capy/term.scm \
	lib/capy/generator.scm \
	lib/capy/future.scm \
	lib/capy/binary-heap.scm \
	lib/capy/intrusive/dlist.scm \
	lib/capy/intrusive/slist.scm \
	lib/capy/compiler/tree-il/terms.scm 	 \
	lib/capy/compiler/tree-il/fold.scm       \
	lib/capy/compiler/tree-il/primitives.scm \
	lib/capy/compiler/tree-il/resolve-free-vars.scm \
	lib/capy/compiler/tree-il/letrectify.scm \
	lib/capy/compiler/tree-il.scm				


SRFI_SRCS_R7RS := \
	lib/srfi/0.scm \
	lib/srfi/1.scm \
	lib/srfi/6.scm \
	lib/srfi/8.scm \
	lib/srfi/9.scm \
	lib/srfi/11.scm \
	lib/srfi/13.scm \
	lib/srfi/14.scm \
	lib/srfi/16.scm \
	lib/srfi/23.scm \
	lib/srfi/26.scm \
	lib/srfi/27.scm \
	lib/srfi/28.scm \
	lib/srfi/34.scm \
	lib/srfi/36.scm \
	lib/srfi/39.scm \
	lib/srfi/48.scm \
	lib/srfi/55.scm \
	lib/srfi/64.scm \
	lib/srfi/98.scm \
	lib/srfi/124.scm \
	lib/srfi/125.scm \
	lib/srfi/128.scm \
	lib/srfi/130.scm \
	lib/srfi/132.scm \
	lib/srfi/132/delndups.scm \
	lib/srfi/132/merge.scm \
	lib/srfi/132/select.scm \
	lib/srfi/132/sortfaster.scm \
	lib/srfi/132/sorting.scm \
	lib/srfi/132/sortp.scm \
	lib/srfi/132/vector-util.scm \
	lib/srfi/145.scm \
	lib/srfi/157.scm \
	lib/srfi/180.scm \
	lib/srfi/213.scm \
	lib/srfi/214.scm \
	lib/srfi/257.scm \
	lib/srfi/257/misc.scm \
	lib/srfi/259.scm

SRFI_SRCS_R6RS := $(patsubst lib/srfi/%.scm,lib/srfi/:%.scm,$(SRFI_SRCS_R7RS))
SRFI_SRCS := $(SRFI_SRCS_R7RS) $(SRFI_SRCS_R6RS)

R7RS_SRCS := \
	lib/scheme/base.scm \
	lib/scheme/case-lambda.scm \
	lib/scheme/char.scm \
	lib/scheme/comparator.scm \
	lib/scheme/complex.scm \
	lib/scheme/cxr.scm \
	lib/scheme/eval.scm \
	lib/scheme/file.scm \
	lib/scheme/inexact.scm \
	lib/scheme/hash-table.scm \
	lib/scheme/lazy.scm \
	lib/scheme/list.scm \
	lib/scheme/load.scm \
	lib/scheme/process-context.scm \
	lib/scheme/r5rs.scm \
	lib/scheme/read.scm \
	lib/scheme/repl.scm \
	lib/scheme/sort.scm \
	lib/scheme/time.scm \
	lib/scheme/write.scm
COMMON_SRCS := \
	lib/common/pregexp.scm

CAPY_ARGS_SRCS := \
	lib/capy/args/help/optional.scm \
	lib/capy/args/string.scm \
	lib/capy/args/option.scm \
	lib/capy/args/grammar.scm \
	lib/capy/args/results.scm \
	lib/capy/args/parser.scm \
	lib/capy/args/runner.scm \
	lib/capy/args.scm

# Map sources to outputs (given OUT).

BOOT_OUTS  = $(patsubst lib/boot/%.scm,$(OUT)/boot/%.$(COMPILED_SCM_EXT),$(BOOT_SRCS))
CORE_OUTS  = $(patsubst lib/core/%.scm,$(OUT)/core/%.$(COMPILED_SCM_EXT),$(CORE_SRCS)) $(OUT)/core.$(COMPILED_SCM_EXT)
RNRS_OUTS  = $(patsubst lib/rnrs/%.scm,$(OUT)/rnrs/%.$(COMPILED_SCM_EXT),$(RNRS_SRCS)) $(OUT)/rnrs.$(COMPILED_SCM_EXT)
CAPY_OUTS  = $(patsubst lib/capy/%.scm,$(OUT)/capy/%.$(COMPILED_SCM_EXT),$(CAPY_SRCS))
SRFI_OUTS  = $(patsubst lib/srfi/%.scm,$(OUT)/srfi/%.$(COMPILED_SCM_EXT),$(SRFI_SRCS))
R7RS_OUTS  = $(patsubst lib/scheme/%.scm,$(OUT)/scheme/%.$(COMPILED_SCM_EXT),$(R7RS_SRCS))
COMMON_OUTS = $(patsubst lib/common/%.scm,$(OUT)/common/%.$(COMPILED_SCM_EXT),$(COMMON_SRCS))
CAPY_ARGS_OUTS = $(patsubst lib/capy/%.scm,$(OUT)/capy/%.$(COMPILED_SCM_EXT),$(CAPY_ARGS_SRCS))

.PHONY: all help build build-runtime build-runtime-fhs build-runtime-portable install-scm test stage-0 stage-1 stage-2 \
	compile-cli compile-boot compile-core compile-rnrs compile-capy compile-capy-args compile-srfi compile-r7rs compile-common\
	install-portable dist-portable install install-cross dist-deb dist-rpm

all: build

help:
	@echo "Targets: build build-runtime build-runtime-fhs stage-0 stage-1 stage-2 install-portable dist-portable install"
	@echo "Packaging: dist-deb dist-rpm"
	@echo "Vars: PROFILE=release|debug TARGET=<triple> PORTABLE=1|0 PREFIX=<path> VERSION=<ver>"

install-cross:
	@if [ "$(CARGO_BIN)" = "cross" ]; then \
		cargo install cross --git https://github.com/cross-rs/cross; \
	else \
		echo "Using cargo, no need to install cross"; \
	fi

# Build the static Rust launcher.
build-runtime-bootstrap:
	@echo "Version: $(VERSION)"
	@echo "Target path: $(TARGET_PATH)"
	@echo "Building CapyScheme with profile '$(PROFILE)' for target '$(TARGET)'"
	$(CARGO_BIN) build --profile $(PROFILE) --target $(TARGET) -p capy --bin capy --features portable,bootstrap
	@echo "Build main capy launcher"
	mkdir -p bin
	cp $(TARGET_PATH)/capy bin/capy
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > bin/capyc
	chmod +x bin/capyc


# Aggregate build: runtime + full bootstrap chain.
build: 
	$(MAKE) PREFIX=$(PREFIX) stage-0
	$(MAKE) PREFIX=$(PREFIX) build-runtime
	$(MAKE) PREFIX=$(PREFIX) stage-1
	$(MAKE) PREFIX=$(PREFIX) stage-2
	@echo "Build complete: runtime and bootstrap stages 0-2"

build-runtime:
	$(CARGO_BIN) build --profile $(PROFILE) --target $(TARGET) -p capy --bin capy --features portable --no-default-features
	mkdir -p bin
	cp $(TARGET_PATH)/capy bin/capy
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > bin/capyc
	chmod +x bin/capyc

build-runtime-fhs: 
	CAPY_SYSROOT=$(PREFIX) $(CARGO_BIN) build --no-default-features --profile $(PROFILE) --target $(TARGET) -p capy --bin capy
	mkdir -p bin
	cp $(TARGET_PATH)/capy bin/capy-full
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy-full" --capy-compiler-entrypoint "$$@"' > bin/capyc-full
	chmod +x bin/capyc-full

build-runtime-portable: 
	$(CARGO_BIN) build --profile $(PROFILE) --target $(TARGET) -p capy --bin capy --features portable --no-default-features
	mkdir -p bin
	cp $(TARGET_PATH)/capy bin/capy-full
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy-full" --capy-compiler-entrypoint "$$@"' > bin/capyc-full
	chmod +x bin/capyc-full

install-scm:
	mkdir -p $(PREFIX)/capy/$(VERSION)
	rsync --checksum -r lib $(PREFIX)/capy/$(VERSION)

CAPY ?= stage-0/capy

test:
	@set -euo pipefail; \
	if [ "$(CAPY)" = "stage-0/capy" ] && [ ! -x "$(CAPY)" ]; then \
		$(MAKE) stage-0; \
	fi; \
	if [ ! -x "$(CAPY)" ]; then \
		echo "CAPY executable not found: $(CAPY)" >&2; \
		echo "Run 'make stage-0' or set CAPY=/path/to/capy" >&2; \
		exit 1; \
	fi; \
	status=0; \
	count=0; \
	for test in $$(find tests/lib -type f -name '*.scm' | sort); do \
		count=$$((count + 1)); \
		log=$$(mktemp); \
		echo "Running $$test"; \
		if ! $(CAPY_ENV) $(CAPY) -L lib --fresh-auto-compile -s "$$test" > "$$log" 2>&1; then \
			status=1; \
		fi; \
		cat "$$log"; \
		if grep -Eq "unexpected failures|unexpected successes" "$$log"; then \
			status=1; \
		fi; \
		rm -f "$$log"; \
	done; \
	if [ "$$count" -eq 0 ]; then \
		echo "No tests found under tests/lib" >&2; \
		exit 1; \
	fi; \
	exit "$$status"

compile-psyntax:
	$(call require_var,BIN)
	@echo "Compiling psyntax"
	$(CAPY_ENV) $(BIN) -s lib/boot/compile-psyntax.scm lib/boot/psyntax.scm lib/boot/psyntax-exp.scm


# Stage 0 of bootstrapping.
stage-0: build-runtime-bootstrap
	@echo "Creating stage-0 CapyScheme"
	mkdir -p stage-0
	cp $(TARGET_PATH)/capy stage-0/capy
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > stage-0/capyc
	chmod +x stage-0/capyc
	
	RUST_MIN_STACK=134217728 MMTK_PLAN=StickyImmix CAPY_GC_MAX_HEAP=8G XDG_CACHE_HOME="stage-0/cache" CAPY_LOAD_PATH=./lib stage-0/capy -L lib --fresh-auto-compile -c 42
	RUST_MIN_STACK=134217728 MMTK_PLAN=StickyImmix CAPY_GC_MAX_HEAP=8G XDG_CACHE_HOME="stage-0/cache" CAPY_LOAD_PATH=./lib stage-0/capy -L lib --fresh-auto-compile -c '(import (rnrs))'
	RUST_MIN_STACK=134217728 MMTK_PLAN=StickyImmix CAPY_GC_MAX_HEAP=8G XDG_CACHE_HOME="stage-0/cache" CAPY_LOAD_PATH=./lib stage-0/capy -L lib --fresh-auto-compile -c '(import (scheme base))'
	RUST_MIN_STACK=134217728 MMTK_PLAN=StickyImmix CAPY_GC_MAX_HEAP=8G XDG_CACHE_HOME="stage-0/cache" CAPY_LOAD_PATH=./lib stage-0/capy -L lib --fresh-auto-compile -c '(import (srfi 1))'
	RUST_MIN_STACK=134217728 MMTK_PLAN=StickyImmix CAPY_GC_MAX_HEAP=8G XDG_CACHE_HOME="stage-0/cache" CAPY_LOAD_PATH=./lib stage-0/capy -L lib --fresh-auto-compile -c '(import (srfi 13))'
ifeq ($(COMPILE_PSYNTAX),1)
	MMTK_PLAN=StickyImmix  XDG_CACHE_HOME="stage-0/cache" CAPY_LOAD_PATH=./lib stage-0/capy -L lib -s lib/boot/compile-psyntax.scm lib/boot/psyntax.scm lib/boot/psyntax-exp.scm
	RUST_MIN_STACK=134217728 MMTK_PLAN=StickyImmix  XDG_CACHE_HOME="stage-0/cache" CAPY_LOAD_PATH=./lib stage-0/capy -L lib --fresh-auto-compile -c '(import (scheme base) (rnrs))'
endif
	

	@echo "Stage-0 CapyScheme created in stage-0/ directory"

compile-all: compile-boot compile-core compile-rnrs compile-srfi compile-r7rs compile-capy-args compile-cli compile-capy compile-common

stage-1: 
	$(MAKE) $(foreach n,0 1 2 3 4 5 6 7 8 9,$(filter -j$n%,$(MAKEFLAGS))) compile-all COMPILER=stage-0/capyc OUT=stage-1/compiled 
	@echo "Creating stage-1 CapyScheme"
	mkdir -p stage-1
	cp stage-0/capy stage-1/capy
	cp stage-0/capyc stage-1/capyc

stage-2:
	@echo "Creating stage-2 CapyScheme"
	mkdir -p stage-2
	$(MAKE) $(foreach n,0 1 2 3 4 5 6 7 8 9,$(filter -j$n%,$(MAKEFLAGS))) compile-all COMPILER=stage-1/capyc OUT=stage-2/compiled
	cp stage-1/capy stage-2/capy
	cp stage-1/capyc stage-2/capyc

# -------------------------
# Per-file compilation rules
# -------------------------

# Default values for ad-hoc usage:
COMPILER ?=
OUT ?=

# Boot
$(OUT)/boot/%.$(COMPILED_SCM_EXT): lib/boot/%.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy" -L lib $<

# Core
$(OUT)/core/%.$(COMPILED_SCM_EXT): lib/core/%.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

$(OUT)/core.$(COMPILED_SCM_EXT): lib/core.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

# RNRS
$(OUT)/rnrs/%.$(COMPILED_SCM_EXT): lib/rnrs/%.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

$(OUT)/rnrs.$(COMPILED_SCM_EXT): lib/rnrs.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

# Capy
$(OUT)/capy/%.$(COMPILED_SCM_EXT): lib/capy/%.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

# SRFI
$(OUT)/srfi/%.$(COMPILED_SCM_EXT): lib/srfi/%.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

# R7RS
$(OUT)/scheme/%.$(COMPILED_SCM_EXT): lib/scheme/%.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

# Common
$(OUT)/common/%.$(COMPILED_SCM_EXT): lib/common/%.scm
	@mkdir -p $(dir $@)
	$(CAPY_ENV) $(COMPILER) --nobacktrace -o $@ -m "capy user" $<

$(OUT)/capy/args/option.$(COMPILED_SCM_EXT): $(OUT)/capy/args/help/optional.$(COMPILED_SCM_EXT)
$(OUT)/capy/args/grammar.$(COMPILED_SCM_EXT): $(OUT)/capy/args/option.$(COMPILED_SCM_EXT) $(OUT)/capy/args/help/optional.$(COMPILED_SCM_EXT) $(OUT)/capy/args/string.$(COMPILED_SCM_EXT)
$(OUT)/capy/args/results.$(COMPILED_SCM_EXT): $(OUT)/capy/args/grammar.$(COMPILED_SCM_EXT) $(OUT)/capy/args/option.$(COMPILED_SCM_EXT) $(OUT)/capy/args/help/optional.$(COMPILED_SCM_EXT)
$(OUT)/capy/args/parser.$(COMPILED_SCM_EXT): $(OUT)/capy/args/option.$(COMPILED_SCM_EXT) $(OUT)/capy/args/grammar.$(COMPILED_SCM_EXT) $(OUT)/capy/args/help/optional.$(COMPILED_SCM_EXT) $(OUT)/capy/args/results.$(COMPILED_SCM_EXT) $(OUT)/capy/args/string.$(COMPILED_SCM_EXT)
$(OUT)/capy/args/runner.$(COMPILED_SCM_EXT): $(OUT)/capy/args/grammar.$(COMPILED_SCM_EXT) $(OUT)/capy/args/parser.$(COMPILED_SCM_EXT) $(OUT)/capy/args/results.$(COMPILED_SCM_EXT) $(OUT)/capy/args/help/optional.$(COMPILED_SCM_EXT) $(OUT)/capy/args/string.$(COMPILED_SCM_EXT)
$(OUT)/capy/args.$(COMPILED_SCM_EXT): $(OUT)/capy/args/grammar.$(COMPILED_SCM_EXT) $(OUT)/capy/args/option.$(COMPILED_SCM_EXT) $(OUT)/capy/args/parser.$(COMPILED_SCM_EXT) $(OUT)/capy/args/results.$(COMPILED_SCM_EXT)

# -------------------------
# High-level compile targets
# -------------------------


compile-cli: compile-capy-args
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling CLI"
	@mkdir -p $(OUT)
	$(CAPY_ENV) $(COMPILER) -o $(OUT)/boot/cli.$(COMPILED_SCM_EXT) -m "capy" -L lib lib/boot/cli.scm
	$(CAPY_ENV) $(COMPILER) -o $(OUT)/boot.$(COMPILED_SCM_EXT) -m "capy" -L lib lib/boot.scm

compile-boot:
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling boot"
	@$(MAKE) $(BOOT_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "Boot libraries: $(words $(BOOT_SRCS)) files"

compile-core:
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling core"
	@$(MAKE) $(CORE_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "Core libraries: $(words $(CORE_SRCS)) files"

compile-rnrs:
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling rnrs"
	@$(MAKE) $(RNRS_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "RNRS libraries: $(words $(RNRS_SRCS)) files"

compile-capy: compile-capy-args
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling capy"
	@$(MAKE) $(CAPY_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "Capy libraries: $(words $(CAPY_SRCS)) scm"

compile-capy-args:
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling capy args"
	@$(MAKE) $(CAPY_ARGS_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "Capy args libraries: $(words $(CAPY_ARGS_SRCS)) scm"

compile-srfi:
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling srfi"
	@$(MAKE) $(SRFI_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "SRFI libraries: $(words $(SRFI_SRCS)) files"

compile-r7rs:
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling r7rs"
	@$(MAKE) $(R7RS_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "R7RS libraries: $(words $(R7RS_SRCS)) files"

compile-common:
	$(call require_var,COMPILER)
	$(call require_var,OUT)
	@echo "Compiling common"
	@$(MAKE) $(COMMON_OUTS) COMPILER=$(COMPILER) OUT=$(OUT)
	@echo "Common libraries: $(words $(COMMON_SRCS)) files"

# -------------------------
# Install / dist
# -------------------------

install-portable: build build-runtime-portable
	@echo "Installing CapyScheme to $(PREFIX)/capy/$(VERSION)"
	mkdir -p $(PREFIX)/capy/$(VERSION)/extensions
	rsync --checksum -r lib $(PREFIX)/capy/$(VERSION)
	cp bin/capy-full $(PREFIX)/capy/$(VERSION)/capy 
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > $(PREFIX)/capy/$(VERSION)/capyc
	chmod +x $(PREFIX)/capy/$(VERSION)/capyc
	ln -sf $(PREFIX)/capy/$(VERSION)/capy $(PREFIX)/capy/$(VERSION)/capy-$(VERSION)
	cp -r stage-2/compiled $(PREFIX)/capy/$(VERSION)/
	@echo "CapyScheme installed to $(PREFIX)/capy/$(VERSION)"
	@echo "Add $(PREFIX)/capy/$(VERSION) to your PATH to use CapyScheme"

# Produces a portable tar.gz archive without installing.
# Mirrors install-portable.
dist-portable: build build-runtime-portable
	set -euxo pipefail; \
	outdir=$${OUTDIR:-dist}; \
	stagedir=$${STAGEDIR:-stage-dist}; \
	outname=$${OUTNAME:-}; \
	mkdir -p "$$outdir"; \
	archive_name=$${outname:-capyscheme-$(VERSION)-$(TARGET).tar.gz}; \
	stage_root="$$stagedir"; \
	stage_prefix="$$stage_root"; \
	stage_install_dir="$$stage_prefix/capy/$(VERSION)"; \
	echo "Staging portable install into $$stage_install_dir"; \
	rm -rf "$$stage_root"; \
	mkdir -p "$$stage_install_dir/extensions"; \
	rsync --checksum -r lib "$$stage_install_dir"; \
	cp bin/capy-full "$$stage_install_dir/capy"; \
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > "$$stage_install_dir/capyc"; \
	chmod +x "$$stage_install_dir/capyc"; \
	cp -r stage-2/compiled "$$stage_install_dir/"; \
	cp LICENSE "$$stage_install_dir/"; \
	cp CHANGELOG.md "$$stage_install_dir/"; \
	echo "Creating $$outdir/$$archive_name"; \
	tar -C "$$stage_prefix" -czf "$$outdir/$$archive_name" "capy/$(VERSION)"; \
	echo "Wrote $$outdir/$$archive_name"

install: build
	$(MAKE) PREFIX=$(PREFIX) build-runtime-fhs 
	@echo "Installing CapyScheme (FHS) to $(PREFIX)"
	mkdir -p "$(PREFIX)/bin"; \
	mkdir -p "$(PREFIX)/lib"; \
	mkdir -p "$(PREFIX)/lib/capy/compiled"; \
	mkdir -p "$(PREFIX)/share/capy"; \
	cp -r lib "$(PREFIX)/share/capy/"; \
	cp bin/capy-full "$(PREFIX)/bin/capy"; \
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > "$(PREFIX)/bin/capyc"; \
	chmod +x "$(PREFIX)/bin/capyc"; \
	cp -r stage-2/compiled "$(PREFIX)/lib/capy/"; \
	echo "Installation complete."

# -------------------------
# Packaging (deb / rpm)
# -------------------------

# Output/staging knobs
DIST_DIR ?= dist
PKG_NAME ?= capyscheme
PKG_ROOT ?= stage-pkg

# Debian metadata
DEB_MAINTAINER ?= Adel Prokurov <adel.prokurov@gmail.com>
DEB_SECTION ?= devel
DEB_PRIORITY ?= optional

# RPM metadata
RPM_LICENSE ?= LGPL3+
RPM_SUMMARY ?= CapyScheme (runtime + compiler)

dist-deb: build
	@echo "Building .deb package in $(DIST_DIR)/"
	@# Build FHS runtime binaries (linked against /usr/lib)
	$(MAKE) PREFIX=/usr build-runtime-fhs
	rm -rf "$(PKG_ROOT)/deb"
	mkdir -p "$(PKG_ROOT)/deb/root/usr/bin" "$(PKG_ROOT)/deb/root/usr/lib" "$(PKG_ROOT)/deb/root/usr/lib/capy" "$(PKG_ROOT)/deb/root/usr/share/capy" "$(PKG_ROOT)/deb/DEBIAN" "$(DIST_DIR)"
	cp bin/capy-full "$(PKG_ROOT)/deb/root/usr/bin/capy"
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > "$(PKG_ROOT)/deb/root/usr/bin/capyc"
	chmod +x "$(PKG_ROOT)/deb/root/usr/bin/capyc"
	cp -r stage-2/compiled "$(PKG_ROOT)/deb/root/usr/lib/capy/"
	cp -r lib "$(PKG_ROOT)/deb/root/usr/share/capy/"
	installed_size_kb=$$(du -sk "$(PKG_ROOT)/deb/root/usr" | awk '{print $$1}'); \
	arch=$$(dpkg --print-architecture 2>/dev/null || echo amd64); \
	pkgver="$(VERSION)"; \
	deb_ver=$${pkgver#v}; \
	printf '%s\n' \
		"Package: $(PKG_NAME)" \
		"Version: $$deb_ver" \
		"Section: $(DEB_SECTION)" \
		"Priority: $(DEB_PRIORITY)" \
		"Architecture: $$arch" \
		"Maintainer: $(DEB_MAINTAINER)" \
		"Description: CapyScheme (runtime + compiler)" \
		" A Scheme implementation with a native runtime and compiler." \
		"Installed-Size: $$installed_size_kb" \
		> "$(PKG_ROOT)/deb/DEBIAN/control"
	chmod 0755 "$(PKG_ROOT)/deb/DEBIAN"
	arch=$$(dpkg --print-architecture 2>/dev/null || echo amd64); \
	pkgver="$(VERSION)"; \
	deb_ver=$${pkgver#v}; \
	if command -v fakeroot >/dev/null 2>&1; then \
		fakeroot dpkg-deb --build "$(PKG_ROOT)/deb" "$(DIST_DIR)/$(PKG_NAME)_$$deb_ver_$$arch.deb"; \
	else \
		dpkg-deb --build "$(PKG_ROOT)/deb" "$(DIST_DIR)/$(PKG_NAME)_$$deb_ver_$$arch.deb"; \
	fi
	@echo "Wrote $(DIST_DIR)/$(PKG_NAME)_$$(v='$(VERSION)'; echo $${v#v})_$$(dpkg --print-architecture 2>/dev/null || echo amd64).deb"

dist-rpm: build
	@echo "Building .rpm package in $(DIST_DIR)/"
	@# Build FHS runtime binaries (linked against /usr/lib)
	$(MAKE) PREFIX=/usr build-runtime-fhs
	rm -rf "$(PKG_ROOT)/rpm"
	mkdir -p \
		"$(PKG_ROOT)/rpm/root/usr/bin" "$(PKG_ROOT)/rpm/root/usr/lib" "$(PKG_ROOT)/rpm/root/usr/lib/capy" "$(PKG_ROOT)/rpm/root/usr/share/capy" \
		"$(PKG_ROOT)/rpm/rpmbuild/SPECS" "$(PKG_ROOT)/rpm/rpmbuild/SOURCES" "$(PKG_ROOT)/rpm/rpmbuild/BUILD" "$(PKG_ROOT)/rpm/rpmbuild/BUILDROOT" \
		"$(PKG_ROOT)/rpm/rpmbuild/RPMS" "$(PKG_ROOT)/rpm/rpmbuild/SRPMS" "$(PKG_ROOT)/rpm/rpmbuild/RPMDB" "$(DIST_DIR)"
	cp bin/capy-full "$(PKG_ROOT)/rpm/root/usr/bin/capy"
	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/capy" --capy-compiler-entrypoint "$$@"' > "$(PKG_ROOT)/rpm/root/usr/bin/capyc"
	chmod +x "$(PKG_ROOT)/rpm/root/usr/bin/capyc"
	cp -r stage-2/compiled "$(PKG_ROOT)/rpm/root/usr/lib/capy/"
	cp -r lib "$(PKG_ROOT)/rpm/root/usr/share/capy/"
	rm -rf "$(PKG_ROOT)/rpm/rpmbuild/BUILD/$(PKG_NAME)-$(VERSION)"
	mkdir -p "$(PKG_ROOT)/rpm/rpmbuild/BUILD/$(PKG_NAME)-$(VERSION)"
	cp -a "$(PKG_ROOT)/rpm/root/usr" "$(PKG_ROOT)/rpm/rpmbuild/BUILD/$(PKG_NAME)-$(VERSION)/"
	tar -C "$(PKG_ROOT)/rpm/rpmbuild/BUILD" -czf "$(PKG_ROOT)/rpm/rpmbuild/SOURCES/$(PKG_NAME)-$(VERSION).tar.gz" "$(PKG_NAME)-$(VERSION)"
	chlog_date=$$(date '+%a %b %d %Y'); \
	printf '%s\n' \
		"Name:           $(PKG_NAME)" \
		"Version:        $(VERSION)" \
		"Release:        1%{?dist}" \
		"Summary:        $(RPM_SUMMARY)" \
		"License:        $(RPM_LICENSE)" \
		"URL:            https://codeberg.org/playXE/capy" \
		"Source0:        %{name}-%{version}.tar.gz" \
		"" \
		"BuildArch:      %{_arch}" \
		"" \
		"%description" \
		"CapyScheme runtime and compiler." \
		"" \
		"%prep" \
		"%setup -q" \
		"" \
		"%build" \
		"# no-op" \
		"" \
		"%install" \
		"rm -rf %{buildroot}" \
		"mkdir -p %{buildroot}" \
		"cp -a usr %{buildroot}/" \
		"" \
		"%files" \
		"/usr/bin/capy" \
		"/usr/bin/capyc" \
		"/usr/lib/capy/compiled" \
		"/usr/share/capy/lib" \
		"" \
		"%changelog" \
		"* $$chlog_date $(RPM_SUMMARY)" \
		"- Automated build" \
		> "$(PKG_ROOT)/rpm/rpmbuild/SPECS/$(PKG_NAME).spec"
	rpmbuild -bb "$(PKG_ROOT)/rpm/rpmbuild/SPECS/$(PKG_NAME).spec" \
		--define "_topdir $(CURDIR)/$(PKG_ROOT)/rpm/rpmbuild" \
		--define "_dbpath $(CURDIR)/$(PKG_ROOT)/rpm/rpmbuild/RPMDB"
	find "$(PKG_ROOT)/rpm/rpmbuild/RPMS" -name "*.rpm" -maxdepth 2 -type f -print -exec cp -f {} "$(DIST_DIR)/" \;
	@echo "Wrote RPM(s) to $(DIST_DIR)/"

# Recursively walk *.scm files and call schemat
fmt:
	@echo "Formatting Scheme files..."
	@mkdir -p .fmt-tmp
	@find lib -type f -name "*.scm" | while read -r f; do \
		echo "  $$f"; \
		schemat < "$$f" > ".fmt-tmp/$$(basename "$$f")" && \
		[ -s ".fmt-tmp/$$(basename "$$f")" ] && \
		mv ".fmt-tmp/$$(basename "$$f")" "$$f"; \
	done
	@rmdir .fmt-tmp
	@echo "Formatting complete."
