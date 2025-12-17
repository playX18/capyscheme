profile := "release"
host-target := `rustc --print host-tuple`
target := host-target
target-dir := 'target'/target
target-path := if profile == "release" {
    target-dir/'release'
} else {
    target-dir/'debug'
}
cc := 'clang'
install-prefix := '~/.local/share'
fhs-prefix := '/usr/local'
version := `cargo info capy | awk '/^version:/ {print $2}' | head -n 1`
cross := "false"
dynlib-ext := if os() == "macos" {
    "dylib"
} else if os() == "windows" {
    "dll"
} else {
    "so"
}


cargo-bin := if host-target != target {

    "cross"
} else {
    "cargo"
}

install_cross := if cargo-bin == "cross" {
    `cargo install cross --git https://github.com/cross-rs/cross`
} else {
    "Using cargo, no need to install cross"
}
rpath := if os() == "macos" {
    "@loader_path/"
} else {
    "\\$ORIGIN/"
}

rpath-fhs := if os() == "macos" {
    "@loader_path/../lib/"
} else {
    "\\$ORIGIN/../lib/"
}

arch := `rustc -vV | awk '/^host:/ {print $2}' | cut -d- -f1`


# Build the project with the specified profile and target
# by default, it builds in release mode for the host target
build portable:
    @echo "Version: {{version}}"
    @echo "Target path: {{target-path}}"
    @echo 'Building CapyScheme with profile '{{profile}}' for target '{{target}}''


    if [ "{{portable}}" = "true" ]; then \
        {{cargo-bin}} build --profile {{profile}} --target {{target}} -p capy --features portable; \
    else \
        CAPY_SYSROOT={{fhs-prefix}} {{cargo-bin}} build --no-default-features --profile {{profile}} --target {{target}} -p capy; \
    fi

    #@echo "Build boot binary & produce image"
    #{{cc}} bin/boot.c -o bin/boot -lcapy -L{{target-path}}
    #DYLD_FALLBACK_LIBRARY_PATH={{target-path}} LD_LIBRARY_PATH={{target-path}} LIBRARY_PATH={{target-path}} CAPY_LOAD_PATH=./lib ./bin/boot
    #rm bin/boot
    @echo "Build main capy binary"
    {{cc}} bin/capy.c -L{{target-path}} -o bin/capy -lcapy -Wl,-rpath,{{if portable == "true" { rpath } else { rpath-fhs }}}
    {{cc}} bin/capyc.c -L{{target-path}} -o bin/capyc -lcapy -Wl,-rpath,{{if portable == "true" { rpath } else { rpath-fhs }}}


install-scm:
    mkdir -p {{install-prefix}}/capy/{{version}}
    rsync --checksum -r lib {{install-prefix}}/capy/{{version}}


# FHS-style installation.
#
# Layout under {{fhs-prefix}}:
# - bin/capy
# - lib/libcapy.*
# - share/capy (Scheme stdlib)
#
# The runtime will pick this up when CAPY_SYSROOT={{fhs-prefix}} is set
# (or via CAPY_LOAD_PATH/CAPY_LOAD_COMPILED_PATH).
install-fhs: (build "false") 
    @echo 'Installing CapyScheme (FHS) to {{fhs-prefix}}'
    sudo mkdir -p {{fhs-prefix}}/bin
    sudo mkdir -p {{fhs-prefix}}/lib
    sudo mkdir -p {{fhs-prefix}}/lib/capy/compiled/{{arch}}
    sudo mkdir -p {{fhs-prefix}}/lib/capy/cache
    sudo mkdir -p {{fhs-prefix}}/share
    sudo mkdir -p {{fhs-prefix}}/share/capy
    sudo cp -r lib/ {{fhs-prefix}}/share/capy/
    sudo cp 'bin/capy' {{fhs-prefix}}/bin/capy
    sudo cp {{target-path}}/libcapy.* {{fhs-prefix}}/lib/
    @echo 'Precompiling boot libraries (populates compiled cache)'
    set -e; \
        tmp_cache=$(mktemp -d); \
        CAPY_LOAD_PATH="{{fhs-prefix}}/share/capy" XDG_CACHE_HOME="$$tmp_cache" LIBRARY_PATH="{{fhs-prefix}}/lib" LD_LIBRARY_PATH="{{fhs-prefix}}/lib" {{fhs-prefix}}/bin/capy --fresh-auto-compile -c 42; \
        echo 'Copying full cache into {{fhs-prefix}}/lib/capy/cache'; \
        sudo cp -r "$$tmp_cache"/capy/cache/ "{{fhs-prefix}}/lib/capy/cache/"; \
        rm -rf "$$tmp_cache"
    @echo 'Done.'


tar: (build "true")
    @echo 'Creating tarball for CapyScheme version {{version}}'
    mkdir -p dist
    rm -f dist/capy-{{version}}-{{target}}.tar.gz
    mkdir -p temp-dist/capy-{{version}}
    rsync --checksum -r boot temp-dist/capy-{{version}}/
    rsync --checksum -r core temp-dist/capy-{{version}}/
    rsync --checksum -r core.scm temp-dist/capy-{{version}}/
    rsync --checksum -r scheme temp-dist/capy-{{version}}/
    rsync --checksum -r batteries temp-dist/capy-{{version}}/
    cp '{{target-path}}/capy' temp-dist/capy-{{version}}/
    cp {{target-path}}/libcapy.* temp-dist/capy-{{version}}/
    tar -czf dist/capy-{{version}}-{{target}}.tar.gz -C temp-dist capy-{{version}}
    rm -rf temp-dist
    @echo 'Tarball created at dist/capy-{{version}}-{{target}}.tar.gz'

# Stage 0 of bootstrapping CapyScheme: 
# Create a portable CapyScheme binary and build `capyc` + `capy` binaries. Use them to 
# compile the rest of the system in stage 1.
stage-0: (build "true")
    @echo "Creating stage-0 CapyScheme"
    mkdir -p stage-0
    set -e; \
        {{cc}} bin/capy.c -L{{target-path}} -o stage-0/capy -lcapy -Wl,-rpath,{{rpath}}; \
        {{cc}} bin/capyc.c -L{{target-path}} -o stage-0/capyc -lcapy -Wl,-rpath,{{rpath}}; \
        XDG_CACHE_HOME="stage-0/cache" \
        CAPY_LOAD_PATH=./lib LD_LIBRARY_PATH={{target-path}} \
        DYLD_FALLBACK_LIBRARY_PATH={{target-path}} \
        stage-0/capy -L lib --fresh-auto-compile -c 42; 
    @echo "Stage-0 CapyScheme created in stage-0/ directory"

stage-1:\
    (stage-0) \
    (compile-boot "stage-0/capyc" "stage-1/compiled") \
    (compile-core "stage-0/capyc" "stage-1/compiled") \
    (compile-rnrs "stage-0/capyc" "stage-1/compiled") \
    (compile-srfi "stage-0/capyc" "stage-1/compiled") \
    (compile-r7rs "stage-0/capyc" "stage-1/compiled") \
    (compile-cli  "stage-0/capyc" "stage-1/compiled") \
    (compile-capy "stage-0/capyc" "stage-1/compiled")
    @echo "Creating stage-1 CapyScheme"
    cp stage-0/capy stage-1/capy
    cp stage-0/capyc stage-1/capyc 

# Now that stage-1 is built, build stage-2 with usage of stage-1 compiler
stage-2: \
    (stage-1) \
    (compile-boot "stage-1/capyc" "stage-2/compiled") \
    (compile-core "stage-1/capyc" "stage-2/compiled") \
    (compile-rnrs "stage-1/capyc" "stage-2/compiled") \
    (compile-srfi "stage-1/capyc" "stage-2/compiled") \
    (compile-r7rs "stage-1/capyc" "stage-2/compiled") \
    (compile-cli  "stage-1/capyc" "stage-2/compiled") \
    (compile-capy "stage-1/capyc" "stage-2/compiled") 
    @echo "Creating stage-2 CapyScheme"
    cp stage-1/capy stage-2/capy
    cp stage-1/capyc stage-2/capyc


compile-cli compiler out $XDG_CACHE_HOME="stage-0/cache" $CAPY_LOAD_PATH="./lib" $LD_LIBRARY_PATH=target-path $DYLD_FALLBACK_LIBRARY_PATH=target-path:
    @echo "Compiling CLI tools using {{compiler}}"
    

    {{compiler}} -o {{out}}/boot/cli.{{dynlib-ext}} -m "capy" -L lib lib/boot/cli.scm
    {{compiler}} -o {{out}}/boot.{{dynlib-ext}} -m "capy" -L lib lib/boot.scm

compile-boot compiler out $XDG_CACHE_HOME="stage-0/cache" $CAPY_LOAD_PATH="./lib" $LD_LIBRARY_PATH=target-path $DYLD_FALLBACK_LIBRARY_PATH=target-path: 
    @echo "Compiling boot libraries using {{compiler}}"

    mkdir -p {{out}}/boot/
    #!/usr/bin/env -S parallel --shebang --ungroup --jobs {{ num_cpus() }}
    {{compiler}} -o {{out}}/boot/prim.{{dynlib-ext}} -m "capy" -L lib lib/boot/prim.scm
    {{compiler}} -o {{out}}/boot/control.{{dynlib-ext}} -m "capy" -L lib lib/boot/control.scm
    {{compiler}} -o {{out}}/boot/modules.{{dynlib-ext}} -m "capy" -L lib lib/boot/modules.scm
    {{compiler}} -o {{out}}/boot/records.{{dynlib-ext}} -m "capy" -L lib lib/boot/records.scm
    {{compiler}} -o {{out}}/boot/conditions.{{dynlib-ext}} -m "capy" -L lib lib/boot/conditions.scm
    {{compiler}} -o {{out}}/boot/exceptions.{{dynlib-ext}} -m "capy" -L lib lib/boot/exceptions.scm
    {{compiler}} -o {{out}}/boot/expand.{{dynlib-ext}} -m "capy" -L lib lib/boot/expand.scm
    {{compiler}} -o {{out}}/boot/interpreter.{{dynlib-ext}} -m "capy" -L lib lib/boot/interpreter.scm
    {{compiler}} -o {{out}}/boot/enums.{{dynlib-ext}} -m "capy" -L lib lib/boot/enums.scm
    {{compiler}} -o {{out}}/boot/sys.{{dynlib-ext}} -m "capy" -L lib lib/boot/sys.scm
    {{compiler}} -o {{out}}/boot/osdep.{{dynlib-ext}} -m "capy" -L lib lib/boot/osdep.scm
    {{compiler}} -o {{out}}/boot/iosys.{{dynlib-ext}} -m "capy" -L lib lib/boot/iosys.scm
    {{compiler}} -o {{out}}/boot/portio.{{dynlib-ext}} -m "capy" -L lib lib/boot/portio.scm
    {{compiler}} -o {{out}}/boot/bytevectorio.{{dynlib-ext}} -m "capy" -L lib lib/boot/bytevectorio.scm
    {{compiler}} -o {{out}}/boot/fileio.{{dynlib-ext}} -m "capy" -L lib lib/boot/fileio.scm
    {{compiler}} -o {{out}}/boot/conio.{{dynlib-ext}} -m "capy" -L lib lib/boot/conio.scm
    {{compiler}} -o {{out}}/boot/stringio.{{dynlib-ext}} -m "capy" -L lib lib/boot/stringio.scm
    {{compiler}} -o {{out}}/boot/stdio.{{dynlib-ext}} -m "capy" -L lib lib/boot/stdio.scm
    {{compiler}} -o {{out}}/boot/utf16.{{dynlib-ext}} -m "capy" -L lib lib/boot/utf16.scm
    {{compiler}} -o {{out}}/boot/customio.{{dynlib-ext}} -m "capy" -L lib lib/boot/customio.scm
    {{compiler}} -o {{out}}/boot/print.{{dynlib-ext}} -m "capy" -L lib lib/boot/print.scm
    {{compiler}} -o {{out}}/boot/format.{{dynlib-ext}} -m "capy" -L lib lib/boot/format.scm
    {{compiler}} -o {{out}}/boot/log.{{dynlib-ext}} -m "capy" -L lib lib/boot/log.scm
    {{compiler}} -o {{out}}/boot/match-syntax.{{dynlib-ext}} -m "capy" -L lib lib/boot/match-syntax.scm
    {{compiler}} -o {{out}}/boot/psyntax.{{dynlib-ext}} -m "capy" -L lib lib/boot/psyntax.scm
    {{compiler}} -o {{out}}/boot/str2num.{{dynlib-ext}} -m "capy" -L lib lib/boot/str2num.scm
    {{compiler}} -o {{out}}/boot/num2str.{{dynlib-ext}} -m "capy" -L lib lib/boot/num2str.scm
    {{compiler}} -o {{out}}/boot/reader.{{dynlib-ext}} -m "capy" -L lib lib/boot/reader.scm
    {{compiler}} -o {{out}}/boot/eval.{{dynlib-ext}} -m "capy" -L lib lib/boot/eval.scm
    {{compiler}} -o {{out}}/boot/base.{{dynlib-ext}} -m "capy" -L lib lib/boot/base.scm
    {{compiler}} -o {{out}}/boot/libraries.{{dynlib-ext}} -m "capy" -L lib lib/boot/libraries.scm
    {{compiler}} -o {{out}}/boot/match.{{dynlib-ext}} -m "capy" -L lib lib/boot/match.scm
    



compile-core compiler out $XDG_CACHE_HOME="stage-0/cache" $CAPY_LOAD_PATH="./lib" $LD_LIBRARY_PATH=target-path $DYLD_FALLBACK_LIBRARY_PATH=target-path: 
    @echo "Compiling core libraries using {{compiler}}"

    mkdir -p {{out}}/core/io/
    #!/usr/bin/env -S parallel --shebang --ungroup --jobs {{ num_cpus() }}
    {{compiler}} -o {{out}}/core/parameters.{{dynlib-ext}} -m "capy user" lib/core/parameters.scm 
    {{compiler}} -o {{out}}/core/io.{{dynlib-ext}} -m "capy user" lib/core/io.scm
    {{compiler}} -o {{out}}/core/files.{{dynlib-ext}} -m "capy user" lib/core/files.scm
    {{compiler}} -o {{out}}/core/exceptions.{{dynlib-ext}} -m "capy user" lib/core/exceptions.scm
    {{compiler}} -o {{out}}/core/arithmetic.{{dynlib-ext}} -m "capy user" lib/core/arithmetic.scm
    {{compiler}} -o {{out}}/core/sorting.{{dynlib-ext}} -m "capy user" lib/core/sorting.scm
    {{compiler}} -o {{out}}/core/bytevectors.{{dynlib-ext}} -m "capy user" lib/core/bytevectors.scm
   
    {{compiler}} -o {{out}}/core/r5rs.{{dynlib-ext}} -m "capy user" lib/core/r5rs.scm
    {{compiler}} -o {{out}}/core/control.{{dynlib-ext}} -m "capy user" lib/core/control.scm
    {{compiler}} -o {{out}}/core/optargs.{{dynlib-ext}} -m "capy user" lib/core/optargs.scm
    {{compiler}} -o {{out}}/core/lists.{{dynlib-ext}} -m "capy user" lib/core/lists.scm
    {{compiler}} -o {{out}}/core/records.{{dynlib-ext}} -m "capy user" lib/core/records.scm
    {{compiler}} -o {{out}}/core/conditions.{{dynlib-ext}} -m "capy user" lib/core/conditions.scm
    {{compiler}} -o {{out}}/core/bytevector-transcoders.{{dynlib-ext}} -m "capy user" lib/core/bytevector-transcoders.scm
    {{compiler}} -o {{out}}/core/hashtables.{{dynlib-ext}} -m "capy user" lib/core/hashtables.scm
    {{compiler}} -o {{out}}/core/enums.{{dynlib-ext}} -m "capy user" lib/core/enums.scm
    {{compiler}} -o {{out}}/core/struct.{{dynlib-ext}} -m "capy user" lib/core/struct.scm
    {{compiler}} -o {{out}}/core/unicode.{{dynlib-ext}} -m "capy user" lib/core/unicode.scm
    {{compiler}} -o {{out}}/core/repl.{{dynlib-ext}} -m "capy user" lib/core/repl.scm
    {{compiler}} -o {{out}}/core/io/assistants.{{dynlib-ext}} -m "capy user" lib/core/io/assistants.scm
    {{compiler}} -o {{out}}/core.{{dynlib-ext}} -m "capy user" lib/core.scm

compile-rnrs compiler out $XDG_CACHE_HOME="stage-0/cache" $CAPY_LOAD_PATH="./lib" $LD_LIBRARY_PATH=target-path $DYLD_FALLBACK_LIBRARY_PATH=target-path:
    @echo "Compiling rnrs libraries using {{compiler}}"

    mkdir -p {{out}}/rnrs/
    mkdir -p {{out}}/rnrs/records/
    mkdir -p {{out}}/rnrs/io/
    mkdir -p {{out}}/rnrs/arithmetic/
    #!/usr/bin/env -S parallel --shebang --ungroup --jobs {{ num_cpus() }}
    {{compiler}} -o {{out}}/rnrs/base.{{dynlib-ext}} -m "capy user" lib/rnrs/base.scm
    {{compiler}} -o {{out}}/rnrs/unicode.{{dynlib-ext}} -m "capy user" lib/rnrs/unicode.scm
    {{compiler}} -o {{out}}/rnrs/bytevectors.{{dynlib-ext}} -m "capy user" lib/rnrs/bytevectors.scm
    {{compiler}} -o {{out}}/rnrs/lists.{{dynlib-ext}} -m "capy user" lib/rnrs/lists.scm
    {{compiler}} -o {{out}}/rnrs/sorting.{{dynlib-ext}} -m "capy user" lib/rnrs/sorting.scm
    {{compiler}} -o {{out}}/rnrs/control.{{dynlib-ext}} -m "capy user" lib/rnrs/control.scm
    {{compiler}} -o {{out}}/rnrs/records/syntactic.{{dynlib-ext}} -m "capy user" lib/rnrs/records/syntactic.scm
    {{compiler}} -o {{out}}/rnrs/records/procedural.{{dynlib-ext}} -m "capy user" lib/rnrs/records/procedural.scm
    {{compiler}} -o {{out}}/rnrs/records/inspection.{{dynlib-ext}} -m "capy user" lib/rnrs/records/inspection.scm
    {{compiler}} -o {{out}}/rnrs/exceptions.{{dynlib-ext}} -m "capy user" lib/rnrs/exceptions.scm
    {{compiler}} -o {{out}}/rnrs/conditions.{{dynlib-ext}} -m "capy user" lib/rnrs/conditions.scm
    {{compiler}} -o {{out}}/rnrs/io/ports.{{dynlib-ext}} -m "capy user" lib/rnrs/io/ports.scm
    {{compiler}} -o {{out}}/rnrs/io/simple.{{dynlib-ext}} -m "capy user" lib/rnrs/io/simple.scm
    {{compiler}} -o {{out}}/rnrs/files.{{dynlib-ext}} -m "capy user" lib/rnrs/files.scm
    {{compiler}} -o {{out}}/rnrs/programs.{{dynlib-ext}} -m "capy user" lib/rnrs/programs.scm
    {{compiler}} -o {{out}}/rnrs/arithmetic/fixnums.{{dynlib-ext}} -m "capy user" lib/rnrs/arithmetic/fixnums.scm
    {{compiler}} -o {{out}}/rnrs/arithmetic/flonums.{{dynlib-ext}} -m "capy user" lib/rnrs/arithmetic/flonums.scm
    {{compiler}} -o {{out}}/rnrs/arithmetic/bitwise.{{dynlib-ext}} -m "capy user" lib/rnrs/arithmetic/bitwise.scm
    {{compiler}} -o {{out}}/rnrs/syntax-case.{{dynlib-ext}} -m "capy user" lib/rnrs/syntax-case.scm
    {{compiler}} -o {{out}}/rnrs/hashtables.{{dynlib-ext}} -m "capy user" lib/rnrs/hashtables.scm
    {{compiler}} -o {{out}}/rnrs/enums.{{dynlib-ext}} -m "capy user" lib/rnrs/enums.scm
    {{compiler}} -o {{out}}/rnrs.{{dynlib-ext}} -m "capy user" lib/rnrs.scm

compile-capy compiler out $XDG_CACHE_HOME="stage-0/cache" $CAPY_LOAD_PATH="./lib" $LD_LIBRARY_PATH=target-path $DYLD_FALLBACK_LIBRARY_PATH=target-path:
    @echo "Compiling capy libraries using {{compiler}}"

    mkdir -p {{out}}/capy/compiler/
    mkdir -p {{out}}/capy/args/
    #!/usr/bin/env -S parallel --shebang --ungroup --jobs {{ num_cpus() }}
    {{compiler}} -o {{out}}/capy/pretty-print.{{dynlib-ext}} -m "capy user" lib/capy/pretty-print.sls 
    {{compiler}} -o {{out}}/capy/compiler/tree-il.{{dynlib-ext}} -m "capy user" lib/capy/compiler/tree-il.scm
    {{compiler}} -o {{out}}/capy/args/argparser.{{dynlib-ext}} -m "capy user" lib/capy/args/argparser.sls
    {{compiler}} -o {{out}}/capy/args/option.{{dynlib-ext}} -m "capy user" lib/capy/args/option.sls
    {{compiler}} -o {{out}}/capy/args/parser.{{dynlib-ext}} -m "capy user" lib/capy/args/parser.sls
    {{compiler}} -o {{out}}/capy/args/results.{{dynlib-ext}} -m "capy user" lib/capy/args/results.sls
    {{compiler}} -o {{out}}/capy/args.{{dynlib-ext}} -m "capy user" lib/capy/args.sls


# Compiles SRFI libraries using specified compiler.
compile-srfi compiler out $XDG_CACHE_HOME="stage-0/cache" $CAPY_LOAD_PATH="./lib" $LD_LIBRARY_PATH=target-path $DYLD_FALLBACK_LIBRARY_PATH=target-path:
    @echo "Compiling SRFI libraries using {{compiler}}"
    mkdir -p {{out}}/srfi/
    mkdir -p {{out}}/srfi/srfi-132
    #!/usr/bin/env -S parallel --shebang --ungroup --jobs {{ num_cpus() }}
    {{compiler}} -o {{out}}/srfi/srfi-0.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-0.scm
    {{compiler}} -o {{out}}/srfi/srfi-1.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-1.scm
    {{compiler}} -o {{out}}/srfi/srfi-6.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-6.scm
    {{compiler}} -o {{out}}/srfi/srfi-8.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-8.scm
    {{compiler}} -o {{out}}/srfi/srfi-9.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-9.scm
    {{compiler}} -o {{out}}/srfi/srfi-11.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-11.scm
    {{compiler}} -o {{out}}/srfi/srfi-13.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-13.scm
    {{compiler}} -o {{out}}/srfi/srfi-14.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-14.scm
    {{compiler}} -o {{out}}/srfi/srfi-16.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-16.scm
    {{compiler}} -o {{out}}/srfi/srfi-23.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-23.scm
    {{compiler}} -o {{out}}/srfi/srfi-27.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-27.scm
    {{compiler}} -o {{out}}/srfi/srfi-28.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-28.scm
    {{compiler}} -o {{out}}/srfi/srfi-34.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-34.scm
    {{compiler}} -o {{out}}/srfi/srfi-36.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-36.scm
    {{compiler}} -o {{out}}/srfi/srfi-39.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-39.scm
    {{compiler}} -o {{out}}/srfi/srfi-48.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-48.scm
    {{compiler}} -o {{out}}/srfi/srfi-55.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-55.scm
    {{compiler}} -o {{out}}/srfi/srfi-64.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-64.scm
    {{compiler}} -o {{out}}/srfi/srfi-98.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-98.scm
    {{compiler}} -o {{out}}/srfi/srfi-124.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-124.scm
    {{compiler}} -o {{out}}/srfi/srfi-125.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-125.scm
    {{compiler}} -o {{out}}/srfi/srfi-128.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-128.scm
    {{compiler}} -o {{out}}/srfi/srfi-130.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-130.scm
    {{compiler}} -o {{out}}/srfi/srfi-132.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132.scm
    
    {{compiler}} -o {{out}}/srfi/srfi-132/delndups.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132/delndups.scm
    {{compiler}} -o {{out}}/srfi/srfi-132/merge.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132/merge.scm
    {{compiler}} -o {{out}}/srfi/srfi-132/select.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132/select.scm
    {{compiler}} -o {{out}}/srfi/srfi-132/sortfaster.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132/sortfaster.scm
    {{compiler}} -o {{out}}/srfi/srfi-132/sorting.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132/sorting.scm
    {{compiler}} -o {{out}}/srfi/srfi-132/sortp.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132/sortp.scm
    {{compiler}} -o {{out}}/srfi/srfi-132/vector-util.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-132/vector-util.scm

    {{compiler}} -o {{out}}/srfi/srfi-157.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-157.scm
    {{compiler}} -o {{out}}/srfi/srfi-180.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-180.scm
    {{compiler}} -o {{out}}/srfi/srfi-259.{{dynlib-ext}} -m "capy user" lib/srfi/srfi-259.scm

compile-r7rs compiler out $XDG_CACHE_HOME="stage-0/cache" $CAPY_LOAD_PATH="./lib" $LD_LIBRARY_PATH=target-path $DYLD_FALLBACK_LIBRARY_PATH=target-path:
    @echo "Compiling R7RS libraries using {{compiler}}"

    mkdir -p {{out}}/scheme 

    #!/usr/bin/env -S parallel --shebang --ungroup --jobs {{ num_cpus() }}
    {{compiler}} -o {{out}}/scheme/base.{{dynlib-ext}} -m "capy user" lib/scheme/base.scm
    {{compiler}} -o {{out}}/scheme/case-lambda.{{dynlib-ext}} -m "capy user" lib/scheme/case-lambda.scm
    {{compiler}} -o {{out}}/scheme/char.{{dynlib-ext}} -m "capy user" lib/scheme/char.scm
    {{compiler}} -o {{out}}/scheme/comparator.{{dynlib-ext}} -m "capy user" lib/scheme/comparator.scm
    {{compiler}} -o {{out}}/scheme/complex.{{dynlib-ext}} -m "capy user" lib/scheme/complex.scm
    {{compiler}} -o {{out}}/scheme/cxr.{{dynlib-ext}} -m "capy user" lib/scheme/cxr.scm
    {{compiler}} -o {{out}}/scheme/eval.{{dynlib-ext}} -m "capy user" lib/scheme/eval.scm
    {{compiler}} -o {{out}}/scheme/file.{{dynlib-ext}} -m "capy user" lib/scheme/file.scm
    {{compiler}} -o {{out}}/scheme/inexact.{{dynlib-ext}} -m "capy user" lib/scheme/inexact.scm
    {{compiler}} -o {{out}}/scheme/hash-table.{{dynlib-ext}} -m "capy user" lib/scheme/hash-table.scm
    {{compiler}} -o {{out}}/scheme/lazy.{{dynlib-ext}} -m "capy user" lib/scheme/lazy.scm
    {{compiler}} -o {{out}}/scheme/list.{{dynlib-ext}} -m "capy user" lib/scheme/list.scm
    {{compiler}} -o {{out}}/scheme/load.{{dynlib-ext}} -m "capy user" lib/scheme/load.scm
    {{compiler}} -o {{out}}/scheme/process-context.{{dynlib-ext}} -m "capy user" lib/scheme/process-context.scm
    {{compiler}} -o {{out}}/scheme/r5rs.{{dynlib-ext}} -m "capy user" lib/scheme/r5rs.scm
    {{compiler}} -o {{out}}/scheme/read.{{dynlib-ext}} -m "capy user" lib/scheme/read.scm
    {{compiler}} -o {{out}}/scheme/repl.{{dynlib-ext}} -m "capy user" lib/scheme/repl.scm
    {{compiler}} -o {{out}}/scheme/sort.{{dynlib-ext}} -m "capy user" lib/scheme/sort.scm
    {{compiler}} -o {{out}}/scheme/time.{{dynlib-ext}} -m "capy user" lib/scheme/time.scm
    {{compiler}} -o {{out}}/scheme/write.{{dynlib-ext}} -m "capy user" lib/scheme/write.scm

# Default installation method: portable binary.
# 
# Builds stage-0, stage-1, stage-2 and installs stage-2 to
# {{install-prefix}}/capy/{{version}}.
install-portable: (stage-2)
    @echo 'Installing CapyScheme to {{install-prefix}}/capy/{{version}}'
    mkdir -p {{install-prefix}}/capy/{{version}}
    mkdir -p {{install-prefix}}/capy/{{version}}/extensions
    rsync --checksum -r lib {{install-prefix}}/capy/{{version}}
    cp 'stage-2/capy' {{install-prefix}}/capy/{{version}}/
    cp 'stage-2/capyc' {{install-prefix}}/capy/{{version}}/
    ln -sf {{install-prefix}}/capy/{{version}}/capy {{install-prefix}}/capy/{{version}}/capy-{{version}}
    cp {{target-path}}/libcapy.* {{install-prefix}}/capy/{{version}}/
    cp -r stage-2/compiled {{install-prefix}}/capy/{{version}}/

    @echo "CapyScheme installed to {{install-prefix}}/capy/{{version}}"
    @echo "Add {{install-prefix}}/capy/{{version}} to your PATH to use CapyScheme"


SUDO :=`(id -u | grep -q '^0$$' && echo '' || echo 'sudo ')`

# Install using FHS layout
install: (stage-2) (build "false") 
    @echo 'Installing CapyScheme (FHS) to {{fhs-prefix}}'
    #!/usr/bin/env bash
    set -e
    
    {{SUDO}} mkdir -p {{fhs-prefix}}/bin
    {{SUDO}} mkdir -p {{fhs-prefix}}/lib
    {{SUDO}} mkdir -p {{fhs-prefix}}/lib/capy/compiled
    {{SUDO}} mkdir -p {{fhs-prefix}}/share/capy 
    {{SUDO}} cp -r lib/ {{fhs-prefix}}/share/capy/
    {{SUDO}} cp 'stage-2/capy' {{fhs-prefix}}/bin/capy
    {{SUDO}} cp 'stage-2/capyc' {{fhs-prefix}}/bin/capyc
    {{SUDO}} cp {{target-path}}/libcapy.* {{fhs-prefix}}/lib/
    {{SUDO}} cp -r stage-2/compiled {{fhs-prefix}}/lib/capy/

    @echo "Installation complete."