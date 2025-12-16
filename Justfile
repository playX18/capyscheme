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
    



# Perform portable installation of CapyScheme which installs the binary
# and all necessary resources to the specified install prefix
# by default, it installs to ~/.local/capy
install-portable: (build "true")
    @echo 'Installing CapyScheme to {{install-prefix}}/capy/{{version}}'
    mkdir -p {{install-prefix}}/capy/{{version}}
    mkdir -p {{install-prefix}}/capy/{{version}}/extensions
    rsync --checksum -r lib {{install-prefix}}/capy/{{version}}
    cp 'bin/capy' {{install-prefix}}/capy/{{version}}/
    ln -sf {{install-prefix}}/capy/{{version}}/capy {{install-prefix}}/capy/{{version}}/capy-{{version}}
    cp {{target-path}}/libcapy.* {{install-prefix}}/capy/{{version}}/
    #cp capy.heap {{install-prefix}}/capy/{{version}}/
    #rm capy.heap
    rm bin/capy



    @echo "CapyScheme installed to {{install-prefix}}/capy/{{version}}"
    @echo "Add {{install-prefix}}/capy/{{version}} to your PATH to use CapyScheme"

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
        XDG_CACHE_HOME="$$tmp_cache" LIBRARY_PATH="{{fhs-prefix}}/lib" LD_LIBRARY_PATH="{{fhs-prefix}}/lib" {{fhs-prefix}}/bin/capy --fresh-auto-compile -c 42; \
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
