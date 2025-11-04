profile := "release"
host-target := `rustc --print host-tuple`
target := host-target
target-dir := 'target'/target
target-path := if profile == "release" {
    target-dir/'release'
} else {
    target-dir/'debug'
}

install-prefix := '~/.local'
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


# Build the project with the specified profile and target
# by default, it builds in release mode for the host target
build portable:
    @echo "Version: {{version}}"
    @echo "Target path: {{target-path}}"
    @echo 'Building CapyScheme with profile '{{profile}}' for target '{{target}}''
    

    {{cargo-bin}} build --profile {{profile}} -Zbuild-std=std -Z build-std-features="optimize_for_size" --target {{target}} -p capy {{if portable == "true" { "--features portable" } else { "" } }}
    {{cargo-bin}} build --profile {{profile}} -Zbuild-std=std -Z build-std-features="optimize_for_size" --target {{target}} -p capy-driver {{if portable == "true" { "--features portable" } else { "" } }}
    {{cargo-bin}} build --profile {{profile}} -Zbuild-std=std -Z build-std-features="optimize_for_size" --target {{target}} -p capy-bin {{if portable == "true" { "--features portable" } else { "" } }}



# Perform portable installation of CapyScheme which installs the binary
# and all necessary resources to the specified install prefix
# by default, it installs to ~/.local/share/capy
install-portable: (build "true")
    @echo 'Installing CapyScheme to {{install-prefix}}/share/capy/{{version}}'
    @-mkdir -p {{install-prefix}}/share/capy/{{version}}
    @-mkdir -p {{install-prefix}}/share/capy/{{version}}/extensions
    rsync --checksum -r boot {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r core {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r core.scm {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r stdlib {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r batteries {{install-prefix}}/share/capy/{{version}}
    cp '{{target-path}}/capy' {{install-prefix}}/share/capy/{{version}}/
    cp '{{target-path}}/capyvm' {{install-prefix}}/share/capy/{{version}}/
    ln -sf {{install-prefix}}/share/capy/{{version}}/capy {{install-prefix}}/share/capy/{{version}}/capy-{{version}}
    cp {{target-path}}/libcapy.* {{install-prefix}}/share/capy/{{version}}/
    cp {{target-path}}/libcapy_*.* {{install-prefix}}/share/capy/{{version}}/extensions/
   
    @echo "CapyScheme installed to {{install-prefix}}/share/capy/{{version}}"
    @echo "Add {{install-prefix}}/share/capy/{{version}} to your PATH to use CapyScheme"

install-scm: 
    @-mkdir -p {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r boot {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r core {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r core.scm {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r stdlib {{install-prefix}}/share/capy/{{version}}
    rsync --checksum -r batteries {{install-prefix}}/share/capy/{{version}}
    
tar: (build "true")
    @echo 'Creating tarball for CapyScheme version {{version}}'
    @-mkdir -p dist
    @-rm -f dist/capy-{{version}}-{{target}}.tar.gz
    @-mkdir -p temp-dist/capy-{{version}}
    rsync --checksum -r boot temp-dist/capy-{{version}}/
    rsync --checksum -r core temp-dist/capy-{{version}}/
    rsync --checksum -r core.scm temp-dist/capy-{{version}}/
    rsync --checksum -r scheme temp-dist/capy-{{version}}/
    rsync --checksum -r batteries temp-dist/capy-{{version}}/
    cp '{{target-path}}/capy' temp-dist/capy-{{version}}/
    cp {{target-path}}/libcapy.* temp-dist/capy-{{version}}/
    tar -czf dist/capy-{{version}}-{{target}}.tar.gz -C temp-dist capy-{{version}}
    @-rm -rf temp-dist
    @echo 'Tarball created at dist/capy-{{version}}-{{target}}.tar.gz'