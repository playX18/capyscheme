//! Platform-specific linker invocation utilities.

use std::{
    path::{Path, PathBuf},
    process::Command,
};

pub struct Linker {
    path: PathBuf,
}

impl Linker {
    pub fn new() -> std::io::Result<Self> {
        let name = if cfg!(target_os = "macos") {
            "ld64.lld"
        } else {
            "ld"
        };
        let path = which::which(name).map_err(|e| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("No linker '{}' found in PATH: {}", name, e),
            )
        })?;
        Ok(Self { path })
    }

    pub fn link(&self, from: &Path, to: &Path) -> std::io::Result<()> {
        let mut cmd = Command::new(&self.path);
        rlpaths(&mut cmd);

        if cfg!(target_os = "macos") {
            let arch = if cfg!(target_arch = "x86_64") {
                "x86_64"
            } else if cfg!(target_arch = "aarch64") {
                "arm64"
            } else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Unsupported,
                    "Unsupported architecture for macOS linking",
                ));
            };
            cmd.arg("-dynamic")
                .arg("-dylib")
                .arg("-arch")
                .arg(arch)
                .arg("-platform_version")
                .arg("macos")
                .arg("16.0.0")
                .arg("26.0");
        } else {
            cmd.arg("--shared");
        }

        if !cmd
            .arg("-lcapy")
            .arg("-o")
            .arg(to)
            .arg(from)
            .status()?
            .success()
        {
            return Err(std::io::Error::other(
                "Linker command failed with status".to_string(),
            ));
        }
        if cfg!(target_os = "macos") {
            let dsymutil = which::which("dsymutil").map_err(|e| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("No dsymutil found in PATH: {}", e),
                )
            })?;
            if !Command::new(dsymutil).arg(to).status()?.success() {
                return Err(std::io::Error::other(
                    "dsymutil command failed with status".to_string(),
                ));
            }
        }

        Ok(())
    }
}

/// Given command add rlpath linker flags to it.
fn rlpaths(command: &mut Command) {
    if cfg!(feature = "portable") {
        if cfg!(target_os = "macos") {
            command.arg("-rpath").arg("@loader_path/");
        } else {
            command.arg("--rpath").arg("$ORIGIN/");
        }
        let exe = std::env::current_exe().expect("Failed to get current exe path");
        let exe = std::fs::read_link(&exe).unwrap_or(exe);
        let dir = exe.parent().expect("Failed to get exe parent directory");

        command.arg("-L").arg(dir);
    }

    if let Some(env) = std::env::var_os("LIBRARY_PATH") {
        command.arg("-L").arg(env);
    }

    if let Some(env) = std::env::var_os("LD_LIBRARY_PATH") {
        command.arg("-L").arg(env);
    }
}
