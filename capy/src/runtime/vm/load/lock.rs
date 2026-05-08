use std::{
    fs::{File, OpenOptions},
    io,
    path::{Path, PathBuf},
};

pub(super) struct CacheLock {
    file: File,
    path: PathBuf,
}

impl CacheLock {
    pub(super) fn acquire(path: impl AsRef<Path>) -> io::Result<Self> {
        let path = path.as_ref();
        let file = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .open(path)?;

        if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) } != 0 {
            return Err(io::Error::last_os_error());
        }

        Ok(Self {
            file,
            path: path.to_owned(),
        })
    }

    pub(super) fn for_artifact(path: impl AsRef<Path>) -> PathBuf {
        let path = path.as_ref();
        let mut lock_name = path
            .file_name()
            .map(|name| name.to_os_string())
            .unwrap_or_else(|| "artifact".into());
        lock_name.push(".lock");

        match path.parent() {
            Some(parent) => parent.join(lock_name),
            None => PathBuf::from(lock_name),
        }
    }
}

impl Drop for CacheLock {
    fn drop(&mut self) {
        if unsafe { libc::flock(self.file.as_raw_fd(), libc::LOCK_UN) } != 0 {
            log::warn!(
                "Failed to release cache lock {}: {}",
                self.path.display(),
                io::Error::last_os_error()
            );
        }
    }
}

#[cfg(unix)]
use std::os::fd::AsRawFd;

#[cfg(not(unix))]
compile_error!("capy load cache locking requires flock-compatible file descriptors");
