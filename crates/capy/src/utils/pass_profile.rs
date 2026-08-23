use std::{
    fmt::Write,
    sync::LazyLock,
    time::{Duration, Instant},
};

use crate::utils::flags;

static ENABLED: LazyLock<bool> = LazyLock::new(flags::profile_passes);

pub fn enabled() -> bool {
    *ENABLED
}

pub(crate) fn format_line<'a>(
    phase: &str,
    fields: impl IntoIterator<Item = (&'a str, &'a str)>,
    elapsed: Duration,
) -> String {
    let mut line = format!(";; PERF (capy) phase={phase}");
    for (key, value) in fields {
        let _ = write!(line, " {key}={value}");
    }
    let _ = write!(line, " elapsed_ms={:.3}", elapsed.as_secs_f64() * 1000.0);
    line
}

pub struct ProfileScope {
    phase: &'static str,
    start: Option<Instant>,
    fields: Vec<(&'static str, String)>,
}

impl ProfileScope {
    pub fn new(phase: &'static str) -> Self {
        Self {
            phase,
            start: enabled().then(Instant::now),
            fields: Vec::new(),
        }
    }

    pub fn is_enabled(&self) -> bool {
        self.start.is_some()
    }

    pub fn field(&mut self, key: &'static str, value: impl std::fmt::Display) {
        if self.is_enabled() {
            self.fields.push((key, value.to_string()));
        }
    }

    fn emit(&mut self) {
        let Some(start) = self.start.take() else {
            return;
        };

        eprintln!(
            "{}",
            format_line(
                self.phase,
                self.fields
                    .iter()
                    .map(|(key, value)| (*key, value.as_str())),
                start.elapsed(),
            )
        );
    }
}

impl Drop for ProfileScope {
    fn drop(&mut self) {
        self.emit();
    }
}
