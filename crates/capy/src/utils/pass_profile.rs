use std::{
    fmt::Write,
    sync::LazyLock,
    time::{Duration, Instant},
};

static ENABLED: LazyLock<bool> =
    LazyLock::new(|| parse_enabled_flag(std::env::var("CAPY_PROFILE_PASSES").ok().as_deref()));

pub fn enabled() -> bool {
    *ENABLED
}

fn parse_enabled_flag(value: Option<&str>) -> bool {
    matches!(
        value,
        Some("1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON")
    )
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

#[cfg(test)]
mod tests {
    use super::{format_line, parse_enabled_flag};
    use std::time::Duration;

    #[test]
    fn parse_enabled_flag_handles_expected_values() {
        assert!(!parse_enabled_flag(None));
        assert!(!parse_enabled_flag(Some("0")));
        assert!(parse_enabled_flag(Some("1")));
    }

    #[test]
    fn format_line_is_stable_and_machine_greppable() {
        let line = format_line(
            "cps.rewrite.inline",
            [
                ("round", "2"),
                ("term_size_before", "144"),
                ("shrink_changed", "true"),
            ],
            Duration::from_micros(123_456),
        );

        assert_eq!(
            line,
            ";; PERF (capy) phase=cps.rewrite.inline round=2 term_size_before=144 shrink_changed=true elapsed_ms=123.456"
        );
    }
}
