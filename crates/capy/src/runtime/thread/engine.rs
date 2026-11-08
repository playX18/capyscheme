/// Execution engine, chosen by `CAPY_ENGINE` (via [`flags::engine`]).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]

pub enum Engine {
    Rust,
    Template,
}

impl Engine {
    fn as_str(self) -> &'static str {
        match self {
            Engine::Rust => "rust",
            Engine::Template => "template",
        }
    }

    /// Build default: the template interpreter when compiled in, else the Rust engine.
    pub fn default_for_build() -> Self {
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            Engine::Template
        }
        #[cfg(not(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64"))))]
        {
            Engine::Rust
        }
    }

    /// Engine from the `engine` flag. An unset flag or an invalid value
    /// (already warned about by the flag registry) gives the build default.
    pub(super) fn from_flags_or_default() -> Self {
        match crate::utils::flags::engine() {
            "rust" => Engine::Rust,
            "template" => Engine::Template,
            _ => Self::default_for_build(),
        }
    }
}
