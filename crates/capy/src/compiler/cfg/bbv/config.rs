pub(super) fn version_limit() -> usize {
    std::env::var("CAPY_SBBV_VERSION_LIMIT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(2)
}

pub(super) fn enabled() -> bool {
    version_limit() > 0
}
