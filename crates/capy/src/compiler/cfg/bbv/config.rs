use crate::utils::flags;

pub(super) fn version_limit() -> usize {
    flags::sbbv_version_limit()
}

pub(super) fn enabled() -> bool {
    version_limit() > 0
}
