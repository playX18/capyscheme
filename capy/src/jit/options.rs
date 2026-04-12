use std::sync::OnceLock;

pub struct JITOptions {
    pub enable_allocation_fastpath: bool,
}

pub static OPTIONS: OnceLock<JITOptions> = OnceLock::new();

pub fn get() -> &'static JITOptions {
    OPTIONS.get_or_init(|| JITOptions {
        enable_allocation_fastpath: true,
    })
}
