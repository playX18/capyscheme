use std::sync::RwLock;

use crate::runtime::fasl::FaslLoadOptions;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[allow(dead_code)]
pub(crate) enum ExecutionPolicy {
    #[default]
    Aot,
    Jit,
}

static EXECUTION_POLICY: RwLock<ExecutionPolicy> = RwLock::new(ExecutionPolicy::Aot);
static FASL_LOAD_OPTIONS: RwLock<FaslLoadOptions> = RwLock::new(FaslLoadOptions::NORMAL);

pub(crate) fn get_execution_policy() -> ExecutionPolicy {
    *EXECUTION_POLICY
        .read()
        .expect("execution policy lock should not be poisoned")
}

#[allow(dead_code)]
pub(crate) fn set_execution_policy(policy: ExecutionPolicy) {
    *EXECUTION_POLICY
        .write()
        .expect("execution policy lock should not be poisoned") = policy;
}

pub(crate) fn get_fasl_load_options() -> FaslLoadOptions {
    *FASL_LOAD_OPTIONS
        .read()
        .expect("FASL load options lock should not be poisoned")
}

pub(crate) fn set_fasl_load_options(options: FaslLoadOptions) {
    *FASL_LOAD_OPTIONS
        .write()
        .expect("FASL load options lock should not be poisoned") = options;
}

pub(crate) fn set_fasl_debug_entries(enabled: bool) {
    set_fasl_load_options(if enabled {
        FaslLoadOptions::DEBUG
    } else {
        FaslLoadOptions::NORMAL
    });
}
