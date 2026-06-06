use std::sync::RwLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[allow(dead_code)]
pub(crate) enum ExecutionPolicy {
    #[default]
    Aot,
    Jit,
}

static EXECUTION_POLICY: RwLock<ExecutionPolicy> = RwLock::new(ExecutionPolicy::Aot);

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
