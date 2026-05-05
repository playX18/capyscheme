use std::sync::RwLock;

#[cfg(test)]
use std::sync::Mutex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[allow(dead_code)]
pub(crate) enum ExecutionPolicy {
    #[default]
    JIT,
    #[cfg(feature = "aot")]
    AOT,
}

static EXECUTION_POLICY: RwLock<ExecutionPolicy> = RwLock::new(ExecutionPolicy::JIT);

#[cfg(test)]
pub(crate) static EXECUTION_POLICY_TEST_LOCK: Mutex<()> = Mutex::new(());

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn execution_policy_defaults_to_jit() {
        let _guard = EXECUTION_POLICY_TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let previous = get_execution_policy();
        set_execution_policy(ExecutionPolicy::JIT);

        assert_eq!(ExecutionPolicy::default(), ExecutionPolicy::JIT);
        assert_eq!(get_execution_policy(), ExecutionPolicy::JIT);

        set_execution_policy(previous);
    }
}
