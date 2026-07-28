//! Regression tests for the pending-exception type/bounds checks in the C SNI
//! API (`set_type_error` / `set_bounds_error` in `capy/src/api/exceptions.rs`):
//! invalid arguments must surface as a pending exception and an `Err` result
//! instead of crashing or silently succeeding.
//!
//! The safe API moves the pending exception into the `Err` value (clearing
//! the pending slot), so the assertions check the returned condition.
//!
//! Run from the repo root against a built libcapy, e.g.:
//!   CAPY_LOAD_PATH=$PWD/lib LD_LIBRARY_PATH=$PWD/target/debug \
//!     cargo test -p capy-sni --test type_checks
//!
//! Note: a single test on purpose — concurrent `Scm::new()` on several test
//! threads aborts the runtime ("VM globals not initialized").

use capy_sni::Scm;

#[test]
fn type_confused_calls_set_pending_exception() {
    let scm = Scm::new();
    scm.enter(|env| {
        // car of a non-pair: type error.
        let one = env.fixnum(1);
        let err = env
            .car(one)
            .expect_err("car of a fixnum must return Err (pending exception)");
        let msg = env.value_to_utf8(err).unwrap_or_default();
        assert!(
            msg.contains("expected pair"),
            "car error should be a type error, got: {msg}"
        );

        // vector-ref past the end: bounds error.
        let fill = env.fixnum(0);
        let v = env.make_vector(2, fill);
        assert!(env.vector_ref(v, 1).is_ok(), "in-bounds vector-ref");
        let err = env
            .vector_ref(v, 2)
            .expect_err("out-of-bounds vector-ref must return Err (pending exception)");
        let msg = env.value_to_utf8(err).unwrap_or_default();
        assert!(
            msg.contains("out of bounds"),
            "vector-ref error should be a bounds error, got: {msg}"
        );
    });
}
