/* Capy Scheme Native Interface (SNI) — C ABI for libcapy.
 *
 * Values are opaque ScmRef handles rooted in local/global OopStorage.
 * Obtain a SniEnv* via scm_enter (or scm_attach_current_thread).
 *
 * Error channel: on type confusion or out-of-bounds access, accessor
 * functions set a pending exception (retrieve with sni_exception_occurred,
 * clear with sni_exception_clear) and return SCM_REF_NULL (or 0). Mutators
 * set the pending exception and otherwise do nothing. Passing a NULL env or
 * NULL ref returns silently without setting an exception.
 */
#ifndef CAPY_H
#define CAPY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Scm Scm;
typedef struct SniEnv SniEnv;
typedef struct ScmRefOpaque *ScmRef;

typedef struct SNINativeInterface SNINativeInterface;

struct SniEnv {
  const SNINativeInterface *functions;
  /* remaining fields are private */
};

/* --- Lifecycle ---------------------------------------------------------- */

/* Create a new Scheme runtime instance; NULL on failure. */
Scm *scm_new(void);
/* Create a runtime from a previously saved boot image; NULL on failure. */
Scm *scm_from_image(const uint8_t *image_data, size_t image_size);
/* Destroy a runtime created by scm_new / scm_from_image. */
void scm_free(Scm *scm);

/* Entry callback run with a live env; its return value becomes scm_enter's. */
typedef int (*ScmEnterFn)(SniEnv *env, void *arg);
/* Run `enter` on the runtime's main thread with a live env. */
int scm_enter(Scm *scm, ScmEnterFn enter, void *arg);
/* Attach the calling thread to the runtime and run `enter` with a live env. */
int scm_attach_current_thread(Scm *scm, ScmEnterFn enter, void *arg);
/* Detach a thread previously attached with scm_attach_current_thread. */
void scm_detach_current_thread(Scm *scm);

/* Thread entry callback run on a new Scheme thread. */
typedef void *(*ThreadFn)(Scm *scm, void *arg);
/* Spawn a Scheme thread running `init`; returns a local ref of the thread. */
ScmRef scm_fork(SniEnv *env, ThreadFn init, void *arg);

/* --- Local / global refs ------------------------------------------------ */

/* Push a local-reference frame holding at least `capacity` refs; 0 on success. */
int sni_push_local_frame(SniEnv *env, int capacity);
/* Pop the current local frame, keeping `result` alive in the caller's frame. */
ScmRef sni_pop_local_frame(SniEnv *env, ScmRef result);
/* Ensure the current local frame can hold `capacity` more refs; 0 on success. */
int sni_ensure_local_capacity(SniEnv *env, int capacity);
/* Add a local ref for `obj` to the current frame. */
ScmRef sni_new_local_ref(SniEnv *env, ScmRef obj);
/* Remove a local ref from its frame early. */
void sni_delete_local_ref(SniEnv *env, ScmRef obj);
/* Create a global ref for `obj` that survives local-frame pops. */
ScmRef sni_new_global_ref(SniEnv *env, ScmRef obj);
/* Release a global ref created with sni_new_global_ref. */
void sni_delete_global_ref(SniEnv *env, ScmRef obj);
/* True if `a` and `b` denote the same object (eq?). */
bool sni_is_same(SniEnv *env, ScmRef a, ScmRef b);
/* True if `obj` is SCM_REF_NULL. */
bool sni_ref_is_null(ScmRef obj);

/* --- Call / lookup ------------------------------------------------------ */

/* Look up the public binding `name` in `module_name`; `default_value` if unbound. */
ScmRef sni_public_ref(SniEnv *env, const char *module_name, const char *name,
                      ScmRef default_value);
/* Look up the binding `name` in `module_name`, including non-exported ones. */
ScmRef sni_private_ref(SniEnv *env, const char *module_name, const char *name,
                       ScmRef default_value);
/* Call `proc` with `argc` arguments from `argv`. */
ScmRef sni_call_n(SniEnv *env, ScmRef proc, int argc, const ScmRef *argv);
/* Call `proc` with up to 3 fixed arguments; `nargs` selects how many are passed. */
ScmRef sni_call(SniEnv *env, ScmRef proc, ScmRef a0, ScmRef a1, ScmRef a2,
                int nargs);

/* Fills the argument buffer before a scm_call invocation. */
typedef void (*PrepareCallFn)(SniEnv *env, ScmRef *args, void *data);
/* Receives the result of a scm_call invocation; its return value is scm_call's. */
typedef int (*FinishCallFn)(SniEnv *env, bool success, ScmRef result,
                            void *data);
/* Call `func_name` exported by module `mod_name`; `prepare` supplies args and
 * `finish` consumes the outcome. Returns 0 when the lookup fails. */
int scm_call(Scm *scm, const char *mod_name, const char *func_name,
             PrepareCallFn prepare, void *data1, FinishCallFn finish,
             void *data2);

typedef void (*NativeCallback)(void *data);
/* Run `callback` with the thread marked InNative (no GC safepoints). */
void sni_call_in_native(SniEnv *env, NativeCallback callback, void *data);

/* --- Exceptions --------------------------------------------------------- */

/* Set `obj` as the pending exception; 0 on success. */
int sni_throw(SniEnv *env, ScmRef obj);
/* True if a pending exception is set. */
bool sni_exception_check(SniEnv *env);
/* The pending exception without clearing it; SCM_REF_NULL if none. */
ScmRef sni_exception_occurred(SniEnv *env);
/* Clear and return the pending exception; SCM_REF_NULL if none. */
ScmRef sni_exception_clear(SniEnv *env);
/* Print the pending exception to stderr (best-effort). */
void sni_exception_describe(SniEnv *env);

/* --- Predicates --------------------------------------------------------- */

/* True if `obj` is the empty list. */
bool sni_is_null(SniEnv *env, ScmRef obj);
/* True if `obj` is the EOF object. */
bool sni_is_eof(SniEnv *env, ScmRef obj);
/* True if `obj` is the unspecified value. */
bool sni_is_unspecified(SniEnv *env, ScmRef obj);
/* True if `obj` is a boolean. */
bool sni_is_bool(SniEnv *env, ScmRef obj);
/* True if `obj` is not #f. */
bool sni_is_true(SniEnv *env, ScmRef obj);
/* True if `obj` is a fixnum. */
bool sni_is_fixnum(SniEnv *env, ScmRef obj);
/* True if `obj` is a flonum. */
bool sni_is_flonum(SniEnv *env, ScmRef obj);
/* True if `obj` is a character. */
bool sni_is_char(SniEnv *env, ScmRef obj);
/* True if `obj` is a pair. */
bool sni_is_pair(SniEnv *env, ScmRef obj);
/* True if `obj` is a string. */
bool sni_is_string(SniEnv *env, ScmRef obj);
/* True if `obj` is a symbol. */
bool sni_is_symbol(SniEnv *env, ScmRef obj);
/* True if `obj` is a vector. */
bool sni_is_vector(SniEnv *env, ScmRef obj);
/* True if `obj` is a bytevector. */
bool sni_is_bytevector(SniEnv *env, ScmRef obj);
/* True if `obj` is a procedure. */
bool sni_is_procedure(SniEnv *env, ScmRef obj);

/* --- Constructors / accessors ------------------------------------------- */

/* The empty list. */
ScmRef sni_null(SniEnv *env);
/* The EOF object. */
ScmRef sni_eof(SniEnv *env);
/* The unspecified value. */
ScmRef sni_unspecified(SniEnv *env);
/* #t / #f from a C bool. */
ScmRef sni_bool(SniEnv *env, bool value);
/* Fixnum from an int32. */
ScmRef sni_fixnum(SniEnv *env, int32_t value);
/* Flonum from a double. */
ScmRef sni_flonum(SniEnv *env, double value);
/* Character from a Unicode scalar value. */
ScmRef sni_char(SniEnv *env, uint32_t ch);
/* Intern a symbol from a NUL-terminated UTF-8 name. */
ScmRef sni_intern_symbol(SniEnv *env, const char *name);
/* String from a NUL-terminated UTF-8 string. */
ScmRef sni_string(SniEnv *env, const char *data);
/* String from a UTF-8 byte slice. */
ScmRef sni_string_utf8(SniEnv *env, const uint8_t *data, size_t len);
/* Pair of `car` and `cdr`. */
ScmRef sni_cons(SniEnv *env, ScmRef car, ScmRef cdr);
/* Car of `pair`. */
ScmRef sni_car(SniEnv *env, ScmRef pair);
/* Cdr of `pair`. */
ScmRef sni_cdr(SniEnv *env, ScmRef pair);
/* Set the car of `pair`. */
void sni_set_car(SniEnv *env, ScmRef pair, ScmRef car);
/* Set the cdr of `pair`. */
void sni_set_cdr(SniEnv *env, ScmRef pair, ScmRef cdr);

/* New vector of `len` slots initialized to `fill`. */
ScmRef sni_make_vector(SniEnv *env, size_t len, ScmRef fill);
/* Number of elements in `vector`. */
size_t sni_vector_length(SniEnv *env, ScmRef vector);
/* Element `index` of `vector`. */
ScmRef sni_vector_ref(SniEnv *env, ScmRef vector, size_t index);
/* Set element `index` of `vector`. */
void sni_vector_set(SniEnv *env, ScmRef vector, size_t index, ScmRef value);

/* New bytevector of `len` bytes initialized to `fill`. */
ScmRef sni_make_bytevector(SniEnv *env, size_t len, uint8_t fill);
/* Number of bytes in `bv`. */
size_t sni_bytevector_length(SniEnv *env, ScmRef bv);
/* Byte `index` of `bv`. */
int32_t sni_bytevector_ref(SniEnv *env, ScmRef bv, size_t index);
/* Set byte `index` of `bv`. */
void sni_bytevector_set(SniEnv *env, ScmRef bv, size_t index, uint8_t byte);

/* Length in characters of `s`. */
size_t sni_string_length(SniEnv *env, ScmRef s);
/* Character at `index` of `s` as a Unicode scalar value. */
uint32_t sni_string_ref(SniEnv *env, ScmRef s, size_t index);
/* Set the character at `index` of `s`. */
void sni_string_set(SniEnv *env, ScmRef s, size_t index, uint32_t ch);
/* Encode string `value` as UTF-8 into `buf`; *written gets the byte count.
 * Returns false if `value` is not a string or does not fit. */
bool sni_string_to_utf8(SniEnv *env, ScmRef value, char *buf, size_t capacity,
                        size_t *written);
/* Encode the printed representation of `value` as UTF-8 into `buf`. */
bool sni_value_to_utf8(SniEnv *env, ScmRef value, char *buf, size_t capacity,
                       size_t *written);

/* Exact integer from a uint32. */
ScmRef sni_uint32(SniEnv *env, uint32_t value);
/* Exact integer from a uint64. */
ScmRef sni_uint64(SniEnv *env, uint64_t value);
/* Exact integer from an int64. */
ScmRef sni_int64(SniEnv *env, int64_t value);
/* Extract `value` as u8; false if not an exact integer in range. */
bool sni_to_u8(SniEnv *env, ScmRef value, uint8_t *res);
/* Extract `value` as u16; false if not an exact integer in range. */
bool sni_to_u16(SniEnv *env, ScmRef value, uint16_t *res);
/* Extract `value` as u32; false if not an exact integer in range. */
bool sni_to_u32(SniEnv *env, ScmRef value, uint32_t *res);
/* Extract `value` as u64; false if not an exact integer in range. */
bool sni_to_u64(SniEnv *env, ScmRef value, uint64_t *res);
/* Extract `value` as i8; false if not an exact integer in range. */
bool sni_to_i8(SniEnv *env, ScmRef value, int8_t *res);
/* Extract `value` as i16; false if not an exact integer in range. */
bool sni_to_i16(SniEnv *env, ScmRef value, int16_t *res);
/* Extract `value` as i32; false if not an exact integer in range. */
bool sni_to_i32(SniEnv *env, ScmRef value, int32_t *res);
/* Extract `value` as i64; false if not an exact integer in range. */
bool sni_to_i64(SniEnv *env, ScmRef value, int64_t *res);
/* Extract `value` as f32; false if not a real number. */
bool sni_to_f32(SniEnv *env, ScmRef value, float *res);
/* Extract `value` as f64; false if not a real number. */
bool sni_to_f64(SniEnv *env, ScmRef value, double *res);
/* Extract any real number as f64; false if not a real number. */
bool sni_real_to_f64(SniEnv *env, ScmRef value, double *res);
/* Scheme truth value of `value` (anything but #f is true). */
bool sni_to_bool(SniEnv *env, ScmRef value);

/* --- Load / program args ------------------------------------------------ */

/* Load and evaluate a Scheme source file; 0 on success, -1 on failure. */
int sni_load_file(Scm *scm, const char *name);
/* Compatibility alias of sni_load_file. */
int scm_load_file(Scm *scm, const char *name);
/* Evaluate a UTF-8 source string; SCM_REF_NULL on error. */
ScmRef sni_eval_string(SniEnv *env, const char *source);
/* Program arguments as a Scheme list (new local ref). */
ScmRef sni_program_arguments(SniEnv *env);
/* Initialize program arguments from argc/argv. */
void sni_program_arguments_init(SniEnv *env, int argc, const char *const *argv);
/* Set program arguments from a Scheme list. */
void sni_set_program_arguments(SniEnv *env, ScmRef args);
/* size_t-argc variant used by the C CLI entry points; forwards to
 * sni_program_arguments_init. */
void scm_program_arguments_init(SniEnv *env, size_t argc,
                                const char *const *argv);

/* --- Native procedures / module define ---------------------------------- */

/* Native procedure callable from Scheme. */
typedef ScmRef (*SniNativeFn)(SniEnv *env, int argc, const ScmRef *argv);
/* Wrap a C function as a Scheme procedure (new local ref). */
ScmRef sni_new_native_procedure(SniEnv *env, SniNativeFn fn);
/* Define `name` in `module` to `value`; 0 on success. */
int sni_define(SniEnv *env, const char *module, const char *name, ScmRef value);

/* --- R6RS records ------------------------------------------------------- */

/* Make an R6RS record-type descriptor. */
ScmRef sni_make_rtd(SniEnv *env, ScmRef name, ScmRef parent, ScmRef uid,
                    bool sealed, bool opaque, ScmRef fields);
/* Make a record-constructor descriptor for `rtd`. */
ScmRef sni_make_rcd(SniEnv *env, ScmRef rtd, ScmRef parent_rcd,
                    ScmRef protocol);
/* Constructor procedure for records described by `rcd`. */
ScmRef sni_record_constructor(SniEnv *env, ScmRef rcd);
/* Predicate procedure for records of type `rtd`. */
ScmRef sni_record_predicate(SniEnv *env, ScmRef rtd);
/* Accessor procedure for field `field` of records of type `rtd`. */
ScmRef sni_record_accessor(SniEnv *env, ScmRef rtd, int field);
/* Mutator procedure for field `field` of records of type `rtd`. */
ScmRef sni_record_mutator(SniEnv *env, ScmRef rtd, int field);
/* True if `obj` is a record. */
bool sni_is_record(SniEnv *env, ScmRef obj);

/* --- Classes / generics ------------------------------------------------- */

/* Make a class with `name`, `slots` and direct superclasses `supers`. */
ScmRef sni_make_class(SniEnv *env, ScmRef name, ScmRef slots, ScmRef supers);
/* Make an abstract class (cannot be instantiated directly). */
ScmRef sni_make_abstract_class(SniEnv *env, ScmRef name, ScmRef supers);
/* Instantiate `class_` with `n` initialization arguments. */
ScmRef sni_make_instance(SniEnv *env, ScmRef class_, int n,
                         const ScmRef *initargs);
/* Read slot `slot` of `obj`; `unbound` if the slot is unbound. */
ScmRef sni_slot_ref(SniEnv *env, ScmRef obj, ScmRef slot, ScmRef unbound);
/* Write slot `slot` of `obj`; 0 on success. */
int sni_slot_set(SniEnv *env, ScmRef obj, ScmRef slot, ScmRef value);
/* The class of `obj`. */
ScmRef sni_class_of(SniEnv *env, ScmRef obj);
/* A built-in class by name (e.g. "<pair>"). */
ScmRef sni_builtin_class(SniEnv *env, const char *name);
/* Make a generic function dispatching on at most `max_dispatch_args` args. */
ScmRef sni_make_generic(SniEnv *env, ScmRef name, int max_dispatch_args);
/* Add a method to `generic`; 0 on success. */
int sni_add_method(SniEnv *env, ScmRef generic, ScmRef specializers,
                   int required_argc, ScmRef body, bool locked);

/* --- POD records (movable bytevectors) ---------------------------------- */

/* New movable bytevector usable as POD storage. */
ScmRef sni_make_pod_bytevector(SniEnv *env, size_t len);
/* Raw pointer to `bv` contents; invalid after the next GC or nest. */
uint8_t *sni_bytevector_data(SniEnv *env, ScmRef bv);
/* Number of bytes in `bv`. */
size_t sni_bytevector_len(SniEnv *env, ScmRef bv);
/* Register a POD type descriptor by name, size, alignment and uid. */
ScmRef sni_register_pod_type(SniEnv *env, const char *name, size_t size,
                             size_t align, const char *uid);
/* True if `bv` matches the POD type `pod_type`. */
bool sni_pod_type_check(SniEnv *env, ScmRef pod_type, ScmRef bv);

/* --- R6RS exceptions ---------------------------------------------------- */

/* Raise a non-continuable exception; returns only if a handler returns. */
ScmRef sni_raise(SniEnv *env, ScmRef obj);
/* Raise a continuable exception; returns the handler's value. */
ScmRef sni_raise_continuable(SniEnv *env, ScmRef obj);

/* Body thunk for sni_with_exception_handler. */
typedef ScmRef (*SniThunk)(SniEnv *env, void *data);
/* Handler invoked when `body` raises. */
typedef ScmRef (*SniExceptionHandler)(SniEnv *env, ScmRef exn, void *data);
/* Run `body` under `handler` (R6RS with-exception-handler). */
ScmRef sni_with_exception_handler(SniEnv *env, SniExceptionHandler handler,
                                  void *handler_data, SniThunk body,
                                  void *body_data);

/* Compound condition from `n` component conditions. */
ScmRef sni_condition(SniEnv *env, int n, const ScmRef *components);
/* New &message condition. */
ScmRef sni_make_message_condition(SniEnv *env, const char *msg);
/* New &who condition. */
ScmRef sni_make_who_condition(SniEnv *env, const char *who);
/* New &irritants condition from `n` values. */
ScmRef sni_make_irritants_condition(SniEnv *env, int n,
                                    const ScmRef *irritants);
/* New &assertion condition. */
ScmRef sni_make_assertion_violation(SniEnv *env);
/* New &error condition. */
ScmRef sni_make_error(SniEnv *env);
/* Raise an assertion violation with who, message and `n` irritants. */
void sni_assertion_violation(SniEnv *env, const char *who, const char *message,
                             int n, const ScmRef *irritants);

/* --- Continuations (CPS trampoline) ------------------------------------ */

typedef struct SniContinuation SniContinuation;

/* Resume: return updated continuation (may be same). NULL next => finished. */
typedef SniContinuation *(*SniContFn)(SniEnv *env, SniContinuation *c);

struct SniContinuation {
  SniContFn next; /* NULL => finished */
  /* Continuation-owned strong root (SNI globals OopStorage). Allocated by
   * sni_cont_alloc / released by sni_cont_free. Do NOT replace this pointer;
   * write values with sni_cont_set_result / sni_call_k / sni_raise_k. */
  ScmRef result;
  void *data;                /* opaque env blob (Rust state machine) */
  void (*drop_data)(void *); /* optional destructor for data */
};

SniContinuation *sni_cont_alloc(SniEnv *env, SniContFn start, void *data,
                                void (*drop_data)(void *));
void sni_cont_free(SniEnv *env, SniContinuation *c);

/* Copy `value` into c->result's owned root (does not replace the slot). */
void sni_cont_set_result(SniEnv *env, SniContinuation *c, ScmRef value);

/* Run until finished; returns a new local-ref of the final value. */
ScmRef sni_trampoline(SniEnv *env, SniContinuation *c);

/* Call proc; store outcome into c's owned result root. */
void sni_call_k(SniEnv *env, ScmRef proc, int argc, const ScmRef *argv,
                SniContinuation *c);

/* Raise into Scheme; store outcome into c's owned result root. */
void sni_raise_k(SniEnv *env, ScmRef obj, SniContinuation *c);

/* Run body under handler: both are continuation graphs. Returns finished cont. */
SniContinuation *sni_guard_k(SniEnv *env, SniContinuation *handler,
                             SniContinuation *body);

/* --- Vtable / extensions ------------------------------------------------ */

/* The native-interface vtable for this env. */
const SNINativeInterface *sni_get_functions(SniEnv *env);

/* Native extension entry: export this from a plugin .so loaded via
 * load-native-extension. Prefer SNI_OnLoad(SniEnv*, void*). Return 0 on
 * success. Legacy Rust ABI capy_register_extension is still accepted. */
typedef int (*SNI_OnLoadFn)(SniEnv *env, void *reserved);

struct SNINativeInterface {
  int (*PushLocalFrame)(SniEnv *env, int capacity);
  ScmRef (*PopLocalFrame)(SniEnv *env, ScmRef result);
  int (*EnsureLocalCapacity)(SniEnv *env, int capacity);
  ScmRef (*NewLocalRef)(SniEnv *env, ScmRef obj);
  void (*DeleteLocalRef)(SniEnv *env, ScmRef obj);
  ScmRef (*NewGlobalRef)(SniEnv *env, ScmRef obj);
  void (*DeleteGlobalRef)(SniEnv *env, ScmRef obj);
  bool (*IsSameObject)(SniEnv *env, ScmRef a, ScmRef b);
  ScmRef (*CallN)(SniEnv *env, ScmRef proc, int argc, const ScmRef *argv);
  bool (*ExceptionCheck)(SniEnv *env);
  ScmRef (*ExceptionOccurred)(SniEnv *env);
  ScmRef (*ExceptionClear)(SniEnv *env);
  void (*ExceptionDescribe)(SniEnv *env);
  int (*Throw)(SniEnv *env, ScmRef obj);
  ScmRef (*PublicRef)(SniEnv *env, const char *module_name, const char *name,
                      ScmRef default_value);
  ScmRef (*Cons)(SniEnv *env, ScmRef car, ScmRef cdr);
  ScmRef (*Car)(SniEnv *env, ScmRef pair);
  ScmRef (*Cdr)(SniEnv *env, ScmRef pair);
  ScmRef (*NewString)(SniEnv *env, const char *data);
  ScmRef (*Null)(SniEnv *env);
  ScmRef (*NewNativeProcedure)(SniEnv *env, SniNativeFn fn);
  int (*Define)(SniEnv *env, const char *module, const char *name,
                ScmRef value);
  ScmRef (*MakeClass)(SniEnv *env, ScmRef name, ScmRef slots, ScmRef supers);
  ScmRef (*MakeAbstractClass)(SniEnv *env, ScmRef name, ScmRef supers);
  ScmRef (*MakeInstance)(SniEnv *env, ScmRef class_, int n,
                         const ScmRef *initargs);
  ScmRef (*SlotRef)(SniEnv *env, ScmRef obj, ScmRef slot, ScmRef unbound);
  int (*SlotSet)(SniEnv *env, ScmRef obj, ScmRef slot, ScmRef value);
  ScmRef (*MakeGeneric)(SniEnv *env, ScmRef name, int max_dispatch_args);
  int (*AddMethod)(SniEnv *env, ScmRef generic, ScmRef specializers,
                   int required_argc, ScmRef body, bool locked);
  ScmRef (*Raise)(SniEnv *env, ScmRef obj);
  ScmRef (*RaiseContinuable)(SniEnv *env, ScmRef obj);
  ScmRef (*WithExceptionHandler)(SniEnv *env, SniExceptionHandler handler,
                                 void *handler_data, SniThunk body,
                                 void *body_data);
  void (*AssertionViolation)(SniEnv *env, const char *who, const char *message,
                             int n, const ScmRef *irritants);
  ScmRef (*Trampoline)(SniEnv *env, SniContinuation *c);
  void (*CallK)(SniEnv *env, ScmRef proc, int argc, const ScmRef *argv,
                SniContinuation *c);
  void (*RaiseK)(SniEnv *env, ScmRef obj, SniContinuation *c);
  SniContinuation *(*GuardK)(SniEnv *env, SniContinuation *handler,
                             SniContinuation *body);
};

#ifdef __cplusplus
}
#endif

#endif /* CAPY_H */
