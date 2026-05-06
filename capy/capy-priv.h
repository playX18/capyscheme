#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include "wchar.h"



/**
 * Offset from allocation to the actual object
 */
#define OBJECT_REF_OFFSET 8

#define OBJECT_HEADER_OFFSET -OBJECT_REF_OFFSET

#define UNKNOWN_TYPE_BITS UINT16_MAX

#define FASL_EOF 0

#define FASL_TAG_LOOKUP 1

#define FASL_TAG_FIXNUM 2

#define FASL_TAG_PLIST 3

#define FASL_TAG_DLIST 4

#define FASL_TAG_VECTOR 5

#define FASL_TAG_RATIONAL 6

#define FASL_TAG_COMPLEX 7

#define FASL_TAG_FLONUM 8

#define FASL_TAG_BIGINT 9

#define FASL_TAG_BVECTOR 10

#define FASL_TAG_CHAR 11

#define FASL_TAG_NIL 12

#define FASL_TAG_T 13

#define FASL_TAG_F 14

#define FASL_TAG_SYMBOL 15

#define FASL_TAG_STR 16

#define FASL_TAG_UNINTERNED_SYMBOL 17

#define FASL_TAG_IMMEDIATE 18

#define FASL_TAG_SYNTAX 19

#define FASL_TAG_TUPLE 20

#define FASL_TAG_KEYWORD 21

/**
 * Reference to an object.
 */
#define FASL_TAG_REF 255

#define FASL_TAG_REF_INIT 254



#define PURE_NAN_BITS 9221120237041090560

#define IMPURE_NAN_BITS 18446462598732840960ull

#define VALUE_DOUBLE_ENCODE_OFFSET_BIT 49

#define VALUE_DOUBLE_ENCODE_OFFSET (1 << VALUE_DOUBLE_ENCODE_OFFSET_BIT)

#define VALUE_NUMBER_TAG 18446181123756130304ull

#define HASHTABLE_HANDLER_SIGNATURE 0

#define HASHTABLE_HANDLER_HASH 1

#define HASHTABLE_HANDLER_EQUIV 2

#define HASHTABLE_HANDLER_SIZE 3

#define HASHTABLE_HANDLER_REF 4

#define HASHTABLE_HANDLER_SET 5

#define HASHTABLE_HANDLER_DELETE 6

#define HASHTABLE_HANDLER_CONTAINS 7

#define HASHTABLE_HANDLER_COPY 8

#define HASHTABLE_HANDLER_CLEAR 9

#define HASHTABLE_HANDLER_HASH_FUNC 10

#define HASHTABLE_HANDLER_EQUIV_FUNC 11

#define HASHTABLE_HANDLER_MUTABLE 12

#define HASHTABLE_HANDLER_ALIST 13

#define DIGIT_BIT 64

#define DIGIT_MASK UINT64_MAX

#define DIGIT_BIT_SHIFT_COUNT 6



#define WCM_KEY (TERM_SOURCEV + 1)

#define WCM_VALUE (TERM_SOURCEV + 2)

#define WCM_BODY (TERM_SOURCEV + 3)

#define EREADABLE 1

#define EWRITABLE 2

#define EHUP 4

#define EPRIORITY 8

#define EERROR 16

#define EONESHOT 1

#define ELEVEL 2

#define EEDGE 4

#define EEDGEONESHOT 8

#define RTD_TYPE 0

#define RTD_NAME 1

#define RTD_PARENT 2

#define RTD_UID 3

#define RTD_SEALED 4

#define RTD_OPAQUE 5

#define RTD_FIELDS 6

typedef struct Scm Scm;

typedef struct Scm *ScmRef;

typedef union EncodedValueDescriptor {
  int64_t as_i64;
  uint64_t as_u64;
  double as_f64;
  void *ptr;
} EncodedValueDescriptor;

/**
 * A Scheme value.
 *
 * Uses NaN boxing layout based on JavaScriptCore NaN boxing, we formally prove
 * the correctness of this encoding in tests/z3_value_encoding_proof.py.
 *
 * Heap objects store their tags in the GC header that lives at `OBJECT_REF_OFFSET`
 * bytes before the payload. Some objects still use their first payload word for
 * per-instance metadata such as variable lengths.
 */
typedef struct Value {
  union EncodedValueDescriptor desc;
} Value;


#define Value_NUMBER_TAG (int64_t)18446181123756130304ull
#define Value_LOWEST_OF_HIGH_BITS (1 << 49)
#define Value_OTHER_TAG 2
#define Value_BOOL_TAG 4
#define Value_UNDEFINED_TAG 8
#define Value_CHAR_TAG 130
#define Value_CHAR_MASK (Value_NUMBER_TAG | Value_CHAR_TAG)
#define Value_VALUE_FALSE ((Value_OTHER_TAG | Value_BOOL_TAG) | 0)
#define Value_VALUE_TRUE ((Value_OTHER_TAG | Value_BOOL_TAG) | 1)
#define Value_VALUE_UNDEFINED (Value_OTHER_TAG | Value_UNDEFINED_TAG)
#define Value_VALUE_NULL Value_OTHER_TAG
#define Value_MISC_TAG ((Value_OTHER_TAG | Value_BOOL_TAG) | Value_UNDEFINED_TAG)
#define Value_NOT_CELL_MASK (Value_NUMBER_TAG | Value_OTHER_TAG)
#define Value_VALUE_EMPTY 0
#define Value_VALUE_DELETED 4
#define Value_VALUE_VOID (Value_MISC_TAG | 16)
#define Value_VALUE_UNSPECIFIED (Value_MISC_TAG | 32)
#define Value_VALUE_EOF (Value_MISC_TAG | 48)
#define Value_VALUE_BWP (Value_MISC_TAG | 64)

typedef const void *ContextRef;

typedef void *(*ThreadFn)(ScmRef parent, void *arg);

typedef int (*ScmEnterFn)(ContextRef ctx, void *arg);

typedef void (*PrepareCallFn)(ContextRef ctx, struct Value *args, void *data);

typedef int (*FinishCallFn)(ContextRef ctx, bool success, struct Value result, void *data);





extern const HeapTypeInfo *PAIR_INFO_STATIC;

extern const HeapTypeInfo *VARIABLE_INFO_STATIC;

extern const HeapTypeInfo *CLOSURE_PROC_INFO_STATIC;

extern const HeapTypeInfo *CLOSURE_K_INFO_STATIC;

extern const HeapTypeInfo *MUTABLE_VECTOR_INFO_STATIC;

extern const HeapTypeInfo *IMMUTABLE_VECTOR_INFO_STATIC;

extern const HeapTypeInfo *TUPLE_INFO_STATIC;

/**
 * Create a new Scheme thread instance.
 *
 * The thread instance will be attached to currently active VM or
 * will initialize a new VM if there isn't any.
 *
 * For thread creation look at [`scm_fork()`](scm_fork).
 */
ScmRef scm_new(void);

/**
 * Create a Scheme thread instance and VM from the given heap image.
 *
 * This function can only be invoked once per process.
 *
 * **Note:** This function is currently unimplemented and will panic if called.
 */
ScmRef scm_from_image(const uint8_t *_image_data, uintptr_t _image_size);

/**
 * Free the Scheme thread instance created by [`scm_new()`](scm_new)
 */
void scm_free(ScmRef scm);

/**
 * Fork a new Scheme thread from the given parent thread.
 *
 * New thread is created with a copy of the parent's dynamic state.
 *
 * Returns thread-object for the newly created thread.
 */
struct Value scm_fork(ContextRef parent, ThreadFn init, void *arg);

/**
 * Enter the Scheme runtime with the given Scheme instance.
 *
 * This function will set up the necessary runtime state and call the provided
 * `enter` function with a context reference and the provided argument.
 */
int scm_enter(ScmRef scm, ScmEnterFn enter, void *arg);

/**
 * Given a Scheme context, module name, and variable name,
 * return the public variable reference or the default value
 *
 * Example:
 *
 * ```c
 * int my_enter(ContextRef ctx, void* arg) {
 *     Value var = scm_public_ref(ctx, "boot cli", "enter", VALUE_NULL);
 *     return 0;
 * }
 *
 * ScmRef scm = scm_new();
 * scm_enter(scm, my_enter, NULL);
 * ```
 */
struct Value scm_public_ref(ContextRef ctx,
                            const char *module_name,
                            const char *name,
                            struct Value default_value);

/**
 * Given a Scheme context, module name, and variable name,
 * return the private variable reference or the default value
 *
 * Example:
 * ```c
 * int my_enter(ContextRef ctx, void* arg) {
 *     Value var = scm_private_ref(ctx, "boot cli", "internal-variable", VALUE_NULL);
 *     return 0;
 * }
 *
 * ScmRef scm = scm_new();
 * scm_enter(scm, my_enter, NULL);
 * ```
 */
struct Value scm_private_ref(ContextRef ctx,
                             const char *module_name,
                             const char *name,
                             struct Value default_value);

/**
 * Intern a symbol with the given name in the Scheme context.
 */
struct Value scm_intern_symbol(ContextRef ctx, const char *name);

/**
 * Create a new Scheme string with the given data in the Scheme context.
 */
struct Value scm_string(ContextRef ctx, const char *data);

/**
 * Return whether `value` is a Scheme string.
 */
bool scm_is_string(ContextRef _ctx, struct Value value);

/**
 * Return the number of UTF-8 bytes needed to represent a Scheme string.
 *
 * Returns 0 when `value` is not a Scheme string.
 */
uintptr_t scm_string_utf8_length(ContextRef _ctx, struct Value value);

/**
 * Copy a Scheme string to a UTF-8 buffer.
 *
 * `written`, when non-null, receives the required byte length excluding the
 * trailing NUL. Returns false if `value` is not a string, `buf` is null, or
 * `capacity` is too small for the UTF-8 bytes plus a trailing NUL.
 */
bool scm_string_to_utf8(ContextRef _ctx,
                        struct Value value,
                        char *buf,
                        uintptr_t capacity,
                        uintptr_t *written);

/**
 * Return the number of UTF-8 bytes needed to print a Scheme value.
 */
uintptr_t scm_value_utf8_length(ContextRef _ctx, struct Value value);

/**
 * Copy the printed representation of any Scheme value to a UTF-8 buffer.
 */
bool scm_value_to_utf8(ContextRef _ctx,
                       struct Value value,
                       char *buf,
                       uintptr_t capacity,
                       uintptr_t *written);

/**
 * Convert a Scheme real number to an `f64`. Writes the result into `res` and
 * returns `true` on success, or `false` if `value` is not a number.
 */
bool scm_real_to_f64(ContextRef ctx, struct Value value, double *res);

/**
 * Construct a new Scheme pair (cons cell) from the given car and cdr values.
 */
struct Value scm_cons(ContextRef ctx, struct Value car, struct Value cdr);

/**
 * Set the car (first element) of a Scheme pair.
 */
void scm_set_car(ContextRef ctx, struct Value pair, struct Value car);

/**
 * Set the cdr (second element / rest) of a Scheme pair.
 */
void scm_set_cdr(ContextRef ctx, struct Value pair, struct Value cdr);

/**
 * Set the element at `index` in a Scheme vector to `value`.
 */
void scm_vector_set(ContextRef ctx, struct Value vector, uintptr_t index, struct Value value);

/**
 * Set the element at `index` in a Scheme tuple to `value`.
 */
void scm_tuple_set(ContextRef ctx, struct Value tuple, uintptr_t index, struct Value value);

/**
 * Set the character at `index` in a Scheme string to `ch` (as a Unicode code point).
 * If `ch` is not a valid Unicode scalar value, the replacement character U+FFFD is used.
 */
void scm_string_set(ContextRef ctx, struct Value string, uintptr_t index, uint32_t ch);

/**
 * Return the character at `index` in a Scheme string as a Unicode code point.
 */
uint32_t scm_string_ref(ContextRef _ctx, struct Value string, uintptr_t index);

/**
 * Call a named Scheme procedure from a given module.
 *
 * `prepare` is invoked to build the argument list, and `finish` is invoked
 * with the result (or error) once the call completes.
 */
int scm_call(ScmRef scm,
             const char *mod_name,
             const char *func_name,
             PrepareCallFn prepare,
             void *data1,
             FinishCallFn finish,
             void *data2);

/**
 * Resume a call using the current accumulator value as the procedure.
 *
 * `prepare` builds the argument list, and `finish` receives the result or error.
 */
int scm_resume_accumulator(ScmRef scm,
                           PrepareCallFn prepare,
                           void *data1,
                           FinishCallFn finish,
                           void *data2);

/**
 * Load and evaluate a Scheme source file by path. Returns 0 on success, -1 on failure.
 */
int scm_load_file(ScmRef scm, const char *name);

/**
 * Return the current program arguments as a Scheme list of strings.
 */
struct Value scm_program_arguments(ContextRef ctx);

/**
 * Initialize program arguments from a C-style `argc`/`argv` pair.
 *
 * # Safety
 *
 * `argv` must point to an array of at least `argc` valid, non-null C strings.
 */
void scm_program_arguments_init(ContextRef ctx, uintptr_t argc, const char *const *argv);

/**
 * Set the program arguments to the given Scheme list of strings.
 */
void scm_set_program_arguments(ContextRef ctx, struct Value args);
