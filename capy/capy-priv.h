#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * Offset from allocation to the actual object
 */
#define OBJECT_REF_OFFSET 8

#define OBJECT_HEADER_OFFSET -OBJECT_REF_OFFSET

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

/**
 * Reference to an object.
 */
#define FASL_TAG_REF 255

#define FASL_TAG_REF_INIT 254

#define TYPE_IMMEDIATE 0

#define TYPE_REF 1

#define TYPE_TRUE 2

#define TYPE_FALSE 3

#define TYPE_I32 4

#define TYPE_CHAR 5

#define TYPE_EOF 6

#define TYPE_NULL 7

#define TYPE_UNDEFINED 8

#define TYPE_BWP 9

/**
 * Closure from dynamic library: dlsym from library[ix] and sname.
 */
#define TYPE_CLOSURE_SCM 20

/**
 * Native closure: read from vec of native function pointers
 */
#define TYPE_CLOSURE_NATIVE_PROC 21

#define TYPE_CLOSURE_NATIVE_CONT 22

#define TYPE_POINTER_NULL 30

#define TYPE_POINTER_DYLIB 31

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
 * Heap objects store their tags in the header provided by RSGC crate. Some objects
 * such as vectors require first word to be an additional header to store the length
 * or any other information. This header is always 64 bits in size and in case of
 * vectors can be loaded as a valid fixnum value.
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

typedef void *ContextRef;

typedef void *(*ThreadFn)(ScmRef parent, void *arg);

typedef int (*ScmEnterFn)(ContextRef ctx, void *arg);

typedef void (*PrepareCallFn)(ContextRef ctx, struct Value *args, void *data);

typedef int (*FinishCallFn)(ContextRef ctx, bool success, struct Value result, void *data);





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
 */
ScmRef scm_from_image(const uint8_t *image_data, uintptr_t image_size);

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
 * ScmRef scm = scm_new();
 *
 * scm_enter(scm, [](ContextRef ctx, void*){
 *     Value var = scm_public_ref(ctx, "boot cli", "enter", VALUE_NULL);
 * }, NULL);
 *
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
 * ScmRef scm = scm_new();
 * scm_enter(scm, [](ContextRef ctx, void*){
 *     Value var = scm_private_ref(ctx, "boot cli", "internal-variable", VALUE_NULL);
 * }, NULL);
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
 * Create Scheme number from a 32-bit unsigned integer.
 */
struct Value scm_uint32(ContextRef ctx, uint32_t value);

struct Value scm_uint64(ContextRef ctx, uint64_t value);

struct Value scm_int64(ContextRef ctx, int64_t value);

bool scm_to_u8(ContextRef ctx, struct Value value, uint8_t *res);

bool scm_to_u16(ContextRef ctx, struct Value value, uint16_t *res);

bool scm_to_u32(ContextRef ctx, struct Value value, uint32_t *res);

bool scm_to_u64(ContextRef ctx, struct Value value, uint64_t *res);

bool scm_to_i64(ContextRef ctx, struct Value value, int64_t *res);

bool scm_to_f64(ContextRef ctx, struct Value value, double *res);

bool scm_real_to_f64(ContextRef ctx, struct Value value, double *res);

bool scm_to_i32(ContextRef ctx, struct Value value, int32_t *res);

bool scm_to_f32(ContextRef ctx, struct Value value, float *res);

bool scm_to_i16(ContextRef ctx, struct Value value, int16_t *res);

bool scm_to_i8(ContextRef ctx, struct Value value, int8_t *res);

struct Value scm_cons(ContextRef ctx, struct Value car, struct Value cdr);

void scm_set_car(ContextRef ctx, struct Value pair, struct Value car);

void scm_set_cdr(ContextRef ctx, struct Value pair, struct Value cdr);

void scm_vector_set(ContextRef ctx, struct Value vector, uintptr_t index, struct Value value);

void scm_tuple_set(ContextRef ctx, struct Value tuple, uintptr_t index, struct Value value);

void scm_string_set(ContextRef ctx, struct Value string, uintptr_t index, uint32_t ch);

uint32_t scm_string_ref(ContextRef _ctx, struct Value string, uintptr_t index);

int scm_call(ScmRef scm,
             const char *mod_name,
             const char *func_name,
             PrepareCallFn prepare,
             void *data1,
             FinishCallFn finish,
             void *data2);

int scm_resume_accumulator(ScmRef scm,
                           PrepareCallFn prepare,
                           void *data1,
                           FinishCallFn finish,
                           void *data2);

int scm_load_file(ScmRef scm, const char *name);

struct Value scm_program_arguments(ContextRef ctx);

void scm_program_arguments_init(ContextRef ctx, uintptr_t argc, const char *const *argv);

void scm_set_program_arguments(ContextRef ctx, struct Value args);
