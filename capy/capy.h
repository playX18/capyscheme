#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

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

typedef struct State State;

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

typedef struct Context {
  const Mutation *mc;
  const struct State *state;
} Context;

/**
 * Create a new Scheme thread instance.
 *
 * The thread instance will be attached to currently active VM or
 * will initialize a new VM if there isn't any.
 *
 * For thread creation look at [`scm_fork()`](scm_fork).
 */
ScmRef scm_new(void);

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

struct Value scm_public_ref(const struct Context *ctx,
                            const char *module_name,
                            const char *name,
                            struct Value default_value);

struct Value scm_private_ref(const struct Context *ctx,
                             const char *module_name,
                             const char *name,
                             struct Value default_value);
