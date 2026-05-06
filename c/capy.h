#include <inttypes.h>

typedef struct HeapTypeInfo {
    uint64_t vtable;
    uint16_t type_bits;
} HeapTypeInfo;
#include "../capy/capy-priv.h"

#define VALUE_NULL ((struct Value){.desc = {.as_u64 = Value_VALUE_NULL}})