#include <setjmp.h>
#include <stddef.h>
#include <stdlib.h>

/* Opaque buffer large enough for any supported platform's jmp_buf. */
typedef struct {
    unsigned char data[256];
} capy_jmp_buf;

typedef void (*capy_scheme_entry_fn)(void *data);

/*
 * Install `env` as the setjmp target, call `entry(data)`, and return after a
 * matching longjmp. From Rust's perspective this function returns only once
 * (after longjmp); the returns-twice behavior of setjmp stays inside C.
 */
int capy_with_setjmp(capy_jmp_buf *env, capy_scheme_entry_fn entry, void *data) {
    if (setjmp(*(jmp_buf *)env) == 0) {
        entry(data);
        /* Scheme entry must exit via longjmp. */
        abort();
    }
    return 1;
}

void capy_longjmp(capy_jmp_buf *env, int val) {
    longjmp(*(jmp_buf *)env, val == 0 ? 1 : val);
}
