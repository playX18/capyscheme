/*
 * capy_sched.h: threads for native code linked against libcapy.
 *
 * Every function here is callable from a thread, or from an OS thread that
 * called capy_sched_attach() first. Calls from other threads return an error
 * code. Panics inside the library are caught and reported as error codes.
 *
 * A blocking call that is interrupted by a kill returns CAPY_THREAD_KILLED (2)
 * and does not unwind. Return from your thread promptly once you see it.
 * The library then unwinds the thread, running its destructors and
 * dynamic-wind after-thunks.
 *
 * Do not touch the Scheme heap from a callback except through the SNI API,
 * which keeps the thread pinned to its worker while it runs.
 */
#ifndef CAPY_SCHED_H
#define CAPY_SCHED_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Results of capy_thread_join and of the blocking calls. */
#define CAPY_THREAD_OK 0
#define CAPY_THREAD_PANICKED 1
#define CAPY_THREAD_KILLED 2

/* Event bits for capy_thread_await_fd. Pass exactly one. */
#define CAPY_THREAD_READABLE 1
#define CAPY_THREAD_WRITABLE 2

/* capy_thread_await_fd results. -1 means error, with errno set. */
#define CAPY_THREAD_FD_READY 0
#define CAPY_THREAD_FD_TIMED_OUT 1
#define CAPY_THREAD_FD_CLOSED 3

/* Opaque thread handle. */
typedef struct capy_thread capy_thread;

/* Body of a thread. */
typedef void (*capy_thread_fn)(void *arg);

/*
 * Start a thread that calls fn(arg). stack_kb is the usable stack in KiB,
 * or 0 for the default. Returns NULL on failure (errno is set).
 */
capy_thread *capy_thread_spawn(capy_thread_fn fn, void *arg, size_t stack_kb);

/*
 * Wait for g to finish. Parks the caller when it is a thread, and blocks
 * the OS thread otherwise. Returns CAPY_THREAD_OK, CAPY_THREAD_PANICKED,
 * CAPY_THREAD_KILLED, or -1 on error. Joining the same handle twice is an error.
 */
int capy_thread_join(capy_thread *g);

/* Request that g be killed. Idempotent. Does not wait. */
void capy_thread_kill(capy_thread *g);

/* Release the handle. The thread keeps running, detached. */
void capy_thread_free(capy_thread *g);

/* Id of the running thread, or 0 when the caller is not a thread. */
uint64_t capy_thread_id(void);

/* Yield to other threads. Returns CAPY_THREAD_OK, CAPY_THREAD_KILLED, or -1. */
int capy_thread_yield(void);

/* Sleep for ms milliseconds. Returns CAPY_THREAD_OK, CAPY_THREAD_KILLED, or -1. */
int capy_thread_sleep_ms(uint64_t ms);

/*
 * Wait until fd is ready for events (CAPY_THREAD_READABLE or CAPY_THREAD_WRITABLE).
 * A negative timeout_ms waits forever. Returns CAPY_THREAD_FD_READY,
 * CAPY_THREAD_FD_TIMED_OUT, CAPY_THREAD_FD_CLOSED, CAPY_THREAD_KILLED, or -1 with errno set.
 */
int capy_thread_await_fd(int fd, int events, int64_t timeout_ms);

/* Nonzero when the running thread has been killed. */
int capy_thread_is_killed(void);

/*
 * Bind the calling OS thread so it may call this API. Idempotent. Returns 0,
 * or -1 on failure. Call capy_sched_detach before the thread exits.
 */
int capy_sched_attach(void);

/* Unbind a thread bound by capy_sched_attach. Returns 0, or -1 if it was not bound. */
int capy_sched_detach(void);

#ifdef __cplusplus
}
#endif

#endif /* CAPY_SCHED_H */
