/* Constants declared as objects rather than macros, the way webgpu.h declares its flags. */
#include <stdint.h>

typedef uint64_t Flags;
enum Mode { MODE_A = 1, MODE_B = 4 };

static const Flags FLAG_NONE = 0x0000000000000000;
static const Flags FLAG_READ = 0x0000000000000001;
static const Flags FLAG_WRITE = 0x0000000000000002;
static const Flags FLAG_RW = FLAG_READ | FLAG_WRITE;
static const Flags FLAG_HIGH = (Flags)1 << 40;
static const uint32_t ALL_BITS = -1;
static const enum Mode DEFAULT_MODE = MODE_B;
static const int SHIFTED = MODE_A << 3;
static const double HALF = 0.5;
static const float THIRD = 1.0f / 3.0f;
static void *const NOTHING = 0;
static const char GREETING[] = "hello";
const int PLAIN = SHIFTED + 1;

/* Not constants: skipped. */
static int counter = 0;
static const int FROM_COUNTER = sizeof(counter);
