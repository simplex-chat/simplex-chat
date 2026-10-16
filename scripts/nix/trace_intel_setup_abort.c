/* Diagnostic-only abort interposer. Never loaded by the canonical build. */
#define _DARWIN_C_SOURCE 1

#include <dlfcn.h>
#include <errno.h>
#include <inttypes.h>
#include <libunwind.h>
#include <mach-o/dyld.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>

static void line(const char *format, ...)
{
    char buffer[2048];
    va_list arguments;
    va_start(arguments, format);
    int count = vsnprintf(buffer, sizeof(buffer), format, arguments);
    va_end(arguments);
    if (count < 0) {
        return;
    }
    size_t length = (size_t)count;
    if (length >= sizeof(buffer)) {
        length = sizeof(buffer) - 1;
        buffer[length - 1] = '\n';
    }
    size_t written = 0;
    while (written < length) {
        ssize_t result = write(STDERR_FILENO, buffer + written, length - written);
        if (result > 0) {
            written += (size_t)result;
        } else if (result < 0 && errno == EINTR) {
            continue;
        } else {
            break;
        }
    }
}

__attribute__((constructor)) static void restore_child_environment(void)
{
    const char *present = getenv("SIMPLEX_SETUP_ABORT_DYLD_PRESENT");
    const char *previous = getenv("SIMPLEX_SETUP_ABORT_DYLD_VALUE");
    bool valid = present != NULL &&
        (strcmp(present, "0") == 0 || strcmp(present, "1") == 0);
    if (!valid || (strcmp(present, "1") == 0 && previous == NULL)) {
        line("Intel Setup abort tracer: invalid private loader metadata\n");
        _exit(125);
    }
    int restored = strcmp(present, "1") == 0
        ? setenv("DYLD_INSERT_LIBRARIES", previous, 1)
        : unsetenv("DYLD_INSERT_LIBRARIES");
    if (restored != 0 || unsetenv("SIMPLEX_SETUP_ABORT_DYLD_PRESENT") != 0 ||
        unsetenv("SIMPLEX_SETUP_ABORT_DYLD_VALUE") != 0) {
        line("Intel Setup abort tracer: cannot restore child loader environment\n");
        _exit(125);
    }
    /* The fallback reports addresses, never a core or arbitrary memory. */
    struct rlimit limit;
    if (getrlimit(RLIMIT_CORE, &limit) != 0) {
        line("Intel Setup abort tracer: cannot read core limit\n");
        _exit(125);
    }
    limit.rlim_cur = 0;
    if (setrlimit(RLIMIT_CORE, &limit) != 0) {
        line("Intel Setup abort tracer: cannot disable diagnostic core output\n");
        _exit(125);
    }
    line("Intel Setup abort tracer loaded; child loader environment restored; core output disabled\n");
}

static void address_details(uintptr_t value)
{
    /* Resolve an address without reading any pointed-to closure data. */
    Dl_info address = {0};
    if (dladdr((const void *)value, &address) != 0) {
        uintptr_t start = (uintptr_t)address.dli_saddr;
        bool named = address.dli_sname != NULL && address.dli_saddr != NULL && value >= start;
        line("    address image=%s base=%p symbol=%s+0x%" PRIxPTR "\n",
             address.dli_fname != NULL ? address.dli_fname : "<unavailable>",
             address.dli_fbase, named ? address.dli_sname : "<unavailable>",
             named ? value - start : 0);
    }
}

static void saved_registers(unw_cursor_t *cursor)
{
#if defined(__x86_64__)
    static const int registers[] = {
        UNW_X86_64_RBX, UNW_X86_64_RBP, UNW_X86_64_R12,
        UNW_X86_64_R13, UNW_X86_64_R14, UNW_X86_64_R15
    };
    static const char *const names[] = { "rbx", "rbp", "r12", "r13", "r14", "r15" };
#elif defined(__aarch64__) || defined(__arm64__)
    static const int registers[] = {
        UNW_ARM64_X19, UNW_ARM64_X20, UNW_ARM64_X21, UNW_ARM64_X22,
        UNW_ARM64_X23, UNW_ARM64_X24, UNW_ARM64_X25, UNW_ARM64_X26,
        UNW_ARM64_X27, UNW_ARM64_X28, UNW_ARM64_FP, UNW_ARM64_LR
    };
    static const char *const names[] = {
        "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26",
        "x27", "x28", "fp", "lr"
    };
#else
#error "Intel Setup abort tracer requires a 64-bit Darwin target"
#endif
    for (size_t index = 0; index < sizeof(registers) / sizeof(registers[0]); ++index) {
        unw_word_t value = 0;
        int result = unw_get_reg(cursor, registers[index], &value);
        if (result == 0) {
            line("  saved %s=0x%" PRIxPTR "\n", names[index], (uintptr_t)value);
            address_details((uintptr_t)value);
        } else {
            line("  saved %s unavailable (%d)\n", names[index], result);
        }
    }
}

static void trace_frames(void)
{
    unw_context_t context;
    unw_cursor_t cursor;
    if (unw_getcontext(&context) != 0 || unw_init_local(&cursor, &context) != 0) {
        line("Intel Setup abort tracer: local unwind initialization failed\n");
        return;
    }
    for (unsigned int frame = 0; frame < 80; ++frame) {
        unw_word_t ip = 0;
        unw_word_t sp = 0;
        unw_word_t offset = 0;
        char symbol[512] = "<unavailable>";
        int ip_result = unw_get_reg(&cursor, UNW_REG_IP, &ip);
        int sp_result = unw_get_reg(&cursor, UNW_REG_SP, &sp);
        int symbol_result = unw_get_proc_name(&cursor, symbol, sizeof(symbol), &offset);
        symbol[sizeof(symbol) - 1] = '\0';
        Dl_info image = {0};
        int image_result = ip_result == 0 ? dladdr((const void *)(uintptr_t)ip, &image) : 0;
        line("Intel Setup abort frame %u ip=0x%" PRIxPTR " (%d) sp=0x%" PRIxPTR
             " (%d) symbol=%s+0x%" PRIxPTR " (%d) image=%s base=%p\n",
             frame, (uintptr_t)ip, ip_result, (uintptr_t)sp, sp_result,
             symbol, (uintptr_t)offset, symbol_result,
             image_result != 0 && image.dli_fname != NULL ? image.dli_fname : "<unavailable>",
             image_result != 0 ? image.dli_fbase : NULL);
        saved_registers(&cursor);
        int result = unw_step(&cursor);
        if (result <= 0) {
            line("Intel Setup abort unwind finished (%d)\n", result);
            return;
        }
    }
    line("Intel Setup abort backtrace truncated after 80 frames\n");
}

static void trace_images(void)
{
    uint32_t count = _dyld_image_count();
    uint32_t bound = count < 256 ? count : 256;
    for (uint32_t index = 0; index < bound; ++index) {
        const char *name = _dyld_get_image_name(index);
        line("Intel Setup abort image %u base=%p slide=0x%" PRIxPTR " path=%s\n",
             index, (const void *)_dyld_get_image_header(index),
             (uintptr_t)_dyld_get_image_vmaddr_slide(index),
             name != NULL ? name : "<unavailable>");
    }
    if (bound != count) {
        line("Intel Setup abort images truncated at 256 of %u\n", count);
    }
}

/* Avoid overlapping or recursive logging if a failed unwind itself aborts. */
static atomic_flag tracing = ATOMIC_FLAG_INIT;

_Noreturn static void trace_abort(void)
{
    if (!atomic_flag_test_and_set_explicit(&tracing, memory_order_relaxed)) {
        line("Intel Setup abort tracer: abort called; current-thread unwind is best-effort\n");
        trace_frames();
        trace_images();
    }
    /* dyld excludes the interposing image's own reference, calling real abort. */
    abort();
}

/* Darwin's __interpose ABI is two pointer-sized words; no function/data cast. */
__attribute__((used, section("__DATA,__interpose")))
static const struct {
    void (*replacement)(void);
    void (*original)(void);
} abort_interpose = { trace_abort, abort };
