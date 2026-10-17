/* Diagnostic-only Darwin SIGBUS record. Never load this in a canonical build. */
#define _DARWIN_C_SOURCE 1

#include <errno.h>
#include <fcntl.h>
#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/ucontext.h>
#include <unistd.h>

#define RECORD_MAGIC UINT64_C(0x5349474255533031)
#define RECORD_WORDS 64

#if defined(__x86_64__)
#define RECORD_CPU CPU_TYPE_X86_64
#elif defined(__aarch64__) || defined(__arm64__)
#define RECORD_CPU CPU_TYPE_ARM64
#else
#error "The SIGBUS diagnostic requires 64-bit Darwin"
#endif

/* This is the only static value read from the signal handler. */
_Static_assert(ATOMIC_INT_LOCK_FREE == 2, "The handler requires lock-free int atomics");
static atomic_int record_fd = ATOMIC_VAR_INIT(-1);

static void fail(void)
{
    static const char message[] = "GHC SIGBUS diagnostic: initialization failed\n";
    (void)write(STDERR_FILENO, message, sizeof(message) - 1);
    _exit(125);
}

static void fault_record(int signal_number, siginfo_t *information, void *context)
{
    /* No formatter, allocator, unwinder, dyld call, or fault-address read here.
     * A short/interrupted write is reported as a truncated record by the reader.
     */
    uint64_t record[RECORD_WORDS] = {0};
    record[0] = RECORD_MAGIC;
    record[1] = 1;
    record[2] = 2;
    record[3] = RECORD_CPU;
    record[4] = (uint64_t)signal_number;
    if (information != NULL) {
        record[5] = (uint64_t)(int64_t)information->si_code;
        record[6] = (uint64_t)(uintptr_t)information->si_addr;
    }
    ucontext_t *machine = context;
    if (machine != NULL && machine->uc_mcontext != NULL) {
#if defined(__x86_64__)
        const struct __darwin_x86_thread_state64 *state = &machine->uc_mcontext->__ss;
        record[7] = state->__rip;
        record[8] = state->__rsp;
        record[9] = state->__rbp;
        record[11] = state->__rflags;
        record[12] = state->__rax;
        record[13] = state->__rbx;
        record[14] = state->__rcx;
        record[15] = state->__rdx;
        record[16] = state->__rdi;
        record[17] = state->__rsi;
        record[18] = state->__r8;
        record[19] = state->__r9;
        record[20] = state->__r10;
        record[21] = state->__r11;
        record[22] = state->__r12;
        record[23] = state->__r13;
        record[24] = state->__r14;
        record[25] = state->__r15;
#else
        const struct __darwin_arm_thread_state64 *state = &machine->uc_mcontext->__ss;
        record[7] = state->__pc;
        record[8] = state->__sp;
        record[9] = state->__fp;
        record[10] = state->__lr;
        record[11] = state->__cpsr;
        for (unsigned int index = 0; index < 29; ++index) {
            record[12 + index] = state->__x[index];
        }
#endif
    }
    (void)write(atomic_load_explicit(&record_fd, memory_order_relaxed), record, sizeof(record));
    /* SA_RESETHAND restored SIG_DFL. Queue the same signal while it is blocked,
     * then return for default termination. Darwin raise() can report the same
     * si_code as a hardware fault, so returning alone could swallow that signal.
     * The record above retains the original fault address and machine context.
     */
    (void)kill(getpid(), signal_number);
}

__attribute__((constructor)) static void initialize(void)
{
    const char *path = getenv("SIMPLEX_GHC_BUS_RECORD");
    const char *present = getenv("SIMPLEX_GHC_BUS_DYLD_PRESENT");
    const char *previous = getenv("SIMPLEX_GHC_BUS_DYLD_VALUE");
    if (path == NULL || path[0] != '/' || present == NULL ||
        (strcmp(present, "0") != 0 && strcmp(present, "1") != 0) ||
        (strcmp(present, "1") == 0 && previous == NULL)) {
        fail();
    }
    const int had_previous = strcmp(present, "1") == 0;
    char *previous_copy = had_previous ? strdup(previous) : NULL;
    if (had_previous && previous_copy == NULL) {
        fail();
    }
    int descriptor = open(path, O_WRONLY | O_CREAT | O_EXCL | O_APPEND | O_CLOEXEC, 0600);
    if (descriptor < 0) {
        fail();
    }
    atomic_store_explicit(&record_fd, descriptor, memory_order_relaxed);
    int restored = had_previous ? setenv("DYLD_INSERT_LIBRARIES", previous_copy, 1)
                                : unsetenv("DYLD_INSERT_LIBRARIES");
    free(previous_copy);
    if (restored != 0 || unsetenv("SIMPLEX_GHC_BUS_RECORD") != 0 ||
        unsetenv("SIMPLEX_GHC_BUS_DYLD_PRESENT") != 0 ||
        unsetenv("SIMPLEX_GHC_BUS_DYLD_VALUE") != 0) {
        fail();
    }
    struct rlimit core_limit;
    if (getrlimit(RLIMIT_CORE, &core_limit) != 0) {
        fail();
    }
    core_limit.rlim_cur = 0;
    if (setrlimit(RLIMIT_CORE, &core_limit) != 0) {
        fail();
    }
    struct sigaction previous_action;
    if (sigaction(SIGBUS, NULL, &previous_action) != 0 || previous_action.sa_handler != SIG_DFL) {
        fail();
    }

    /* Startup metadata is captured outside the signal handler. */
    uint64_t record[RECORD_WORDS] = {0};
    record[0] = RECORD_MAGIC;
    record[1] = 1;
    record[2] = 1;
    record[3] = RECORD_CPU;
    record[4] = (uint64_t)getpid();
    const struct mach_header_64 *header = NULL;
    intptr_t slide = 0;
    uint32_t image_count = _dyld_image_count();
    if (image_count > 1024) {
        fail();
    }
    for (uint32_t index = 0; index < image_count; ++index) {
        const struct mach_header *image = _dyld_get_image_header(index);
        if (image != NULL && image->filetype == MH_EXECUTE) {
            if (header != NULL) {
                fail();
            }
            header = (const struct mach_header_64 *)image;
            slide = _dyld_get_image_vmaddr_slide(index);
        }
    }
    if (header == NULL || header->magic != MH_MAGIC_64 ||
        header->cputype != RECORD_CPU || header->sizeofcmds > 1024 * 1024 ||
        header->ncmds > 1024) {
        fail();
    }
    record[5] = (uint64_t)(uintptr_t)header;
    record[6] = (uint64_t)slide;
    const unsigned char *commands = (const unsigned char *)(header + 1);
    size_t offset = 0;
    for (uint32_t index = 0; index < header->ncmds; ++index) {
        if (offset > header->sizeofcmds || header->sizeofcmds - offset < sizeof(struct load_command)) {
            fail();
        }
        const struct load_command *command = (const struct load_command *)(commands + offset);
        if (command->cmdsize < sizeof(*command) || command->cmdsize > header->sizeofcmds - offset) {
            fail();
        }
        if (command->cmd == LC_UUID) {
            if (command->cmdsize != sizeof(struct uuid_command) || record[7] != 0) {
                fail();
            }
            const struct uuid_command *uuid = (const struct uuid_command *)command;
            memcpy(&record[8], uuid->uuid, sizeof(uuid->uuid));
            record[7] = 1;
        }
        offset += command->cmdsize;
    }
    if (write(descriptor, record, sizeof(record)) != (ssize_t)sizeof(record)) {
        fail();
    }
    struct sigaction action = {0};
    action.sa_sigaction = fault_record;
    action.sa_flags = SA_SIGINFO | SA_RESETHAND;
    if (sigemptyset(&action.sa_mask) != 0 || sigaction(SIGBUS, &action, NULL) != 0) {
        fail();
    }
}
