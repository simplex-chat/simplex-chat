/* SPDX-License-Identifier: AGPL-3.0-only */
/* Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd. */

#include <inttypes.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/utsname.h>
#include <sys/personality.h>
#include <sys/wait.h>
#include <unistd.h>

static uint32_t current_persona(void)
{
    return (uint32_t)personality(UINT32_MAX);
}

static int equal_persona(uint32_t actual, uint32_t expected)
{
    if (actual == expected) {
        return 0;
    }
    (void)fprintf(stderr, "persona got=%08" PRIx32 " expected=%08" PRIx32 "\n",
                  actual, expected);
    return 1;
}

struct thread_case {
    uint32_t initial;
    int result;
};

static void *check_thread(void *opaque)
{
    struct thread_case *test = opaque;
    uint32_t changed;

    if (test == NULL) {
        return NULL;
    }
    changed = test->initial ^ UNAME26;
    test->result = equal_persona(current_persona(), test->initial);
    test->result |= equal_persona((uint32_t)personality(changed), test->initial);
    test->result |= equal_persona(current_persona(), changed);
    return NULL;
}

int main(void)
{
    uint32_t initial = current_persona();
    uint32_t changed = initial ^ UNAME26;
    uint32_t linux_domain = (initial & ~(uint32_t)PER_MASK) | PER_LINUX;
    struct thread_case test = { initial, 1 };
    pthread_t thread;
    pid_t child;
    int status;
    struct utsname names;

    if ((initial & PER_MASK) != PER_LINUX32) {
        (void)fprintf(stderr, "Android startup did not retain PER_LINUX32\n");
        return 10;
    }
    if (equal_persona((uint32_t)personality(changed), initial) ||
        equal_persona(current_persona(), changed) || uname(&names) != 0) {
        return 11;
    }
    (void)fprintf(stderr, "UNAME26=%u release=%s\n",
                  (changed & UNAME26) != 0 ? 1U : 0U, names.release);
    if (((changed & UNAME26) != 0 &&
         strncmp(names.release, "2.6.", 4) != 0) ||
        equal_persona((uint32_t)personality(initial), changed)) {
        return 11;
    }
    if (pthread_create(&thread, NULL, check_thread, &test) != 0) {
        return 12;
    }
    if (pthread_join(thread, NULL) != 0 || test.result != 0 ||
        equal_persona(current_persona(), initial)) {
        return 13;
    }
    child = fork();
    if (child < 0) {
        return 14;
    }
    if (child == 0) {
        int result = equal_persona(current_persona(), initial);
        result |= equal_persona((uint32_t)personality(linux_domain), initial);
        result |= equal_persona(current_persona(), linux_domain);
        _exit(result == 0 ? 0 : 15);
    }
    if (waitpid(child, &status, 0) != child || !WIFEXITED(status) ||
        WEXITSTATUS(status) != 0 || equal_persona(current_persona(), initial)) {
        return 16;
    }
    if (equal_persona((uint32_t)personality(linux_domain), initial) ||
        equal_persona(current_persona(), linux_domain) ||
        equal_persona((uint32_t)personality(initial), linux_domain) ||
        equal_persona(current_persona(), initial)) {
        return 17;
    }
    if (fputs("SIMPLEX_ARMV7_PERSONALITY_CHECK_PASS\n", stderr) < 0) {
        return 18;
    }
    return 0;
}
