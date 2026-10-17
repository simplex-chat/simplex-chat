#define FS_NAMESPACE rts
#include "fs.h"
#include <windows.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <io.h>
#include <share.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

enum { PATH_CAPACITY = 32768 };
#ifdef O_NOCTTY
#define PROBE_O_NOCTTY O_NOCTTY
#else
#define PROBE_O_NOCTTY 0
#endif
#ifdef O_NONBLOCK
#define PROBE_O_NONBLOCK O_NONBLOCK
#else
#define PROBE_O_NONBLOCK 0
#endif

static void reset_errors(void) {
    errno = 0;
    SetLastError(ERROR_SUCCESS);
}

static void print_path(const char *label, const wchar_t *path) {
    int bytes = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, path, -1,
                                   NULL, 0, NULL, NULL);
    if (bytes <= 0) {
        printf("path %s utf8-error=%lu\n", label, GetLastError());
        return;
    }
    char *text = malloc((size_t)bytes);
    if (text == NULL) {
        fputs("path logging allocation failed\n", stderr);
        exit(2);
    }
    if (WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, path, -1,
                            text, bytes, NULL, NULL) != bytes) {
        free(text);
        fputs("path logging conversion failed\n", stderr);
        exit(2);
    }
    printf("path %s chars=%zu value=%s\n", label, wcslen(path), text);
    free(text);
}

/* Replay the real Win32/CRT boundaries independently of __rts_swopen.
 * Their outcomes do not instrument or identify its opaque internal branch. */
static bool direct_open(const wchar_t *path) {
    SECURITY_ATTRIBUTES attributes = {0};
    attributes.nLength = sizeof(attributes);
    attributes.bInheritHandle = FALSE;
    reset_errors();
    HANDLE handle = CreateFileW(path, GENERIC_READ,
                               FILE_SHARE_READ | FILE_SHARE_WRITE,
                               &attributes, OPEN_EXISTING,
                               FILE_ATTRIBUTE_NORMAL, NULL);
    DWORD error = GetLastError();
    int saved_errno = errno;
    printf("direct CreateFileW handle=%p invalid=%d errno=%d winerror=%lu\n",
           (void *)handle, handle == INVALID_HANDLE_VALUE, saved_errno, error);
    fflush(stdout);
    if (handle == INVALID_HANDLE_VALUE) {
        return false;
    }

    reset_errors();
    int fd = _open_osfhandle((intptr_t)handle, _O_RDONLY);
    error = GetLastError();
    saved_errno = errno;
    printf("direct _open_osfhandle fd=%d errno=%d winerror=%lu\n",
           fd, saved_errno, error);
    fflush(stdout);
    if (fd < 0) {
        CloseHandle(handle);
        return false;
    }
    reset_errors();
    intptr_t roundtrip = _get_osfhandle(fd);
    error = GetLastError();
    saved_errno = errno;
    printf("direct _get_osfhandle value=0x%" PRIxPTR
           " same=%d errno=%d winerror=%lu\n",
           (uintptr_t)roundtrip, roundtrip == (intptr_t)handle,
           saved_errno, error);
    reset_errors();
    int mode = _setmode(fd, _O_BINARY);
    error = GetLastError();
    saved_errno = errno;
    printf("direct _setmode previous=%d errno=%d winerror=%lu\n",
           mode, saved_errno, error);
    fflush(stdout);
    bool passed = roundtrip == (intptr_t)handle && mode != -1;
    if (mode != -1) {
        unsigned char byte = 0;
        reset_errors();
        int count = _read(fd, &byte, 1);
        error = GetLastError();
        saved_errno = errno;
        printf("direct _read count=%d first=%u errno=%d winerror=%lu\n",
               count, (unsigned int)byte, saved_errno, error);
        passed = passed && count >= 0;
    }
    int closed = _close(fd); /* The descriptor owns the Win32 handle. */
    printf("direct _close result=%d\n", closed);
    return passed && closed == 0;
}

static bool check_variant(const char *label, const wchar_t *path, int expected_first, bool *fs_readable) {
    *fs_readable = false;
    printf("BEGIN %s\n", label);
    print_path(label, path);
    fflush(stdout);
    reset_errors();
    wchar_t *converted = FS(create_device_name)(path);
    DWORD helper_error = GetLastError();
    int helper_errno = errno;
    printf("helper null=%d errno=%d winerror=%lu\n",
           converted == NULL, helper_errno, helper_error);
    if (converted != NULL) {
        print_path("helper-result", converted);
    }
    fflush(stdout);
    bool direct_passed = direct_open(path);
    if (converted != NULL) {
        puts("helper-result direct boundary replay");
        fflush(stdout);
        (void)direct_open(converted);
    }
    free(converted);

    puts("before actual __rts_swopen");
    fflush(stdout);
    reset_errors();
    int fd = FS(swopen)(path, _O_RDONLY | _O_BINARY | _O_NOINHERIT
                       | PROBE_O_NOCTTY | PROBE_O_NONBLOCK,
                       _SH_DENYNO, 0666);
    DWORD error = GetLastError();
    int saved_errno = errno;
    printf("actual __rts_swopen fd=%d errno=%d winerror=%lu\n",
           fd, saved_errno, error);
    if (fd >= 0) {
        reset_errors();
        intptr_t handle = _get_osfhandle(fd);
        error = GetLastError();
        saved_errno = errno;
        printf("actual _get_osfhandle value=0x%" PRIxPTR
               " errno=%d winerror=%lu\n", (uintptr_t)handle,
               saved_errno, error);
        struct __stat64 stat_buffer = {0};
        reset_errors();
        int stat_result = _fstat64(fd, &stat_buffer);
        error = GetLastError();
        saved_errno = errno;
        printf("actual _fstat64 result=%d size=%" PRId64
               " mode=%lu errno=%d winerror=%lu\n", stat_result,
               (int64_t)stat_buffer.st_size, (unsigned long)stat_buffer.st_mode,
               saved_errno, error);
        /* Match HsBase.h's struct_stat and __hscore_fstat route as well. */
        struct _stati64 haskell_stat = {0};
        reset_errors();
        int haskell_stat_result = _fstati64(fd, &haskell_stat);
        error = GetLastError();
        saved_errno = errno;
        printf("actual _fstati64 result=%d size=%" PRId64
               " mode=%lu struct-bytes=%zu dev=%lu ino=%lu errno=%d winerror=%lu\n",
               haskell_stat_result, (int64_t)haskell_stat.st_size,
               (unsigned long)haskell_stat.st_mode,
               sizeof(haskell_stat), (unsigned long)haskell_stat.st_dev,
               (unsigned long)haskell_stat.st_ino, saved_errno, error);
        BY_HANDLE_FILE_INFORMATION info = {0};
        reset_errors();
        BOOL info_result = GetFileInformationByHandle((HANDLE)handle, &info);
        error = GetLastError();
        saved_errno = errno;
        printf("actual GetFileInformationByHandle result=%d volume=%lu"
               " index-high=%lu index-low=%lu size-high=%lu size-low=%lu"
               " errno=%d winerror=%lu\n", (int)info_result,
               info.dwVolumeSerialNumber, info.nFileIndexHigh,
               info.nFileIndexLow, info.nFileSizeHigh, info.nFileSizeLow,
               saved_errno, error);
        reset_errors();
        int mode = _setmode(fd, _O_BINARY);
        error = GetLastError();
        saved_errno = errno;
        printf("actual _setmode previous=%d errno=%d winerror=%lu\n",
               mode, saved_errno, error);
        unsigned char byte = 0;
        reset_errors();
        int count = _read(fd, &byte, 1);
        error = GetLastError();
        saved_errno = errno;
        printf("actual _read count=%d first=%u errno=%d winerror=%lu\n",
               count, (unsigned int)byte, saved_errno, error);
        int closed = _close(fd);
        printf("actual _close result=%d\n", closed);
        *fs_readable = handle != -1 && stat_result == 0
                       && haskell_stat_result == 0 && info_result
                       && haskell_stat.st_size == stat_buffer.st_size
                       && (stat_buffer.st_mode & _S_IFMT) == _S_IFREG
                       && (haskell_stat.st_mode & _S_IFMT) == _S_IFREG
                       && stat_buffer.st_size > 0 && mode != -1
                       && count == 1 && closed == 0
                       && (expected_first < 0
                           || (byte == (unsigned int)expected_first
                               && stat_buffer.st_size == 2));
    }
    printf("END %s\n", label);
    fflush(stdout);
    /* Preserve helper/swopen failures as observations. Direct read success is
     * the independent prerequisite that makes an existing-file case useful. */
    return direct_passed;
}

static int check_path(const char *label, const char *utf8, bool strict, int expected_first) {
    wchar_t *path = calloc(PATH_CAPACITY, sizeof(wchar_t));
    wchar_t *absolute = calloc(PATH_CAPACITY, sizeof(wchar_t));
    wchar_t *extended = calloc(PATH_CAPACITY, sizeof(wchar_t));
    if (path == NULL || absolute == NULL || extended == NULL) {
        free(path);
        free(absolute);
        free(extended);
        return 2;
    }
    if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, utf8, -1,
                             path, PATH_CAPACITY)) {
        free(path);
        free(absolute);
        free(extended);
        return 2;
    }
    printf("CASE %s\n", label);
    bool fs_ok = false;
    bool readable = check_variant("original", path, expected_first, &fs_ok);
    bool all_fs_ok = fs_ok;
    reset_errors();
    DWORD count = GetFullPathNameW(path, PATH_CAPACITY, absolute, NULL);
    DWORD error = GetLastError();
    printf("resolve count=%lu capacity=%d winerror=%lu\n",
           count, PATH_CAPACITY, error);
    fflush(stdout);
    if (count > 0 && count < PATH_CAPACITY) {
        readable = check_variant("drive-absolute", absolute, expected_first, &fs_ok) || readable;
        all_fs_ok = all_fs_ok && fs_ok;
        if (count >= 3 && absolute[1] == L':' && absolute[2] == L'\\') {
            /* The canonical /nix/store input is rooted on the current drive.
             * Exercise that form on the same owned file without store writes. */
            wmemcpy(path, absolute + 2, (size_t)count - 1);
            for (size_t i = 0; path[i] != L'\0'; ++i) {
                if (path[i] == L'\\') { path[i] = L'/'; }
            }
            readable = check_variant("slash-rooted", path, expected_first, &fs_ok) || readable;
            all_fs_ok = all_fs_ok && fs_ok;
        } else {
            puts("ENVIRONMENT: owned fixture has no drive-rooted representation");
            all_fs_ok = false;
        }
        size_t prefix_length = 4;
        size_t offset = 0;
        const wchar_t *prefix = L"\\\\?\\";
        if (count >= 2 && absolute[0] == L'\\' && absolute[1] == L'\\') {
            prefix = L"\\\\?\\UNC\\";
            prefix_length = 8;
            offset = 2;
        }
        if ((size_t)count + prefix_length - offset < PATH_CAPACITY) {
            wmemcpy(extended, prefix, prefix_length);
            wmemcpy(extended + prefix_length, absolute + offset,
                    (size_t)count + 1 - offset);
            readable = check_variant("extended", extended, expected_first, &fs_ok) || readable;
            all_fs_ok = all_fs_ok && fs_ok;
        } else {
            all_fs_ok = false;
        }
    } else {
        all_fs_ok = false;
    }
    free(path);
    free(absolute);
    free(extended);
    /* Any successful real direct read proves the fixture exists. Raw long
     * paths can fail under MAX_PATH policy even when extended paths work. */
    printf("CASE-RESULT %s readable-prerequisite=%d all-FS-variants=%d strict=%d\n",
           label, readable, all_fs_ok, strict);
    fflush(stdout);
    if (!readable) {
        fputs("ENVIRONMENT: no direct variant could read the existing fixture\n", stderr);
        return 2;
    }
    return !strict || all_fs_ok ? 0 : 1;
}

int main(int argc, char **argv) {
    puts("PROBE_MAIN_STARTED: real Windows filesystem APIs");
    fflush(stdout);
    bool strict = argc > 1 && strcmp(argv[1], "--strict") == 0;
    int first = strict ? 2 : 1;
    if (argc != first + 3 && argc != first + 4) {
        fputs("usage: probe.exe [--strict] SHORT LONG ARCHIVE_SHAPED [ACTUAL_ARCHIVE]\n", stderr);
        return 2;
    }
    printf("CRT __USE_MINGW_ANSI_STDIO=%d\n", __USE_MINGW_ANSI_STDIO);
    printf("FLAGS O_NOCTTY=%d O_NONBLOCK=%d O_BINARY=%d _O_NOINHERIT=%d\n",
           PROBE_O_NOCTTY, PROBE_O_NONBLOCK, O_BINARY, _O_NOINHERIT);
#ifdef _UCRT
    puts("CRT _UCRT=defined");
#else
    puts("CRT _UCRT=undefined");
#endif
    fflush(stdout);
    int status = check_path("short", argv[first], strict, 1);
    int current = check_path("long", argv[first + 1], strict, 1);
    if (current > status) { status = current; }
    current = check_path("archive-shaped", argv[first + 2], strict, 1);
    if (current > status) { status = current; }
    if (argc == first + 4) {
        current = check_path("actual-archive", argv[first + 3], strict, -1);
        if (current > status) { status = current; }
    }
    printf("RESULT mode=%s %s\n", strict ? "strict-FS-read" : "observations",
           status == 0 ? "PASS" : status == 2 ? "ENVIRONMENT-FAILURE" : "FS-REGRESSION-FAILURE");
    return status;
}
