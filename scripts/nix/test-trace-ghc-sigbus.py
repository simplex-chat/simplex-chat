#!/usr/bin/env python3
"""Owned native Darwin controls; cross-compile x86_64 without executing it."""

import importlib.util
import hashlib
import json
import os
from pathlib import Path
import platform
import signal
import struct
import subprocess
import tempfile
import unittest
from unittest import mock


HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("sigbus", HERE / "trace-ghc-sigbus.py")
SIGBUS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(SIGBUS)

FIXTURE = r'''
#define _DARWIN_C_SOURCE 1
#include <fcntl.h>
#include <mach-o/dyld.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>

static int environment_ok(const char *expected)
{
    if (getenv("SIMPLEX_GHC_BUS_RECORD") != NULL ||
        getenv("SIMPLEX_GHC_BUS_DYLD_PRESENT") != NULL ||
        getenv("SIMPLEX_GHC_BUS_DYLD_VALUE") != NULL) return 0;
    const char *value = getenv("DYLD_INSERT_LIBRARIES");
    return strcmp(expected, "<unset>") == 0 ? value == NULL
        : value != NULL && strcmp(expected, value) == 0;
}

int main(int argc, char **argv)
{
    if (argc < 2) return 120;
    if (strcmp(argv[1], "0") == 0 || strcmp(argv[1], "7") == 0) {
        printf("argv0=%s argc=%d last=%s\n", argv[0], argc, argv[argc - 1]);
        return atoi(argv[1]);
    }
    if (strcmp(argv[1], "raise") == 0) {
        if (raise(SIGBUS) != 0) return 121;
        return 122;
    }
    if (strcmp(argv[1], "mmap") == 0 && argc == 3) {
        long page = sysconf(_SC_PAGESIZE);
        int descriptor = open(argv[2], O_RDWR | O_CREAT | O_EXCL, 0600);
        if (page <= 0 || descriptor < 0 || ftruncate(descriptor, page) != 0) return 123;
        void *memory = mmap(NULL, (size_t)page, PROT_READ, MAP_SHARED, descriptor, 0);
        if (memory == MAP_FAILED || ftruncate(descriptor, 0) != 0) return 124;
        volatile const unsigned char *bytes = memory;
        return bytes[0];
    }
    if (strcmp(argv[1], "child-check") == 0 && argc == 3) {
        if (!environment_ok(argv[2])) return 126;
        for (uint32_t index = 0; index < _dyld_image_count(); ++index) {
            const char *name = _dyld_get_image_name(index);
            if (name != NULL && strstr(name, "trace-arm64.dylib") != NULL) return 127;
        }
        puts("CHILD_ENV_OK");
        return 0;
    }
    if (strcmp(argv[1], "child-env") == 0 && argc == 3) {
        if (!environment_ok(argv[2])) return 126;
        pid_t child = fork();
        if (child < 0) return 128;
        if (child == 0) {
            execl(argv[0], argv[0], "child-check", argv[2], (char *)NULL);
            _exit(129);
        }
        int status = 0;
        if (waitpid(child, &status, 0) != child || !WIFEXITED(status)) return 130;
        return WEXITSTATUS(status);
    }
    if (strcmp(argv[1], "sleep") == 0) { for (;;) pause(); }
    return 131;
}
'''


class ParserControls(unittest.TestCase):
    def test_bounded_regular_record_reads(self):
        with tempfile.TemporaryDirectory(prefix="ghc-sigbus-parser-") as directory:
            root = Path(directory)
            record = root / "record.bin"
            words = [SIGBUS.MAGIC, 1, 1, SIGBUS.CPUS["arm64"]] + [0] * 60
            record.write_bytes(struct.pack("<64Q", *words))
            with mock.patch.object(Path, "read_bytes", side_effect=AssertionError("Unbounded read")):
                self.assertEqual(SIGBUS.decode(record)["bytes"], 512)
                record.write_bytes(bytes(4 * 512 + 1))
                with self.assertRaisesRegex(ValueError, "byte limit"):
                    SIGBUS.decode(record)
            link = root / "record-link"
            link.symlink_to(record)
            with self.assertRaises(OSError):
                SIGBUS.decode(link)
            fifo = root / "record-fifo"
            os.mkfifo(fifo)
            with self.assertRaisesRegex(ValueError, "regular file"):
                SIGBUS.decode(fifo)

    def test_bounded_immutable_wrapper_reads(self):
        with tempfile.TemporaryDirectory(prefix="ghc-sigbus-parser-") as directory:
            root = Path(directory)
            binary, tracer = root / "ghc-fixture", root / "tracer.dylib"
            for path, kind in ((binary, 2), (tracer, 6)):
                path.write_bytes(struct.pack("<4I", 0xFEEDFACF, SIGBUS.CPUS["arm64"], 0, kind))
            source = root / "ghc-source"
            source.write_text('#!/bin/sh\nexec ' + str(binary) + ' "$@"\n')
            original_hash = hashlib.sha256(source.read_bytes()).hexdigest()
            with mock.patch.object(Path, "read_bytes", side_effect=AssertionError("Unbounded read")):
                report = SIGBUS.prepare(source, root / "copy", tracer, root / "record.bin",
                                        "arm64", binary)
                self.assertEqual(report["sourceSHA256"], original_hash)
                source.write_bytes(bytes(64 * 1024 + 1))
                with self.assertRaisesRegex(ValueError, "byte limit"):
                    SIGBUS.prepare(source, root / "large-copy", tracer, root / "large.bin",
                                   "arm64", binary)
            link = root / "source-link"
            link.symlink_to(source)
            with self.assertRaises(OSError):
                SIGBUS.prepare(link, root / "link-copy", tracer, root / "link.bin", "arm64", binary)


class Controls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if platform.system() != "Darwin" or platform.machine() != "arm64":
            raise unittest.SkipTest("This prototype control runs only native arm64 Darwin")
        cls.directory = tempfile.TemporaryDirectory(prefix="ghc-sigbus-controls-")
        cls.root = Path(cls.directory.name)
        cls.compiler = subprocess.check_output(["xcrun", "--find", "clang"], text=True).strip()
        cls.sdk = subprocess.check_output(["xcrun", "--show-sdk-path"], text=True).strip()
        cls.commands = []
        flags = ["-isysroot", cls.sdk, "-std=c11", "-Werror=vla", "-pedantic",
                 "-Wall", "-Wextra", "-Werror", "-O0", "-g"]
        for architecture in ("arm64", "x86_64"):
            command = [cls.compiler, *flags, "-arch", architecture, "-dynamiclib",
                       str(HERE / "trace_ghc_sigbus.c"), "-o",
                       str(cls.root / f"trace-{architecture}.dylib")]
            result = subprocess.run(command, capture_output=True, text=True, timeout=30)
            cls.commands.append({"argv": command, "exit": result.returncode, "stderr": result.stderr})
            if result.returncode != 0:
                raise AssertionError(result.stderr)
        fixture = cls.root / "Fixture.c"
        fixture.write_text(FIXTURE)
        cls.binary = cls.root / "ghc-fixture"
        subprocess.run([cls.compiler, *flags, "-arch", "arm64", str(fixture),
                        "-o", str(cls.binary)], check=True, capture_output=True, timeout=30)
        binary_data = cls.binary.read_bytes()
        command_count = struct.unpack_from("<I", binary_data, 16)[0]
        offset = 32
        cls.binary_uuid = None
        for _ in range(command_count):
            kind, size = struct.unpack_from("<II", binary_data, offset)
            if kind == 0x1B:
                cls.binary_uuid = binary_data[offset + 8:offset + 24].hex()
            offset += size
        if cls.binary_uuid is None:
            raise AssertionError("Fixture executable must contain LC_UUID")
        cls.source = cls.root / "ghc-original"
        cls.source.write_text('#!/bin/sh\nexec ' + str(cls.binary) + ' \\\n  "$@"\n')
        cls.source.chmod(0o755)
        cls.source_hash = hashlib.sha256(cls.source.read_bytes()).hexdigest()
        benign_source = cls.root / "Benign.c"
        benign_source.write_text("int diagnostic_control(void) { return 0; }\n")
        cls.benign = cls.root / "benign.dylib"
        subprocess.run([cls.compiler, *flags, "-arch", "arm64", "-dynamiclib",
                        str(benign_source), "-o", str(cls.benign)], check=True,
                       capture_output=True, timeout=30)
        cls.results = []

    @classmethod
    def tearDownClass(cls):
        output = os.environ.get("SIGBUS_CONTROL_RECEIPT")
        if output:
            Path(output).write_text(json.dumps({"compile": cls.commands,
                                               "runtime": cls.results}, indent=2) + "\n")
        cls.directory.cleanup()

    def run_owned(self, argv, env=None, timeout=10):
        process = subprocess.Popen(argv, env=env, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE, start_new_session=True)
        try:
            stdout, stderr = process.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            stdout, stderr = process.communicate(timeout=5)
            self.results.append({"argv": list(map(str, argv)), "pid": process.pid,
                                 "exit": process.returncode, "boundedTimeout": True})
            raise
        self.results.append({"argv": list(map(str, argv)), "pid": process.pid,
                             "exit": process.returncode, "stdout": stdout.decode(),
                             "stderr": stderr.decode()})
        return process.returncode, stdout, stderr

    def wrapped(self, name):
        output, record = self.root / (name + ".sh"), self.root / (name + ".bin")
        SIGBUS.prepare(self.source, output, self.root / "trace-arm64.dylib",
                       record, "arm64", self.binary)
        return output, record

    def test_normal_status_and_original_argv(self):
        for status in (0, 7):
            wrapper, record = self.wrapped("normal" + str(status))
            argv = [str(status), "space and ' quote"]
            original = self.run_owned([self.source, *argv])
            traced = self.run_owned([wrapper, *argv])
            self.assertEqual(original, traced)
            self.assertEqual(hashlib.sha256(self.source.read_bytes()).hexdigest(), self.source_hash)
            self.assertEqual(traced[0], status)
            decoded = SIGBUS.decode(record)
            self.assertEqual([r["kind"] for r in decoded["records"]], ["startup"])
            self.assertEqual(decoded["records"][0]["mainImageUUID"], self.binary_uuid)

    def test_hardware_and_raised_sigbus_preserve_signal(self):
        for mode in ("mmap", "raise"):
            wrapper, record = self.wrapped(mode)
            arguments = [mode] + ([str(self.root / "empty-mapping")] if mode == "mmap" else [])
            status, _, _ = self.run_owned([wrapper, *arguments])
            decoded = SIGBUS.decode(record)
            self.results[-1]["decoded"] = decoded
            self.assertEqual(status, -signal.SIGBUS)
            fault = next(r for r in decoded["records"] if r["kind"] == "fault")
            self.assertEqual(fault["signal"], signal.SIGBUS)
            self.assertNotEqual(int(fault["pc"], 16), 0)
            self.assertNotEqual(int(fault["sp"], 16), 0)

    def test_child_environment_none_and_empty(self):
        for index, previous in enumerate((None, "", str(self.benign))):
            record = self.root / f"child-{index}.bin"
            env = os.environ.copy()
            for key in (*SIGBUS.PRIVATE, "DYLD_INSERT_LIBRARIES"):
                env.pop(key, None)
            env.update(SIMPLEX_GHC_BUS_RECORD=str(record),
                       SIMPLEX_GHC_BUS_DYLD_PRESENT="0" if previous is None else "1",
                       DYLD_INSERT_LIBRARIES=str(self.root / "trace-arm64.dylib") +
                       (":" + previous if previous else ""))
            if previous is not None:
                env["SIMPLEX_GHC_BUS_DYLD_VALUE"] = previous
            self.assertEqual(self.run_owned([self.binary, "child-env",
                                            "<unset>" if previous is None else previous], env)[0], 0)
            self.assertEqual(len(SIGBUS.decode(record)["records"]), 1)

    def test_shape_cpu_and_record_rejections(self):
        bad = self.root / "bad.sh"
        bad.write_text(self.source.read_text() + "echo unexpected\n")
        with self.assertRaises(ValueError):
            SIGBUS.prepare(bad, self.root / "bad-copy", self.root / "trace-arm64.dylib",
                           self.root / "bad.bin", "arm64", self.binary)
        bad.write_text('#!/bin/sh\nexec ' + str(self.binary) + '; echo injected "$@"\n')
        with self.assertRaises(ValueError):
            SIGBUS.prepare(bad, self.root / "separator-copy", self.root / "trace-arm64.dylib",
                           self.root / "separator.bin", "arm64", self.binary)
        with self.assertRaises(ValueError):
            SIGBUS.prepare(self.source, self.root / "wrong-cpu", self.root / "trace-x86_64.dylib",
                           self.root / "wrong.bin", "arm64", self.binary)
        truncated = self.root / "truncated.bin"
        truncated.write_bytes(b"not a record")
        with self.assertRaises(ValueError):
            SIGBUS.decode(truncated)

    def test_owned_timeout_cleanup(self):
        wrapper, _ = self.wrapped("timeout")
        with self.assertRaises(subprocess.TimeoutExpired):
            self.run_owned([wrapper, "sleep"], timeout=1)
        pid = self.results[-1]["pid"]
        with self.assertRaises(ProcessLookupError):
            os.kill(pid, 0)


if __name__ == "__main__":
    unittest.main()
