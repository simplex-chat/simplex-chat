#!/usr/bin/env python3
"""Prepare a private compiler wrapper and decode its bounded SIGBUS record."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import stat
import struct


MAGIC = 0x5349474255533031
WORDS = 64
CPUS = {"x86_64": 0x01000007, "arm64": 0x0100000C}
PRIVATE = (
    "SIMPLEX_GHC_BUS_RECORD",
    "SIMPLEX_GHC_BUS_DYLD_PRESENT",
    "SIMPLEX_GHC_BUS_DYLD_VALUE",
)


def read_regular(path, limit):
    descriptor = os.open(path, os.O_RDONLY | os.O_CLOEXEC | os.O_NOFOLLOW | os.O_NONBLOCK)
    with os.fdopen(descriptor, "rb") as stream:
        if not stat.S_ISREG(os.fstat(stream.fileno()).st_mode):
            raise ValueError("Diagnostic input must be a regular file")
        data = stream.read(limit + 1)
    if len(data) > limit:
        raise ValueError("Diagnostic input exceeds its byte limit")
    return data


def cpu(path, file_type=None):
    with Path(path).open("rb") as stream:
        header = stream.read(16)
    if len(header) != 16 or struct.unpack("<I", header[:4])[0] != 0xFEEDFACF:
        raise ValueError("Expected a thin little-endian 64-bit Mach-O executable")
    if file_type is not None and struct.unpack_from("<I", header, 12)[0] != file_type:
        raise ValueError("Unexpected Mach-O file type")
    return struct.unpack_from("<I", header, 4)[0]


def prepare(source, output, tracer, record, architecture, expected_binary):
    source, output, tracer, record = map(Path, (source, output, tracer, record))
    expected_binary = Path(expected_binary)
    if not all(path.is_absolute() for path in (source, output, tracer, record, expected_binary)):
        raise ValueError("Every diagnostic path must be absolute")
    original = read_regular(source, 64 * 1024)
    text = original.decode("utf-8")
    # The pinned compiler factory emits only this shebang and a continued exec.
    # Fail closed on other script logic rather than bypassing any wrapper setup.
    if not text.startswith("#!/bin/sh\nexec ") or text.count("\nexec ") != 1:
        raise ValueError("Compiler wrapper must contain one final exec after its shebang")
    body = text[len("#!/bin/sh\n"):]
    logical = body.replace("\\\n", "").rstrip("\n")
    if re.fullmatch(r'exec [A-Za-z0-9_./:+,=@% -]+ "\$@"', logical) is None:
        raise ValueError("Compiler wrapper contains unsupported shell syntax")
    argv = shlex.split(logical)
    if (len(argv) < 3 or argv[0] != "exec" or argv[-1] != "$@"
            or "\n" in body.replace("\\\n", "").rstrip("\n")):
        raise ValueError("Unsupported compiler wrapper exec shape")
    if Path(argv[1]) != expected_binary or not expected_binary.name.startswith("ghc"):
        raise ValueError("Compiler executable does not match the expected wrapper target")
    if cpu(expected_binary, 2) != CPUS[architecture] or cpu(tracer, 6) != CPUS[architecture]:
        raise ValueError("Compiler or diagnostic dylib has the wrong Mach-O CPU")
    # These builtins run only after /bin/sh has started. Setting DYLD before
    # launching that protected shell would not reliably inject the compiler.
    insertion = """if [ "${SIMPLEX_GHC_BUS_RECORD+x}" = x ] ||
   [ "${SIMPLEX_GHC_BUS_DYLD_PRESENT+x}" = x ] ||
   [ "${SIMPLEX_GHC_BUS_DYLD_VALUE+x}" = x ]; then
  echo 'GHC SIGBUS diagnostic: private environment key already exists' >&2
  exit 125
fi
if [ "${DYLD_INSERT_LIBRARIES+x}" = x ]; then
  export SIMPLEX_GHC_BUS_DYLD_PRESENT=1
  export SIMPLEX_GHC_BUS_DYLD_VALUE="$DYLD_INSERT_LIBRARIES"
else
  export SIMPLEX_GHC_BUS_DYLD_PRESENT=0
fi
export SIMPLEX_GHC_BUS_RECORD=RECORD
export DYLD_INSERT_LIBRARIES=TRACER${DYLD_INSERT_LIBRARIES:+:$DYLD_INSERT_LIBRARIES}
""".replace("RECORD\n", shlex.quote(str(record)) + "\n").replace(
        "TRACER${", shlex.quote(str(tracer)) + "${")
    prefix = b"#!/bin/sh\n"
    changed = prefix + insertion.encode() + original[len(prefix):]
    if changed.replace(insertion.encode(), b"", 1) != original:
        raise ValueError("Private wrapper failed its reverse byte comparison")
    with output.open("xb") as stream:
        stream.write(changed)
    output.chmod(source.stat().st_mode & 0o777)
    return {
        "source": str(source), "sourceSHA256": hashlib.sha256(original).hexdigest(),
        "copy": str(output), "copySHA256": hashlib.sha256(changed).hexdigest(),
        "expectedBinary": str(expected_binary), "cpu": architecture,
        "record": str(record), "originalExecAndArgumentsPreserved": True,
    }


def decode(path):
    data = read_regular(path, 4 * WORDS * 8)
    size = WORDS * 8
    if not data or len(data) > 4 * size or len(data) % size:
        raise ValueError("Missing, truncated, or oversized SIGBUS record")
    result = []
    kinds = set()
    for offset in range(0, len(data), size):
        words = struct.unpack_from("<" + "Q" * WORDS, data, offset)
        if words[0] != MAGIC or words[1] != 1 or words[2] not in (1, 2):
            raise ValueError("Unknown SIGBUS record format")
        if words[2] in kinds or words[3] not in CPUS.values():
            raise ValueError("Duplicate record or unsupported CPU")
        kinds.add(words[2])
        architecture = next(name for name, value in CPUS.items() if value == words[3])
        if words[2] == 1:
            result.append({
                "kind": "startup", "cpu": architecture, "pid": words[4],
                "mainImageBase": hex(words[5]), "mainImageSlide": hex(words[6]),
                "mainImageUUID": data[offset + 64:offset + 80].hex() if words[7] else None,
            })
        else:
            names = (["rax", "rbx", "rcx", "rdx", "rdi", "rsi", "r8", "r9",
                      "r10", "r11", "r12", "r13", "r14", "r15"]
                     if architecture == "x86_64" else [f"x{index}" for index in range(29)])
            result.append({
                "kind": "fault", "cpu": architecture, "signal": words[4],
                "code": struct.unpack("<q", struct.pack("<Q", words[5]))[0],
                "faultAddress": hex(words[6]), "pc": hex(words[7]),
                "sp": hex(words[8]), "bp": hex(words[9]), "lr": hex(words[10]),
                "flags": hex(words[11]),
                "registers": {name: hex(words[12 + index]) for index, name in enumerate(names)},
            })
    return {"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest(), "records": result}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    create = commands.add_parser("prepare")
    for name in ("source", "output", "tracer", "record", "expected-binary"):
        create.add_argument("--" + name, required=True)
    create.add_argument("--cpu", choices=CPUS, required=True)
    read = commands.add_parser("decode")
    read.add_argument("record")
    arguments = parser.parse_args()
    if arguments.command == "prepare":
        report = prepare(arguments.source, arguments.output, arguments.tracer,
                         arguments.record, arguments.cpu, arguments.expected_binary)
    else:
        report = decode(arguments.record)
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
