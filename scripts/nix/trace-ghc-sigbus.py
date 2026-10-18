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
INSTRUCTION_BYTES = 16
CPUS = {"x86_64": 0x01000007, "arm64": 0x0100000C}
PRIVATE = (
    "SIMPLEX_GHC_BUS_RECORD",
    "SIMPLEX_GHC_BUS_DYLD_PRESENT",
    "SIMPLEX_GHC_BUS_DYLD_VALUE",
)


def read_regular(path, limit):
    descriptor = os.open(path, os.O_RDONLY | os.O_CLOEXEC | os.O_NOFOLLOW | os.O_NONBLOCK)
    with os.fdopen(descriptor, "rb") as stream:
        info = os.fstat(stream.fileno())
        if not stat.S_ISREG(info.st_mode):
            raise ValueError("Diagnostic input must be a regular file")
        if info.st_size > limit:
            raise ValueError("Diagnostic input exceeds its byte limit")
        data = stream.read(limit)
        if os.fstat(stream.fileno()).st_size > limit:
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


def executable_image(path, architecture):
    # Read only the header and bounded load commands, never the text/data payload.
    descriptor = os.open(path, os.O_RDONLY | os.O_CLOEXEC | os.O_NONBLOCK)
    with os.fdopen(descriptor, "rb") as stream:
        info = os.fstat(stream.fileno())
        if not stat.S_ISREG(info.st_mode):
            raise ValueError("Compiler executable must be a regular file")
        header = stream.read(32)
        if len(header) != 32:
            raise ValueError("Truncated Mach-O executable header")
        magic, kind, _, file_type, count, size, _, _ = struct.unpack("<8I", header)
        if magic != 0xFEEDFACF or kind != CPUS[architecture] or file_type != 2:
            raise ValueError("Unexpected Mach-O executable format or CPU")
        if count > 1024 or size > 1024 * 1024 or 32 + size > info.st_size:
            raise ValueError("Mach-O load commands exceed their bounds")
        commands = stream.read(size)
        if len(commands) != size:
            raise ValueError("Truncated Mach-O load commands")
    offset, uuid, segments = 0, None, []
    for _ in range(count):
        if len(commands) - offset < 8:
            raise ValueError("Missing Mach-O load command")
        command, length = struct.unpack_from("<II", commands, offset)
        if length < 8 or length % 8 or length > len(commands) - offset:
            raise ValueError("Invalid Mach-O load command size")
        if command == 0x1B:  # LC_UUID
            if length != 24 or uuid is not None:
                raise ValueError("Invalid or duplicate Mach-O UUID")
            uuid = commands[offset + 8:offset + 24].hex()
        elif command == 0x19:  # LC_SEGMENT_64
            if length < 72:
                raise ValueError("Truncated Mach-O segment")
            name, address, virtual_size, file_offset, file_size, maximum, initial, sections, _ = \
                struct.unpack_from("<16s4Q4I", commands, offset + 8)
            if (length != 72 + sections * 80 or address + virtual_size > 2**64
                    or file_offset + file_size > info.st_size):
                raise ValueError("Invalid Mach-O segment bounds")
            segments.append({
                "name": name.split(b"\0", 1)[0].decode("ascii"),
                "vmaddr": hex(address), "vmsize": hex(virtual_size),
                "fileoff": hex(file_offset), "filesize": hex(file_size),
                "maxprot": maximum, "initprot": initial,
            })
        offset += length
    if offset != len(commands):
        raise ValueError("Mach-O load command count does not match its size")
    return {"uuid": uuid, "segments": segments}


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
    if cpu(tracer, 6) != CPUS[architecture]:
        raise ValueError("Diagnostic dylib has the wrong Mach-O CPU")
    image = executable_image(expected_binary, architecture)
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
        "executableImage": image,
    }


def instruction_record(words, path):
    result = struct.unpack("<q", struct.pack("<Q", words[6]))[0]
    error, attempted = words[7:9]
    if (words[3] != CPUS["x86_64"] or words[5] != INSTRUCTION_BYTES
            or result < -1 or result > INSTRUCTION_BYTES or error > 2**31 - 1
            or attempted not in (0, 1) or (result >= 0 and (error or not attempted))
            or (result == -1 and not error)):
        raise ValueError("Invalid optional instruction metadata")
    report = {
        "kind": "instruction-bytes", "cpu": "x86_64", "pc": hex(words[4]),
        "requestedBytes": words[5], "writeResult": result, "errno": error,
        "attempted": bool(attempted),
        "status": "disabled" if not attempted else "write-failed" if result < 0 else "partial" if result < INSTRUCTION_BYTES else "complete",
    }
    try:
        data = read_regular(Path(str(path) + ".pc"), INSTRUCTION_BYTES)
    except (OSError, ValueError) as error:
        report.update(sidecarStatus=type(error).__name__, sidecarBytes=None)
        if result >= 0:
            report["status"] = "sidecar-unavailable"
    else:
        report.update(sidecarBytes=len(data), instructionBytes=data.hex())
        if result >= 0 and len(data) != result:
            report["status"] = "sidecar-size-mismatch"
    return report


def decode(path):
    data = read_regular(path, 4 * WORDS * 8)
    size = WORDS * 8
    if not data or len(data) > 4 * size:
        raise ValueError("Missing, truncated, or oversized SIGBUS record")
    complete = len(data) - len(data) % size
    if complete < size:
        raise ValueError("Missing, truncated, or oversized SIGBUS record")
    result = []
    kinds = set()
    optional_error = None
    for offset in range(0, complete, size):
        words = struct.unpack_from("<" + "Q" * WORDS, data, offset)
        if {1, 2}.issubset(kinds):
            # The primary facts are already complete. An optional append must
            # never make them unreadable, including a short metadata write.
            try:
                if 3 in kinds:
                    raise ValueError("Duplicate optional instruction record")
                if (words[0] != MAGIC or words[1] != 1 or words[2] != 3):
                    raise ValueError("Invalid optional instruction record")
                report = instruction_record(words, path)
                fault = next(record for record in result if record["kind"] == "fault")
                if report["pc"] != fault["pc"] or report["cpu"] != fault["cpu"]:
                    raise ValueError("Optional instruction context does not match fault")
                result.append(report)
            except ValueError as error:
                optional_error = str(error)
            kinds.add(3)
            continue
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
            fault = {
                "kind": "fault", "cpu": architecture, "signal": words[4],
                "code": struct.unpack("<q", struct.pack("<Q", words[5]))[0],
                "faultAddress": hex(words[6]), "pc": hex(words[7]),
                "sp": hex(words[8]), "bp": hex(words[9]), "lr": hex(words[10]),
                "flags": hex(words[11]),
                "registers": {name: hex(words[12 + index]) for index, name in enumerate(names)},
            }
            if architecture == "x86_64" and words[41]:
                if words[41] != 1 or words[42] > 65535 or words[43] > 2**32 - 1:
                    raise ValueError("Invalid x86 exception-state extension")
                fault["x86ExceptionState"] = {
                    "trapNumber": words[42], "errorCode": hex(words[43]),
                    "faultAddress": hex(words[44]),
                }
            result.append(fault)
    if len(data) != complete:
        if not {1, 2}.issubset(kinds):
            raise ValueError("Truncated primary SIGBUS record")
        optional_error = "Truncated optional instruction metadata"
    elif kinds == {1, 2} and any("x86ExceptionState" in record for record in result):
        optional_error = "Missing optional instruction metadata"
    report = {"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest(), "records": result}
    if optional_error is not None:
        report["optionalInstructionError"] = optional_error
    return report


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
