"""Relabelling iOS device objects for the arm64 iOS simulator, for objects that mac2ios can't relabel, and copying
the code signature of one Mach-O file onto another."""

import struct
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path

from .common import BuildError

MH_MAGIC_64 = 0xFEEDFACF
MH_OBJECT = 1
MACH_HEADER_64 = "<IiiIIIII"
LOAD_COMMAND = "<II"

LC_SYMTAB = 0x2
LC_DYSYMTAB = 0xB
LC_SEGMENT_64 = 0x19
LC_VERSION_MIN_IPHONEOS = 0x25
LC_FUNCTION_STARTS = 0x26
LC_DATA_IN_CODE = 0x29
LC_LINKER_OPTION = 0x2D
LC_LINKER_OPTIMIZATION_HINT = 0x2E
LC_BUILD_VERSION = 0x32

PLATFORM_IOSSIMULATOR = 7
TOOL_CLANG = 1

# LC_BUILD_VERSION with one tool is 16 bytes longer than LC_VERSION_MIN_IPHONEOS: everything after the load commands
# moves by 16, which keeps the 16-byte alignment of the sections' data
BUILD_VERSION = "<IIIIIIII"
SHIFT = struct.calcsize(BUILD_VERSION) - struct.calcsize("<IIII")

# the offset fields, by their byte offset in each command, that point into the data after the load commands
SYMTAB_OFFSETS = (8, 16)
DYSYMTAB_OFFSETS = (32, 40, 48, 56, 64, 72)
LINKEDIT_DATA_OFFSETS = (8,)
SEGMENT_64_FILEOFF = 40
SEGMENT_64_NSECTS = 64
SEGMENT_64_HEADER = 72
SECTION_64_SIZE = 80
SECTION_64_OFFSET = 48
SECTION_64_RELOFF = 56
OFFSET_FIELDS = {
    LC_SYMTAB: SYMTAB_OFFSETS,
    LC_DYSYMTAB: DYSYMTAB_OFFSETS,
    LC_FUNCTION_STARTS: LINKEDIT_DATA_OFFSETS,
    LC_DATA_IN_CODE: LINKEDIT_DATA_OFFSETS,
    LC_LINKER_OPTIMIZATION_HINT: LINKEDIT_DATA_OFFSETS,
    LC_LINKER_OPTION: (),
    LC_BUILD_VERSION: (),
}

AR_MAGIC = b"!<arch>\n"
AR_HEADER = 60
AR_SIZE = slice(48, 58)
AR_LONG_NAME = b"#1/"
AR_SYMBOL_TABLE = b"__.SYMDEF"


def shift_offset(command: bytearray, at: int, field: str = "<I") -> None:
    (value,) = struct.unpack_from(field, command, at)
    if value:
        struct.pack_into(field, command, at, value + SHIFT)


def shift_command(cmd: int, command: bytearray) -> None:
    if cmd == LC_SEGMENT_64:
        shift_offset(command, SEGMENT_64_FILEOFF, "<Q")
        (nsects,) = struct.unpack_from("<I", command, SEGMENT_64_NSECTS)
        for i in range(nsects):
            section = SEGMENT_64_HEADER + i * SECTION_64_SIZE
            shift_offset(command, section + SECTION_64_OFFSET)
            shift_offset(command, section + SECTION_64_RELOFF)
        return
    if cmd not in OFFSET_FIELDS:
        raise BuildError(f"load command {cmd:#x} may point into the object; it is not known how to move it")
    for at in OFFSET_FIELDS[cmd]:
        shift_offset(command, at)


def simulator_object(obj: bytes) -> bytes:
    """obj with its LC_VERSION_MIN_IPHONEOS replaced by an LC_BUILD_VERSION for the simulator, or obj unchanged."""
    magic, cputype, cpusubtype, filetype, ncmds, sizeofcmds, flags, reserved = struct.unpack_from(MACH_HEADER_64, obj)
    if magic != MH_MAGIC_64 or filetype != MH_OBJECT:
        return obj
    header = struct.calcsize(MACH_HEADER_64)
    commands, offset, replaced = [], header, False
    for _ in range(ncmds):
        cmd, size = struct.unpack_from(LOAD_COMMAND, obj, offset)
        command = bytearray(obj[offset : offset + size])
        if cmd == LC_VERSION_MIN_IPHONEOS:
            _, _, minos, sdk = struct.unpack_from("<IIII", command)
            new_size = struct.calcsize(BUILD_VERSION)
            build = struct.pack(BUILD_VERSION, LC_BUILD_VERSION, new_size, PLATFORM_IOSSIMULATOR, minos, sdk, 1, TOOL_CLANG, sdk)
            commands.append(bytearray(build))
            replaced = True
        else:
            commands.append(command)
        offset += size
    if not replaced:
        return obj
    for command in commands:
        shift_command(struct.unpack_from("<I", command)[0], command)
    new_header = struct.pack(MACH_HEADER_64, magic, cputype, cpusubtype, filetype, ncmds, sizeofcmds + SHIFT, flags, reserved)
    return new_header + b"".join(commands) + obj[header + sizeofcmds :]


def archive_members(data: bytes) -> Iterator[tuple[bytes, bytes, bytes]]:
    """Each member's header, its BSD long name bytes (empty for short names) and its contents."""
    if not data.startswith(AR_MAGIC):
        raise BuildError("not an ar archive")
    offset = len(AR_MAGIC)
    while offset < len(data):
        header = data[offset : offset + AR_HEADER]
        size = int(header[AR_SIZE].decode().strip())
        body = data[offset + AR_HEADER : offset + AR_HEADER + size]
        name_length = int(header[3:16].decode().strip()) if header.startswith(AR_LONG_NAME) else 0
        yield header, body[:name_length], body[name_length:]
        offset += AR_HEADER + size + (size & 1)


def simulator_archive(lib: Path) -> int:
    """Rewrites lib's device objects for the simulator, without its symbol table, which ranlib must write again;
    returns how many objects changed."""
    data = lib.read_bytes()
    out, changed = bytearray(AR_MAGIC), 0
    for header, name, contents in archive_members(data):
        if name.startswith(AR_SYMBOL_TABLE) or header.startswith(AR_SYMBOL_TABLE):
            continue
        new = simulator_object(contents)
        changed += new is not contents
        size = len(name) + len(new)
        out += header[: AR_SIZE.start] + str(size).encode().ljust(AR_SIZE.stop - AR_SIZE.start) + header[AR_SIZE.stop :]
        out += name + new + (b"\n" if size & 1 else b"")
    lib.write_bytes(out)
    return changed


LC_CODE_SIGNATURE = 0x1D
LINKEDIT_DATA = "<IIII"
LINKEDIT = b"__LINKEDIT"
SEGMENT_64_NAME = slice(8, 24)
SEGMENT_64_VMSIZE = 32
SEGMENT_64_FILESIZE = 48
MACH_HEADER_NCMDS = 16

FAT_MAGIC = 0xCAFEBABE
FAT_HEADER = ">II"
FAT_ARCH = ">iiIII"

CODE_DIFFERS = "it differs by more than its code signature"

# Java class files start with FAT_MAGIC too, followed by their version, which is 45 or more
JAVA_CLASS_MIN_VERSION = 45


@dataclass(frozen=True, slots=True)
class CodeSignature:
    """Where the LC_CODE_SIGNATURE command and the signature it points to are in a thin Mach-O file."""

    command: int
    dataoff: int
    datasize: int


@dataclass(frozen=True, slots=True)
class FatArch:
    cputype: int
    cpusubtype: int
    offset: int
    size: int
    align: int


def is_thin_macho(data: bytes) -> bool:
    return len(data) >= struct.calcsize(MACH_HEADER_64) and struct.unpack_from("<I", data)[0] == MH_MAGIC_64


def fat_arches(data: bytes) -> list[FatArch] | None:
    if len(data) < struct.calcsize(FAT_HEADER):
        return None
    magic, count = struct.unpack_from(FAT_HEADER, data)
    if magic != FAT_MAGIC or not 0 < count < JAVA_CLASS_MIN_VERSION:
        return None
    size = struct.calcsize(FAT_ARCH)
    return [FatArch(*struct.unpack_from(FAT_ARCH, data, struct.calcsize(FAT_HEADER) + i * size)) for i in range(count)]


def is_macho(data: bytes) -> bool:
    return is_thin_macho(data) or fat_arches(data) is not None


def load_commands(data: bytes) -> Iterator[tuple[int, int]]:
    """The offset and type of each load command of a thin 64-bit Mach-O file."""
    ncmds = struct.unpack_from("<I", data, MACH_HEADER_NCMDS)[0]
    offset = struct.calcsize(MACH_HEADER_64)
    for _ in range(ncmds):
        cmd, size = struct.unpack_from(LOAD_COMMAND, data, offset)
        yield offset, cmd
        offset += size


def code_signature(data: bytes) -> CodeSignature | None:
    for offset, cmd in load_commands(data):
        if cmd == LC_CODE_SIGNATURE:
            _, _, dataoff, datasize = struct.unpack_from(LINKEDIT_DATA, data, offset)
            return CodeSignature(offset, dataoff, datasize)
    return None


def linkedit_command(data: bytes) -> int:
    for offset, cmd in load_commands(data):
        if cmd == LC_SEGMENT_64 and data[offset + SEGMENT_64_NAME.start : offset + SEGMENT_64_NAME.stop].rstrip(b"\0") == LINKEDIT:
            return offset
    raise BuildError("the Mach-O file has no __LINKEDIT segment")


def copy_thin_signature(released: bytes, built: bytes) -> bytes:
    """built with released's signature in place of its own, or added after its __LINKEDIT data as codesign adds one,
    with released's LC_CODE_SIGNATURE and the __LINKEDIT sizes that cover the signature."""
    signature = code_signature(released)
    if signature is None:
        return built
    size = struct.calcsize(LINKEDIT_DATA)
    out = bytearray(built)
    own = code_signature(built)
    if own:
        command = own.command
        del out[own.dataoff :]
    else:
        _, _, _, _, ncmds, sizeofcmds, _, _ = struct.unpack_from(MACH_HEADER_64, built)
        command = struct.calcsize(MACH_HEADER_64) + sizeofcmds
        # codesign adds the command in the free space after the others, which must not hide a difference of the code
        if any(built[command : command + size]):
            return built
        struct.pack_into("<II", out, MACH_HEADER_NCMDS, ncmds + 1, sizeofcmds + size)
    out += bytes(max(0, signature.dataoff - len(out)))
    out[command : command + size] = released[signature.command : signature.command + size]
    released_linkedit, built_linkedit = linkedit_command(released), linkedit_command(built)
    for field in (SEGMENT_64_VMSIZE, SEGMENT_64_FILESIZE):
        out[built_linkedit + field : built_linkedit + field + 8] = released[released_linkedit + field : released_linkedit + field + 8]
    return bytes(out + released[signature.dataoff : signature.dataoff + signature.datasize])


def copy_code_signature(released: bytes, built: bytes) -> bytes:
    """built signed as released is: each thin file, or each architecture of a universal one, with released's code
    signature, in the layout of released's architectures; built as it is when the two are not Mach-O files of the
    same architectures."""
    if is_thin_macho(released) and is_thin_macho(built):
        return copy_thin_signature(released, built)
    released_arches, built_arches = fat_arches(released), fat_arches(built)
    if released_arches is None or built_arches is None:
        return built
    if [(a.cputype, a.cpusubtype) for a in released_arches] != [(a.cputype, a.cpusubtype) for a in built_arches]:
        return built
    out = bytearray(released[: struct.calcsize(FAT_HEADER) + len(released_arches) * struct.calcsize(FAT_ARCH)])
    for ours, theirs in zip(built_arches, released_arches, strict=True):
        out += bytes(max(0, theirs.offset - len(out)))
        out += copy_thin_signature(released[theirs.offset : theirs.offset + theirs.size], built[ours.offset : ours.offset + ours.size])
    return bytes(out)


def same_code(released: bytes, built: bytes) -> bool:
    """Whether released is built, or built with released's code signature."""
    if released == built:
        return True
    try:
        return is_macho(released) and copy_code_signature(released, built) == released
    except (struct.error, BuildError):
        return False
