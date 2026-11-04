"""APK ZIP details that apksigcopier and repro-apk don't handle."""

import struct
from dataclasses import dataclass

from .common import BuildError

LOCAL_HEADER = b"PK\x03\x04"


CENTRAL_HEADER = b"PK\x01\x02"


END_OF_CENTRAL_DIRECTORY = b"PK\x05\x06"


APKSIGNER_ALIGNMENT_EXTRA = 0xD935


V1_SIGNATURE_SUFFIXES = (".SF", ".RSA", ".DSA", ".EC")


@dataclass(frozen=True, slots=True)
class ZipEntry:
    name: str
    central_offset: int
    local_offset: int
    flags: int


def central_directory(data: bytes) -> list[ZipEntry]:
    end = data.rfind(END_OF_CENTRAL_DIRECTORY)
    if end < 0:
        raise BuildError("no end of central directory record")
    count, _, offset = struct.unpack("<HII", data[end + 10 : end + 20])
    entries = []
    for _ in range(count):
        if data[offset : offset + 4] != CENTRAL_HEADER:
            raise BuildError(f"no central directory header at {offset}")
        (flags,) = struct.unpack("<H", data[offset + 8 : offset + 10])
        name_len, extra_len, comment_len = struct.unpack("<HHH", data[offset + 28 : offset + 34])
        (local_offset,) = struct.unpack("<I", data[offset + 42 : offset + 46])
        name = data[offset + 46 : offset + 46 + name_len].decode("utf-8")
        entries.append(ZipEntry(name, offset, local_offset, flags))
        offset += 46 + name_len + extra_len + comment_len
    return entries


def local_extra_fields(data: bytes, entry: ZipEntry) -> list[int]:
    offset = entry.local_offset
    if data[offset : offset + 4] != LOCAL_HEADER:
        raise BuildError(f"no local header for {entry.name} at {offset}")
    name_len, extra_len = struct.unpack("<HH", data[offset + 26 : offset + 30])
    extra = data[offset + 30 + name_len : offset + 30 + name_len + extra_len]
    fields = []
    while len(extra) >= 4:
        field_id, size = struct.unpack("<HH", extra[:4])
        fields.append(field_id)
        extra = extra[4 + size :]
    return fields


def apk_padding(data: bytes) -> str:
    """apksigner >= 35.0.0-rc1 pads entries with its 0xd935 extra field; AGP and older apksigner with zeros."""
    for entry in central_directory(data):
        if APKSIGNER_ALIGNMENT_EXTRA in local_extra_fields(data, entry):
            return "apksigner"
    return "zero"


def is_v1_signature_file(name: str) -> bool:
    if not name.startswith("META-INF/") or "/" in name.removeprefix("META-INF/"):
        return False
    return name == "META-INF/MANIFEST.MF" or name.endswith(V1_SIGNATURE_SUFFIXES)


def copy_v1_flags(source: bytes, target: bytes) -> bytes:
    """apksigner >= 35.0.0-rc1 sets the UTF-8 flag on the v1 signature files, which apksigcopier 1.1.1 drops."""
    flags = {e.name: e.flags for e in central_directory(source) if is_v1_signature_file(e.name)}
    patched = bytearray(target)
    for entry in central_directory(target):
        if entry.name in flags:
            packed = struct.pack("<H", flags[entry.name])
            patched[entry.central_offset + 8 : entry.central_offset + 10] = packed
            patched[entry.local_offset + 6 : entry.local_offset + 8] = packed
    return bytes(patched)
