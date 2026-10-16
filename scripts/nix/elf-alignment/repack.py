"""Repack the tiny ELF64 probe without changing section data or alignment."""

import hashlib
import json
import pathlib
import struct
import sys

original_path, packed_path, report_path = map(pathlib.Path, sys.argv[1:])
original = original_path.read_bytes()
assert original[:6] == b"\x7fELF\x02\x01"  # ELF64, little endian
header_format = "<16sHHIQQQIHHHHHH"
section_format = "<IIQQQQIIQQ"
header = list(struct.unpack_from(header_format, original))
assert header[1:3] == [1, 62]  # ET_REL, EM_X86_64
assert header[10] == 0  # No program headers in this fixture.
section_offset, entry_size, count, string_index = header[6], *header[11:14]
assert entry_size == struct.calcsize(section_format) == 64
sections = [
    list(struct.unpack_from(section_format, original, section_offset + i * entry_size))
    for i in range(count)
]
strings = sections[string_index]
names = original[strings[4] : strings[4] + strings[5]]


def section_name(section):
    return names[section[0] :].split(b"\0", 1)[0].decode()


rodata = next(section for section in sections if section_name(section) == ".rodata")
assert rodata[5] == 8192 and rodata[8] == 64 and rodata[4] % 64 == 0
cut = rodata[4]
padding = 8
packed = bytearray(original[:cut] + b"\0" * padding + original[cut:])
header[6] += padding
struct.pack_into(header_format, packed, 0, *header)
observations = []
for i, section in enumerate(sections):
    moved = section.copy()
    if moved[4] >= cut and moved[4] != 0:
        moved[4] += padding
    struct.pack_into(section_format, packed, header[6] + i * entry_size, *moved)
    if section[1] not in (0, 8):  # SHT_NULL/SHT_NOBITS have no file payload.
        assert original[section[4] : section[4] + section[5]] == packed[
            moved[4] : moved[4] + moved[5]
        ], section_name(section)
    observations.append(
        {
            "name": section_name(section),
            "size": section[5],
            "alignment": section[8],
            "originalOffset": section[4],
            "packedOffset": moved[4],
        }
    )
packed_path.write_bytes(packed)
report = {
    "paddingBytes": padding,
    "allSectionPayloadsUnchanged": True,
    "symbolsAndRelocationsUnchanged": True,
    "sha256": {
        path.name: hashlib.sha256(path.read_bytes()).hexdigest()
        for path in (original_path, packed_path)
    },
    "sections": observations,
}
report_path.write_text(json.dumps(report, indent=2) + "\n")
print(json.dumps(report, indent=2))
