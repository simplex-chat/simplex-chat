"""Check actual COFF storage ownership without executing a Windows program."""

import hashlib
import json
from pathlib import Path
import struct
import sys


SYMBOL = "__blst_platform_cap"


def coff(path):
    data = Path(path).read_bytes()
    offset = 0
    if data[:2] == b"MZ":
        offset = struct.unpack_from("<I", data, 60)[0]
        assert data[offset:offset + 4] == b"PE\0\0", path
        offset += 4
    machine, count, _, table, symbols, optional, _ = struct.unpack_from(
        "<HHIIIHH", data, offset
    )
    assert machine == 0x8664 and table + 18 * symbols + 4 <= len(data), path
    strings = table + 18 * symbols
    string_size = struct.unpack_from("<I", data, strings)[0]
    assert 4 <= string_size <= len(data) - strings, path

    def name(raw):
        if raw[:4] == b"\0\0\0\0":
            index = struct.unpack_from("<I", raw, 4)[0]
            assert 4 <= index < string_size, path
            return data[strings + index:strings + string_size].split(b"\0", 1)[0].decode()
        return raw.split(b"\0", 1)[0].decode()

    by_index = {}
    entries = []
    index = 0
    while index < symbols:
        raw, value, section, kind, storage, auxiliary = struct.unpack_from(
            "<8sIhHBB", data, table + 18 * index
        )
        entry = dict(name=name(raw), value=value, section=section,
                     kind=kind, storage=storage)
        entries.append(entry)
        by_index[index] = entry
        index += 1 + auxiliary
    assert index == symbols, path
    definitions = [entry for entry in entries if entry["name"] == SYMBOL]
    assert len(definitions) == 1 and definitions[0]["storage"] == 2, path

    sections = []
    for index in range(count):
        header = offset + 20 + optional + 40 * index
        raw, _, _, size, content, relocations, _, reloc_count, _, flags = struct.unpack_from(
            "<8sIIIIIIHHI", data, header
        )
        assert content + size <= len(data), path
        assert relocations + 10 * reloc_count <= len(data), path
        refs = []
        for item in range(reloc_count):
            address, symbol, kind = struct.unpack_from("<IIH", data, relocations + 10 * item)
            refs.append((address, by_index[symbol]["name"], kind))
        sections.append(dict(name=raw.split(b"\0", 1)[0].decode(), flags=flags,
                             payload=data[content:content + size], relocations=refs))
    return definitions[0], sections


def main(paths):
    assert len(paths) == 5, "server.o original.o patched.o original.exe patched.exe"
    provider, _ = coff(paths[0])
    before, before_sections = coff(paths[1])
    after, after_sections = coff(paths[2])
    # PEi386.c treats undefined/nonzero as COMMON; undefined/zero is a reference.
    # A real defined C symbol has no COFF size, and the RTS records size zero.
    assert provider["section"] > 0, provider
    assert before["section"] == 0 and before["value"] == 4, before
    assert after["section"] == 0 and after["value"] == 0, after
    assert before_sections == after_sections, "Assembly instructions or relocations changed"
    references = sum(symbol == SYMBOL for section in after_sections
                     for _, symbol, _ in section["relocations"])
    assert references > 0, "No real assembly reference to the C storage"
    linked = []
    for path in paths[3:]:
        definition, _ = coff(path)
        assert definition["section"] > 0, definition
        linked.append(definition)
    print(json.dumps({
        "cProvider": provider, "originalAssembly": before, "patchedAssembly": after,
        "assemblySectionsAndRelocationsUnchanged": True,
        "storageRelocations": references, "linkedDefinitions": linked,
        "sha256": {Path(path).name: hashlib.sha256(Path(path).read_bytes()).hexdigest()
                   for path in paths},
        "limit": "Real COFF/PE producer and linkage check; no Windows or GHC RTS execution."
    }, indent=2))


if __name__ == "__main__":
    main(sys.argv[1:])
