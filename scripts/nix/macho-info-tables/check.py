"""Check tables-next-to-code across a real Mach-O link without executing it."""

import argparse
import json
import pathlib
import struct


def macho(path):
    data = path.read_bytes()

    def unpack(fmt, offset):
        size = struct.calcsize(fmt)
        if offset < 0 or offset + size > len(data):
            raise ValueError(f"{path}: truncated Mach-O at {offset}")
        return struct.unpack_from(fmt, data, offset)

    magic, cpu, _, _, commands, command_bytes, _, _ = unpack("<8I", 0)
    if magic != 0xFEEDFACF or cpu != 0x01000007:
        raise ValueError(f"{path}: expected little-endian x86_64 Mach-O")
    sections = []
    symtab = None
    offset = 32
    command_end = offset + command_bytes
    for _ in range(commands):
        cmd, size = unpack("<II", offset)
        if size < 8 or offset + size > command_end:
            raise ValueError(f"{path}: invalid load command")
        if cmd == 0x19:  # LC_SEGMENT_64
            count, = unpack("<I", offset + 64)
            if 72 + count * 80 > size:
                raise ValueError(f"{path}: truncated section list")
            for i in range(count):
                fields = unpack("<16s16sQQIIIIIIII", offset + 72 + i * 80)
                sections.append((fields[2], fields[3], fields[4]))
        elif cmd == 2:  # LC_SYMTAB
            symtab = unpack("<IIII", offset + 8)
        offset += size
    if offset != command_end or symtab is None:
        raise ValueError(f"{path}: missing symbol table or malformed commands")
    symbol_offset, symbol_count, string_offset, string_size = symtab
    if string_offset + string_size > len(data):
        raise ValueError(f"{path}: truncated string table")
    symbols = {}
    for i in range(symbol_count):
        index, kind, section, desc, address = unpack("<IBBHQ", symbol_offset + 16 * i)
        if kind & 0xE0 or kind & 0x0E != 0x0E:  # Skip STAB and non-N_SECT.
            continue
        if index >= string_size or not 1 <= section <= len(sections):
            raise ValueError(f"{path}: invalid symbol")
        end = data.find(b"\0", string_offset + index, string_offset + string_size)
        if end < 0:
            raise ValueError(f"{path}: unterminated symbol")
        name = data[string_offset + index:end].decode("utf-8")
        # Other local names, such as func.eh, may legitimately repeat. Only
        # table/entry names need unique resolution for this layout check.
        if not (name.startswith("_dsp_") or name.endswith("_info")):
            continue
        if name in symbols:
            raise ValueError(f"{path}: ambiguous symbol {name}")
        symbols[name] = (address, section, desc)

    def info_type(name):
        address, section, _ = symbols[name]
        start, length, file_offset = sections[section - 1]
        if not start + 8 <= address < start + length:
            raise ValueError(f"{path}: info table outside section: {name}")
        return unpack("<I", file_offset + address - start - 8)[0]

    return symbols, info_type


def check(original, linked):
    before, before_type = macho(original)
    after, after_type = macho(linked)
    rows = []
    discarded = 0
    for anchor in sorted(before):
        if not anchor.startswith("_dsp_") or not anchor.endswith("_dsp"):
            continue
        info = anchor[5:-4]
        if info not in before:
            raise ValueError(f"{original}: missing info symbol for {anchor}")
        if not before[info][2] & 0x200:  # N_ALT_ENTRY
            raise ValueError(f"{original}: {info} starts an independent subsection")
        if info not in after and anchor not in after:
            discarded += 1
            continue
        if info not in after or anchor not in after:
            raise ValueError(f"{linked}: lost table or entry: {info}")
        old_distance = before[info][0] - before[anchor][0]
        new_distance = after[info][0] - after[anchor][0]
        old_type, new_type = before_type(info), after_type(info)
        if old_distance <= 0 or new_distance != old_distance:
            raise ValueError(f"{info}: table/entry distance changed {old_distance} -> {new_distance}")
        if old_type == 0 or new_type != old_type:
            raise ValueError(f"{info}: closure type changed {old_type} -> {new_type}")
        rows.append({"symbol": info, "tableBytes": old_distance, "closureType": old_type})
    if len(rows) < 12:
        raise ValueError(f"expected at least 12 retained procedure tables, got {len(rows)}")
    return {"result": "PASS", "discardedProcedures": discarded, "tables": rows}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("object", type=pathlib.Path)
    parser.add_argument("linked", type=pathlib.Path)
    args = parser.parse_args()
    print(json.dumps(check(args.object, args.linked), indent=2))
