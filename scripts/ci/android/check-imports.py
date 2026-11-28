#!/usr/bin/env python3

import struct
import sys
import zipfile
from pathlib import Path

CORE_LIBS = ("libsimplex.so", "libsupport.so")
SYSTEM_LIBS = ("libc.so", "libm.so", "libdl.so")
SHT_DYNSYM = 11
STB_GLOBAL = 1
STB_WEAK = 2
STV_DEFAULT = 0


def dynamic_symbols(data, name):
    if data[:4] != b"\x7fELF" or data[5] != 1:
        raise ValueError(f"{name}: not a little-endian ELF file")
    if data[4] == 1:
        shoff, = struct.unpack_from("<I", data, 0x20)
        shentsize, shnum = struct.unpack_from("<HH", data, 0x2E)
        section, sym_size = "<IIIIIIIIII", 16
    else:
        shoff, = struct.unpack_from("<Q", data, 0x28)
        shentsize, shnum = struct.unpack_from("<HH", data, 0x3A)
        section, sym_size = "<IIQQQQIIQQ", 24
    sections = [struct.unpack_from(section, data, shoff + i * shentsize) for i in range(shnum)]
    undefined, defined = set(), set()
    for sh in sections:
        if sh[1] != SHT_DYNSYM:
            continue
        strtab = sections[sh[6]]
        strings = data[strtab[4]:strtab[4] + strtab[5]]
        for off in range(sh[4] + sym_size, sh[4] + sh[5], sym_size):
            if data[4] == 1:
                st_name, _, _, st_info, st_other, st_shndx = struct.unpack_from("<IIIBBH", data, off)
            else:
                st_name, st_info, st_other, st_shndx = struct.unpack_from("<IBBH", data, off)
            symbol = strings[st_name:strings.index(b"\0", st_name)].decode()
            binding = st_info >> 4
            if st_shndx == 0:
                if binding == STB_GLOBAL:
                    undefined.add(symbol)
            elif binding in (STB_GLOBAL, STB_WEAK) and st_other & 3 == STV_DEFAULT:
                defined.add(symbol)
    return undefined, defined


def main(apk, abi, stubs_dir):
    available = set()
    for lib in SYSTEM_LIBS:
        available |= dynamic_symbols((Path(stubs_dir) / lib).read_bytes(), lib)[1]
    core_imports = {}
    with zipfile.ZipFile(apk) as z:
        libs = [n for n in z.namelist() if n.startswith(f"lib/{abi}/") and n.endswith(".so")]
        for name in libs:
            undefined, defined = dynamic_symbols(z.read(name), name)
            available |= defined
            if Path(name).name in CORE_LIBS:
                core_imports[Path(name).name] = undefined
    missing_libs = [lib for lib in CORE_LIBS if lib not in core_imports]
    if missing_libs:
        print(f"{apk}: lib/{abi}/ has no {', '.join(missing_libs)}")
        return 1
    failed = 0
    for lib, imports in core_imports.items():
        missing = sorted(imports - available)
        print(f"{lib}: {len(imports)} imports, {len(missing)} without a definition in {stubs_dir} or the APK")
        for symbol in missing:
            print(f"  {symbol}")
        failed += len(missing)
    return 1 if failed else 0


if __name__ == "__main__":
    if len(sys.argv) != 4:
        sys.exit("usage: check-imports.py <apk> <arm64-v8a|armeabi-v7a> <NDK stubs dir of API 26>")
    sys.exit(main(*sys.argv[1:]))
