#!/usr/bin/python3
"""Zero __eh_frame and __compact_unwind in Mach-O archive members.

Apple ld 1267 rejects the libffi/GHC objects with
"address not in any section" while applying those relocations.
The member size stays the same, so the archive index remains valid.
"""
import struct
import sys


def patch_macho(buf):
    if len(buf) < 32 or struct.unpack_from("<I", buf, 0)[0] != 0xFEEDFACF:
        return 0
    ncmds = struct.unpack_from("<I", buf, 16)[0]
    off = 32
    changed = 0
    for _ in range(ncmds):
        if off + 8 > len(buf):
            return changed
        cmd, cmdsize = struct.unpack_from("<II", buf, off)
        if cmdsize < 8 or off + cmdsize > len(buf):
            return changed
        if cmd == 0x19:  # LC_SEGMENT_64
            nsects = struct.unpack_from("<I", buf, off + 64)[0]
            so = off + 72
            for _s in range(nsects):
                name = bytes(buf[so:so + 16]).split(b"\0", 1)[0]
                if name in (b"__eh_frame", b"__compact_unwind"):
                    size = struct.unpack_from("<Q", buf, so + 40)[0]
                    nreloc = struct.unpack_from("<I", buf, so + 60)[0]
                    if size or nreloc:
                        struct.pack_into("<Q", buf, so + 40, 0)
                        struct.pack_into("<I", buf, so + 60, 0)
                        changed += 1
                so += 80
        off += cmdsize
    return changed


def patch_archive(path):
    data = bytearray(open(path, "rb").read())
    if data[:8] != b"!<arch>\n":
        raise SystemExit(f"{path}: not an archive")
    pos = 8
    members = 0
    sections = 0
    while pos + 60 <= len(data):
        hdr = data[pos:pos + 60]
        if bytes(hdr[58:60]) != b"`\n":
            raise SystemExit(f"{path}: bad member header at {pos}")
        name = bytes(hdr[0:16])
        size = int(bytes(hdr[48:58]).strip() or b"0")
        payload = pos + 60
        name_len = int(name[3:].strip() or b"0") if name.startswith(b"#1/") else 0
        start = payload + name_len
        end = payload + size
        if end > len(data):
            raise SystemExit(f"{path}: member overruns file at {pos}")
        if data[start:start + 4] == b"\xcf\xfa\xed\xfe":
            member = bytearray(data[start:end])
            count = patch_macho(member)
            if count:
                data[start:end] = member
                members += 1
                sections += count
        pos = end + (end & 1)
    open(path, "wb").write(data)
    print(f"{path}: members {members} sections {sections}")


if __name__ == "__main__":
    for path in sys.argv[1:]:
        patch_archive(path)
