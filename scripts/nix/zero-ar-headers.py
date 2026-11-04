"""Zeroes the dates and owners in the member headers of ar archives, which GHC and strip take from the build's clock and user."""

import struct
import sys
from pathlib import Path

AR_MAGIC = b"!<arch>\n"


# name, date, uid, gid, mode, size and end of a member header
AR_HEADER = struct.Struct("16s12s6s6s8s10s2s")


AR_HEADER_END = b"`\n"


# the date, uid, gid and mode that llvm-ar's deterministic mode writes
DETERMINISTIC_FIELDS = (b"0".ljust(12), b"0".ljust(6), b"0".ljust(6), b"644".ljust(8))


class ArchiveError(Exception):
    pass


def zero_headers(data: bytes) -> bytes:
    if not data.startswith(AR_MAGIC):
        raise ArchiveError("not an ar archive")
    zeroed = bytearray(data)
    offset = len(AR_MAGIC)
    while offset < len(zeroed):
        if offset + AR_HEADER.size > len(zeroed):
            raise ArchiveError(f"ends in a member header at {offset}")
        name, *_, size, end = AR_HEADER.unpack_from(zeroed, offset)
        if end != AR_HEADER_END:
            raise ArchiveError(f"no member header at {offset}")
        AR_HEADER.pack_into(zeroed, offset, name, *DETERMINISTIC_FIELDS, size, end)
        # a member of odd size is padded with a newline
        offset += AR_HEADER.size + int(size) + int(size) % 2
    return bytes(zeroed)


def main(paths: list[str]) -> None:
    for path in map(Path, paths):
        try:
            path.write_bytes(zero_headers(path.read_bytes()))
        except ArchiveError as e:
            sys.exit(f"{path}: {e}")


if __name__ == "__main__":
    main(sys.argv[1:])
