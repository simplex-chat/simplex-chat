import pathlib
import struct
import tempfile
import unittest

from .testing import build_script

HEADER = "<IiiIIIII"
CPU_ARM64 = 0x0100000C
DATA = b"\x1f\x20\x03\xd5" * 4  # four arm64 nops


def device_object(extra_commands=b"", extra_ncmds=0):
    """An arm64 object as the Nix iOS build leaves it: one __text section with a relocation, a symbol table, and
    LC_VERSION_MIN_IPHONEOS."""
    segment_size, symtab_size, version_size = 72 + 80, 24, 16
    sizeofcmds = segment_size + symtab_size + version_size + len(extra_commands)
    data_start = 32 + sizeofcmds
    reloc_off = data_start + len(DATA)
    sym_off = reloc_off + 8
    str_off = sym_off + 16
    strings = b"\x00_main\x00\x00"
    segment = struct.pack("<II16sQQQQiiII", 0x19, segment_size, b"", 0, len(DATA), data_start, len(DATA), 7, 7, 1, 0)
    section = struct.pack("<16s16sQQIIIIIIII", b"__text", b"__TEXT", 0, len(DATA), data_start, 2, reloc_off, 1, 0x80000400, 0, 0, 0)
    symtab = struct.pack("<IIIIII", 0x2, symtab_size, sym_off, 1, str_off, len(strings))
    version = struct.pack("<IIII", 0x25, version_size, 0x000B0000, 0x00110000)
    header = struct.pack(HEADER, 0xFEEDFACF, CPU_ARM64, 0, 1, 3 + extra_ncmds, sizeofcmds, 0x2000, 0)
    relocation = struct.pack("<iI", 4, 0x2D000000)
    symbol = struct.pack("<IBBHQ", 1, 0x0F, 1, 0, 0)
    return header + segment + section + symtab + version + extra_commands + DATA + relocation + symbol + strings


class MachoTest(unittest.TestCase):
    def test_device_object_becomes_a_simulator_object_with_its_contents_moved_intact(self):
        dev = device_object()
        sim = build_script.simulator_object(dev)
        self.assertEqual(len(sim), len(dev) + 16)
        _, _, _, _, ncmds, sizeofcmds, _, _ = struct.unpack_from(HEADER, sim)
        self.assertEqual((ncmds, sizeofcmds), (3, struct.unpack_from(HEADER, dev)[5] + 16))
        build = sim[32 + 152 + 24 : 32 + 152 + 24 + 32]
        self.assertEqual(struct.unpack("<IIIIIIII", build), (0x32, 32, 7, 0x000B0000, 0x00110000, 1, 1, 0x00110000))
        fileoff, section_offset, reloff = (
            struct.unpack_from("<Q", sim, 32 + 40)[0],
            *struct.unpack_from("<I", sim, 32 + 72 + 48),
            struct.unpack_from("<I", sim, 32 + 72 + 56)[0],
        )
        self.assertEqual(sim[section_offset : section_offset + len(DATA)], DATA)
        self.assertEqual(fileoff, section_offset)
        self.assertEqual(sim[reloff : reloff + 8], dev[reloff - 16 : reloff - 8])
        sym_off, _, str_off, str_size = struct.unpack_from("<IIII", sim, 32 + 152 + 8)
        self.assertEqual(sim[str_off : str_off + str_size], b"\x00_main\x00\x00")
        self.assertEqual(sim[sym_off : sym_off + 16], dev[sym_off - 16 : sym_off])

    def test_objects_without_the_old_command_stay_as_they_are(self):
        sim = build_script.simulator_object(device_object())
        self.assertIs(build_script.simulator_object(sim), sim)
        self.assertEqual(build_script.simulator_object(b"\x00" * 64), b"\x00" * 64)

    def test_unknown_load_commands_are_refused(self):
        unknown = struct.pack("<IIII", 0x99, 16, 0, 0)
        with self.assertRaises(build_script.BuildError):
            build_script.simulator_object(device_object(unknown, 1))

    def test_archive_keeps_its_members_and_drops_the_symbol_table(self):
        dev = device_object()
        with tempfile.TemporaryDirectory() as tmp:
            lib = pathlib.Path(tmp) / "libgmp.a"
            members = b"!<arch>\n"
            for name, body in ((b"__.SYMDEF SORTED\x00\x00\x00\x00", b"symdef\x00\x00"), (b"assert.o\x00\x00\x00\x00", dev)):
                size = len(name) + len(body)
                header = (
                    f"#1/{len(name)}".ljust(16)
                    + "0".ljust(12)
                    + "0".ljust(6)
                    + "0".ljust(6)
                    + "100644".ljust(8)
                    + str(size).ljust(10)
                    + "`\n"
                )
                members += header.encode() + name + body
            lib.write_bytes(members)
            self.assertEqual(build_script.simulator_archive(lib), 1)
            ((header, name, contents),) = build_script.archive_members(lib.read_bytes())
        self.assertEqual(name, b"assert.o\x00\x00\x00\x00")
        self.assertEqual(int(header[48:58]), len(name) + len(dev) + 16)
        self.assertEqual(contents, build_script.simulator_object(dev))


EXECUTE = 0x2
CODE_START = 0x400
PAGE = 0x4000


def executable(code=DATA * 8, linkedit=b"symbols!" * 3, room=True):
    """A thin arm64 executable as ld leaves it unsigned: __TEXT and __LINKEDIT, the code and the __LINKEDIT data, and
    free space after the load commands unless room is False."""
    sizeofcmds = 2 * 72
    linkedit_off = CODE_START + len(code)
    text = struct.pack("<II16sQQQQiiII", 0x19, 72, b"__TEXT", 0, PAGE, 0, linkedit_off, 5, 5, 0, 0)
    seg = struct.pack("<II16sQQQQiiII", 0x19, 72, b"__LINKEDIT", PAGE, PAGE, linkedit_off, len(linkedit), 1, 1, 0, 0)
    header = struct.pack(HEADER, 0xFEEDFACF, CPU_ARM64, 0, EXECUTE, 2, sizeofcmds, 0, 0)
    commands = header + text + seg
    fill = b"\x00" if room else b"\xcc"
    return commands + fill * (CODE_START - len(commands)) + code + linkedit


def codesign(binary, signature):
    """binary signed as codesign signs it: an LC_CODE_SIGNATURE after the other commands, or the one it has, pointing
    at signature after the __LINKEDIT data aligned to 16 bytes, which __LINKEDIT then covers."""
    data = bytearray(binary)
    ncmds, sizeofcmds = struct.unpack_from("<II", data, 16)
    command = 32
    for _ in range(ncmds):
        cmd, size = struct.unpack_from("<II", data, command)
        if cmd == 0x1D:
            dataoff = struct.unpack_from("<I", data, command + 8)[0]
            del data[dataoff:]
            break
        command += size
    else:
        struct.pack_into("<II", data, 16, ncmds + 1, sizeofcmds + 16)
        data += bytes(-len(data) % 16)
        dataoff = len(data)
    struct.pack_into("<IIII", data, command, 0x1D, 16, dataoff, len(signature))
    linkedit = 32 + 72
    fileoff = struct.unpack_from("<Q", data, linkedit + 40)[0]
    filesize = dataoff + len(signature) - fileoff
    struct.pack_into("<QQ", data, linkedit + 32, -(-filesize // PAGE) * PAGE, fileoff)
    struct.pack_into("<Q", data, linkedit + 48, filesize)
    return bytes(data + signature)


def universal(*slices):
    """A universal file of thin slices, each aligned to 2^14 as lipo aligns arm64 slices."""
    header = struct.pack(">II", 0xCAFEBABE, len(slices))
    offset, arches, body = PAGE, b"", b""
    for i, thin in enumerate(slices):
        arches += struct.pack(">iiIII", CPU_ARM64 + i, 0, offset, len(thin), 14)
        body += bytes(offset - PAGE - len(body)) + thin
        offset += -(-len(thin) // PAGE) * PAGE
    return header + arches + bytes(PAGE - len(header) - len(arches)) + body


DEVELOPER_ID = b"\xfa\xde\x0c\xc0" + b"cms and timestamp" * 40
AD_HOC = b"\xfa\xde\x0c\xc0" + b"ad hoc" * 3


class CodeSignatureTest(unittest.TestCase):
    def test_signature_of_a_release_is_copied_onto_an_unsigned_build(self):
        built = executable()
        released = codesign(built, DEVELOPER_ID)
        self.assertEqual(build_script.copy_code_signature(released, built), released)
        self.assertTrue(build_script.same_code(released, built))

    def test_signature_of_a_release_replaces_the_ad_hoc_signature_of_a_build(self):
        built = codesign(executable(), AD_HOC)
        released = codesign(built, DEVELOPER_ID)
        self.assertNotEqual(len(built), len(released))
        self.assertEqual(build_script.copy_code_signature(released, built), released)

    def test_a_signed_build_of_other_code_does_not_match(self):
        released = codesign(executable(), DEVELOPER_ID)
        other_code = executable(code=DATA * 7 + b"\x00\x00\x00\x00" * 4)
        other_symbols = codesign(executable(linkedit=b"symbols?" * 3), AD_HOC)
        for built in (other_code, other_symbols, executable(code=DATA * 9)):
            self.assertFalse(build_script.same_code(released, built))

    def test_signature_command_does_not_hide_a_build_with_no_free_space(self):
        released = codesign(executable(), DEVELOPER_ID)
        crowded = executable(room=False)
        self.assertEqual(build_script.copy_code_signature(released, crowded), crowded)
        self.assertFalse(build_script.same_code(released, crowded))

    def test_an_unsigned_release_and_other_files_are_compared_as_they_are(self):
        built = executable()
        self.assertTrue(build_script.same_code(built, built))
        self.assertFalse(build_script.same_code(executable(code=DATA * 9), built))
        self.assertFalse(build_script.same_code(b"#!/bin/sh\n", b"#!/bin/bash\n"))
        self.assertEqual(build_script.copy_code_signature(b"text", built), built)

    def test_each_architecture_of_a_universal_file_gets_its_signature(self):
        thin = executable(), executable(code=DATA * 16)
        built = universal(codesign(thin[0], AD_HOC), thin[1])
        released = universal(codesign(thin[0], DEVELOPER_ID), codesign(thin[1], DEVELOPER_ID))
        self.assertTrue(build_script.same_code(released, built))
        self.assertFalse(build_script.same_code(released, universal(thin[1], thin[0])))
        self.assertFalse(build_script.same_code(released, universal(thin[0])))

    def test_java_classes_are_not_universal_files(self):
        java_class = struct.pack(">IHH", 0xCAFEBABE, 0, 65) + b"class"
        self.assertFalse(build_script.is_macho(java_class))
        self.assertTrue(build_script.is_macho(universal(executable())))

    def test_truncated_files_do_not_match(self):
        released = codesign(executable(), DEVELOPER_ID)
        self.assertFalse(build_script.same_code(released, released[:40]))
