import contextlib
import io
import os
import pathlib
import plistlib
import struct
import tarfile
import tempfile
import unittest
import zipfile
from unittest import mock

from .testing import Recorder, build_script, patch_all

HFS_2024 = 0xE1B7B100


SEGMENT_ID_CALLS = "".join(f"koly.fUDIFSegmentID.data{n} = rand();\n" for n in range(1, 5))


def read_ds_store(data):
    """(name, code, type, value) of the records in the single leaf of a .DS_Store, and its root block's fields."""
    space = data[4:]
    magic, root_offset, root_size = struct.unpack(">4sII", space[:12])
    root = space[root_offset : root_offset + root_size]
    count = struct.unpack(">I", root[:4])[0]
    addresses = struct.unpack(f">{count}I", root[8 : 8 + 4 * count])
    p = 8 + 4 * 256
    toc = {}
    for _ in range(struct.unpack(">I", root[p : p + 4])[0]):
        length = root[p + 4]
        name = root[p + 5 : p + 5 + length].decode()
        toc[name] = struct.unpack(">I", root[p + 5 + length : p + 9 + length])[0]
        p += 5 + length
    p += 4
    free = {}
    for bits in range(32):
        n = struct.unpack(">I", root[p : p + 4])[0]
        if n:
            free[bits] = list(struct.unpack(f">{n}I", root[p + 4 : p + 4 + 4 * n]))
        p += 4 + 4 * n
    tree = space[addresses[toc["DSDB"]] & ~0x1F :]
    leaf_id, levels, record_count, nodes, page = struct.unpack(">5I", tree[:20])
    leaf = space[addresses[leaf_id] & ~0x1F :]
    p = 8
    records = []
    for _ in range(struct.unpack(">I", leaf[4:8])[0]):
        length = struct.unpack(">I", leaf[p : p + 4])[0]
        name = leaf[p + 4 : p + 4 + 2 * length].decode("utf-16-be")
        p += 4 + 2 * length
        code, kind = leaf[p : p + 4], leaf[p + 4 : p + 8]
        p += 8
        if kind == b"blob":
            size = struct.unpack(">I", leaf[p : p + 4])[0]
            value = leaf[p + 4 : p + 4 + size]
            p += 4 + size
        else:
            value = leaf[p : p + 4]
            p += 4
        records.append((name, code, kind, value))
    return {"magic": magic, "addresses": addresses, "free": free, "tree": (levels, record_count, nodes, page), "records": records}


def alias_tags(alias):
    tags = {}
    p = 150
    while True:
        tag, length = struct.unpack(">HH", alias[p : p + 4])
        if tag == 0xFFFF:
            return tags
        tags[tag] = alias[p + 4 : p + 4 + length]
        p += 4 + length + length % 2


class LibdmgTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_both_clock_calls_are_pinned_and_a_changed_source_is_rejected(self):
        catalog = self.root / "catalog.c"
        catalog.write_text("a = UNIX_TO_APPLE_TIME(time(NULL));\nb = UNIX_TO_APPLE_TIME(time(NULL));\n")
        build_script.pin_clock(catalog, "1704067200")
        self.assertEqual(catalog.read_text().count("UNIX_TO_APPLE_TIME(((time_t) 1704067200))"), 2)
        catalog.write_text("a = UNIX_TO_APPLE_TIME(time(NULL));\n")
        with self.assertRaises(build_script.BuildError):
            build_script.pin_clock(catalog, "1704067200")

    def test_each_segment_id_call_is_pinned_to_the_ids_of_earlier_dmgs_and_a_changed_source_is_rejected(self):
        dmglib = self.root / "dmglib.c"
        dmglib.write_text(SEGMENT_ID_CALLS * 2)
        build_script.pin_segment_id(dmglib)
        pinned = dmglib.read_text()
        self.assertNotIn("rand()", pinned)
        expected = "".join(f"koly.fUDIFSegmentID.data{n} = {value};\n" for n, value in enumerate(build_script.UDIF_SEGMENT_ID, 1))
        self.assertEqual(pinned, expected * 2)
        for changed in (SEGMENT_ID_CALLS, SEGMENT_ID_CALLS * 2 + "seed = rand();\n"):
            dmglib.write_text(changed)
            with self.assertRaises(build_script.BuildError):
                build_script.pin_segment_id(dmglib)

    def test_the_pinned_segment_id_is_the_minimal_standard_sequence_of_macos_rand(self):
        state, values = 1, []
        for _ in build_script.UDIF_SEGMENT_ID:
            state = state * 16807 % (2**31 - 1)
            values.append(state)
        self.assertEqual(tuple(values), build_script.UDIF_SEGMENT_ID)

    def fetch_sources(self, url, digest, target):
        """The libdmg and zlib tarballs, with the files that the build edits."""
        pinned = {
            build_script.LIBDMG_URL: (build_script.LIBDMG_SHA256, f"libdmg-hfsplus-{build_script.LIBDMG_COMMIT}"),
            build_script.ZLIB_URL: (build_script.ZLIB_SHA256, f"zlib-{build_script.ZLIB_VERSION}"),
        }
        self.assertEqual(digest, pinned[url][0])
        top = pinned[url][1]
        files = {"hfs/catalog.c": b"x = time(NULL); y = time(NULL);\n", "dmg/dmglib.c": SEGMENT_ID_CALLS.encode() * 2}
        with tarfile.open(target, "w:gz") as tar:
            for name, data in (files if url == build_script.LIBDMG_URL else {"configure": b""}).items():
                info = tarfile.TarInfo(f"{top}/{name}")
                info.size = len(data)
                tar.addfile(info, io.BytesIO(data))
        return target

    def test_libdmg_is_built_from_the_pinned_source_with_its_clock_and_id_pinned_the_pinned_zlib_and_no_openssl(self):
        recorder = Recorder()
        with patch_all("fetch_pinned", self.fetch_sources), patch_all("run", recorder):
            tools = build_script.build_libdmg(self.root)
        source = self.root / f"libdmg-hfsplus-{build_script.LIBDMG_COMMIT}"
        zlib = self.root / "zlib"
        self.assertIn("((time_t) 1704067200)", (source / "hfs/catalog.c").read_text())
        self.assertNotIn("rand()", (source / "dmg/dmglib.c").read_text())
        self.assertEqual(recorder.commands[:2], [["./configure", "--static", f"--prefix={zlib}"], ["make", "install"]])
        configure = recorder.commands[2]
        self.assertEqual(configure[:5], ["cmake", "-S", str(source), "-B", str(source / "build")])
        self.assertIn(f"-DZLIB_LIBRARY={zlib}/lib/libz.a", configure)
        self.assertIn(f"-DZLIB_INCLUDE_DIR={zlib}/include", configure)
        self.assertIn(f"-DCMAKE_EXE_LINKER_FLAGS=-L{zlib}/lib", configure)
        self.assertIn("-DCMAKE_DISABLE_FIND_PACKAGE_OpenSSL=TRUE", configure)
        self.assertEqual(recorder.commands[3], ["cmake", "--build", str(source / "build"), "--target", "hfsplus", "dmg-bin"])
        self.assertEqual(recorder.commands[4:], [["otool", "-L", str(tools.hfsplus)], ["otool", "-L", str(tools.dmg)]])
        self.assertEqual(tools, build_script.DmgTools(source / "build/hfs/hfsplus", source / "build/dmg/dmg"))

    def test_a_tool_that_loads_the_system_zlib_fails(self):
        system = "dmg:\n\t/usr/lib/libz.1.dylib (compatibility version 1.0.0)\n\t/usr/lib/libSystem.B.dylib\n"
        recorder = Recorder(answers={"otool -L": system})
        zlib_error = self.assertRaisesRegex(build_script.BuildError, "hfsplus loads the system's zlib")
        with patch_all("fetch_pinned", self.fetch_sources), patch_all("run", recorder), zlib_error:
            build_script.build_libdmg(self.root)

    def test_a_tool_that_loads_a_homebrew_library_fails(self):
        homebrew = (
            "/w/build/dmg/dmg:\n"
            "\t/opt/homebrew/opt/openssl@4/lib/libcrypto.4.dylib (compatibility version 4.0.0, current version 4.0.0)\n"
            "\t/usr/lib/libbz2.1.0.dylib (compatibility version 1.0.0, current version 1.0.8)\n"
            "\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1351.0.0)\n"
        )
        expected = "^dmg loads libraries that are not macOS's own: /opt/homebrew/opt/openssl@4/lib/libcrypto.4.dylib$"
        with self.assertRaisesRegex(build_script.BuildError, expected):
            build_script.check_tool_libraries(pathlib.Path("dmg"), homebrew)

    def test_a_tool_that_loads_only_macos_libraries_passes(self):
        system = (
            "/w/build/dmg/dmg:\n"
            "\t/usr/lib/libbz2.1.0.dylib (compatibility version 1.0.0, current version 1.0.8)\n"
            "\t/usr/lib/liblzma.5.dylib (compatibility version 6.0.0, current version 6.3.0)\n"
            "\t/System/Library/Frameworks/CoreFoundation.framework/Versions/A/CoreFoundation (compatibility version 150.0.0)\n"
            "\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1351.0.0)\n"
        )
        build_script.check_tool_libraries(pathlib.Path("dmg"), system)


class HfsTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def test_dates_count_from_1904(self):
        self.assertEqual(build_script.hfs_date("1704067200"), HFS_2024)
        self.assertEqual(build_script.hfs_date("0"), 2082844800)

    def test_names_are_counted_utf16(self):
        self.assertEqual(build_script.hfs_name("SimpleX"), b"\x00\x07" + "SimpleX".encode("utf-16-be"))
        self.assertEqual(build_script.hfs_name(""), b"\x00\x00")

    def test_node_records_are_found_from_offsets_at_its_end(self):
        node = build_script.btree_node(-1, 1, [b"ab", b"cdef"], 64)
        self.assertEqual(len(node), 64)
        self.assertEqual(struct.unpack(">IIbBHH", node[:14]), (0, 0, -1, 1, 2, 0))
        free, second, first = struct.unpack(">3H", node[-6:])
        self.assertEqual((node[first:second], node[second:free]), (b"ab", b"cdef"))
        with self.assertRaises(build_script.BuildError):
            build_script.btree_node(-1, 1, [bytes(60)], 64)

    def test_header_node_maps_the_header_and_root_leaf(self):
        tree = build_script.catalog_tree(100)
        node = build_script.header_node(tree, 2)
        depth, root, leaves, first, last, node_size, max_key, total, free = struct.unpack(">HIIIIHHII", node[14:44])
        self.assertEqual((depth, root, leaves, first, last), (1, 1, 2, 1, 1))
        self.assertEqual((node_size, max_key, total, free), (4096, 516, 100, 98))
        self.assertEqual(node[248], 0xC0)
        self.assertEqual(struct.unpack(">4H", node[-8:]), (4096 - 8, 248, 120, 14))
        empty = build_script.header_node(build_script.EXTENTS_TREE, 0)
        self.assertEqual(struct.unpack(">HIIII", empty[14:32]), (0, 0, 0, 0, 0))
        self.assertEqual(empty[248], 0x80)
        with self.assertRaises(build_script.BuildError):
            build_script.header_node(build_script.catalog_tree(4096 * 8), 2)

    def test_catalog_leaf_has_the_root_folder_and_its_thread(self):
        leaf = build_script.catalog_leaf("SimpleX", HFS_2024, 4096)
        free, thread_at, folder_at = struct.unpack(">3H", leaf[-6:])
        key_length, parent = struct.unpack(">HI", leaf[folder_at : folder_at + 6])
        self.assertEqual((key_length, parent), (6 + 14, 1))
        self.assertEqual(leaf[folder_at + 6 : folder_at + 22], build_script.hfs_name("SimpleX"))
        record = leaf[folder_at + 22 : thread_at]
        self.assertEqual(len(record), 88)
        kind, _, _, folder_id, created = struct.unpack(">hHIII", record[:16])
        self.assertEqual((kind, folder_id, created), (1, 2, HFS_2024))
        self.assertEqual(struct.unpack(">H", record[42:44])[0], 0o40755)
        thread = leaf[thread_at:free]
        self.assertEqual(thread[:8], struct.pack(">HIH", 6, 2, 0))
        self.assertEqual(thread[8:16], struct.pack(">hhI", 3, 0, 1))
        self.assertEqual(thread[16:], build_script.hfs_name("SimpleX"))

    def test_fork_has_one_extent(self):
        fork = build_script.fork(26, 8)
        self.assertEqual(len(fork), 80)
        self.assertEqual(struct.unpack(">QIIII", fork[:24]), (32768, 32768, 8, 26, 8))
        self.assertEqual(fork[24:], bytes(56))

    def test_volume_header_lays_the_metadata_after_the_allocation_file(self):
        catalog = build_script.catalog_tree(10)
        header, allocation, files = build_script.volume_header("SimpleX", 1000, catalog, b"12345678")
        self.assertEqual(len(header), 512)
        self.assertEqual(header[:12], b"H+\x00\x04\x80\x00\x01\x0010.0")
        created, modified, backup, checked = struct.unpack(">4I", header[16:32])
        self.assertEqual((created, modified, backup, checked), (HFS_2024, HFS_2024, 0, HFS_2024))
        block_size, total, free, next_block, _, _, next_id = struct.unpack(">7I", header[40:68])
        metadata = 1 + 1 + 8 + 10 + 16
        self.assertEqual((block_size, total, free, next_block, next_id), (4096, 1000, 1000 - metadata - 1, metadata, 16))
        self.assertEqual(header[104:112], b"12345678")
        starts = [struct.unpack(">I", header[112 + 80 * i + 16 : 112 + 80 * i + 20])[0] for i in range(4)]
        self.assertEqual(starts, [1, 2, 10, 20])
        self.assertEqual(allocation[:5], b"\xff\xff\xff\xff\xf0")
        self.assertEqual(allocation[999 // 8], 0x80 >> (999 % 8))
        self.assertEqual([(start, len(nodes)) for start, nodes in files], [(2, 4096), (10, 2 * 4096), (20, 8192)])
        with self.assertRaises(build_script.BuildError):
            build_script.volume_header("SimpleX", metadata, catalog, b"12345678")

    def test_formatted_volume_has_both_headers_and_counts_nothing(self):
        image = self.root / "volume.hfs"
        catalog = build_script.catalog_tree(10)
        build_script.format_hfs(image, "SimpleX", 1000, catalog, b"12345678")
        data = image.read_bytes()
        self.assertEqual(len(data), 1000 * 4096)
        self.assertEqual(data[1024:1536], data[-1024:-512])
        self.assertEqual(data[10 * 4096 + 4096 + 8], 0xFF)
        self.assertEqual(build_script.volume_counts(image), build_script.VolumeCounts(0, 0, 16))
        self.assertEqual(build_script.catalog_entries(image), {"/": build_script.CatalogEntry(2, 0o40755, 0)})

    def record(self, parent, name, kind, cnid, mode, flags=0):
        key = struct.pack(">I", parent) + build_script.hfs_name(name)
        fields = struct.pack(">hHII20x", kind, 0, 0, cnid) + struct.pack(">IIBBHI", 0, 0, 0, 0, mode, 0)
        return struct.pack(">H", len(key)) + key + fields + struct.pack(">8xH6x16x8x", flags)

    def test_catalog_entries_follow_the_leaves_and_name_items_by_path(self):
        image = self.root / "volume.hfs"
        catalog = build_script.catalog_tree(10)
        build_script.format_hfs(image, "SimpleX", 1000, catalog, b"12345678")
        thread = struct.pack(">HIH", 6, 16, 0) + struct.pack(">hhI", 3, 0, 2) + build_script.hfs_name("a")
        first = bytearray(
            build_script.btree_node(
                -1, 1, [self.record(1, "SimpleX", 1, 2, 0o40755, 0x0400), self.record(2, "a", 1, 16, 0o40700), thread], 4096
            )
        )
        first[0:4] = struct.pack(">I", 2)
        second = build_script.btree_node(-1, 1, [self.record(16, "b", 2, 17, 0o100755)], 4096)
        data = bytearray(image.read_bytes())
        data[11 * 4096 : 13 * 4096] = bytes(first) + second
        image.write_bytes(data)
        self.assertEqual(
            build_script.catalog_entries(image),
            {
                "/": build_script.CatalogEntry(2, 0o40755, 0x0400),
                "/a": build_script.CatalogEntry(16, 0o40700, 0),
                "/a/b": build_script.CatalogEntry(17, 0o100755, 0),
            },
        )

    def test_catalog_nodes_past_the_header_extents_fail(self):
        image = self.root / "volume.hfs"
        build_script.format_hfs(image, "SimpleX", 1000, build_script.catalog_tree(10), b"12345678")
        data = bytearray(image.read_bytes())
        data[11 * 4096 : 11 * 4096 + 4] = struct.pack(">I", 99)
        image.write_bytes(data)
        with self.assertRaises(build_script.BuildError):
            build_script.catalog_entries(image)

    def test_paths_are_sorted_with_folders_before_their_content(self):
        (self.root / "a/b").mkdir(parents=True)
        (self.root / "a/b/c").write_text("c")
        (self.root / "a.txt").write_text("a")
        (self.root / "Applications").symlink_to("/Applications")
        self.assertEqual(
            [p.as_posix() for p in build_script.volume_paths(self.root)],
            ["Applications", "a", "a.txt", "a/b", "a/b/c"],
        )

    def test_volume_id_follows_the_content(self):
        (self.root / "f").write_text("one")
        (self.root / "l").symlink_to("f")
        paths = build_script.volume_paths(self.root)
        first = build_script.volume_id(self.root, paths)
        self.assertEqual(len(first), 8)
        self.assertEqual(build_script.volume_id(self.root, paths), first)
        (self.root / "f").write_text("two")
        self.assertNotEqual(build_script.volume_id(self.root, paths), first)

    def test_volume_has_room_for_its_files_and_a_tenth_more(self):
        (self.root / "d").mkdir()
        (self.root / "big").write_bytes(bytes(4096 * 100 + 1))
        (self.root / "l").symlink_to("big")
        self.assertEqual([build_script.item_blocks(self.root / n) for n in ("d", "big", "l")], [0, 101, 1])
        catalog = build_script.catalog_tree(3)
        blocks = build_script.volume_blocks(self.root, build_script.volume_paths(self.root), catalog)
        self.assertEqual(blocks, 2 + 1 + 8 + 8 + 16 + 102 + 11 + 8)
        self.assertEqual(build_script.item_mode(self.root / "l"), 0o120644)
        self.assertEqual(build_script.item_mode(self.root / "d"), (self.root / "d").stat().st_mode)


class DsStoreTest(unittest.TestCase):
    def test_record_names_are_utf16_and_blobs_sized(self):
        self.assertEqual(build_script.ds_record("a", b"Iloc", b"blob", b"xy"), b"\x00\x00\x00\x01\x00aIlocblob\x00\x00\x00\x02xy")
        self.assertEqual(build_script.ds_record(".", b"vSrn", b"long", b"\x00\x00\x00\x01")[-8:], b"long\x00\x00\x00\x01")

    def test_free_lists_are_the_ones_finder_writes(self):
        expected = {5: [0x20, 0x60], 7: [0x80], 8: [0x100], 9: [0x200], 10: [0x400], 11: [0x800, 0x2800], 12: [0x3000]}
        expected |= {bits: [1 << bits] for bits in range(14, 31)}
        self.assertEqual({bits: offsets for bits, offsets in enumerate(build_script.ds_free_lists()) if offsets}, expected)

    def test_store_has_one_sorted_leaf(self):
        records = [("b", b"Iloc", b"blob", b"1"), (".", b"vSrn", b"long", b"\x00\x00\x00\x01"), ("A", b"Iloc", b"blob", b"2")]
        data = build_script.ds_store(records)
        self.assertEqual(len(data), 4 + 0x2800)
        store = read_ds_store(data)
        self.assertEqual(store["magic"], b"Bud1")
        self.assertEqual(store["addresses"], (0x200B, 0x45, 0x100C))
        self.assertEqual(store["tree"], (0, 3, 1, 0x1000))
        self.assertEqual([r[0] for r in store["records"]], [".", "A", "b"])
        self.assertEqual(store["free"][11], [0x800, 0x2800])
        with self.assertRaises(build_script.BuildError):
            build_script.ds_store([("x", b"Iloc", b"blob", bytes(5000))])

    def test_alias_tags_are_padded_to_even_lengths(self):
        self.assertEqual(build_script.alias_tag(0, b"abc"), b"\x00\x00\x00\x03abc\x00")
        self.assertEqual(build_script.alias_tag(1, b"ab"), b"\x00\x01\x00\x02ab")

    def test_background_alias_points_into_the_volume(self):
        alias = build_script.background_alias("SimpleX", 18, 19)
        self.assertEqual(struct.unpack(">H", alias[4:6])[0], len(alias))
        self.assertEqual(alias[10:18], b"\x07SimpleX")
        volume_date, fs, disk, folder_id = struct.unpack(">I2sHI", alias[38:50])
        self.assertEqual((volume_date, fs, disk, folder_id), (HFS_2024, b"H+", 5, 18))
        self.assertEqual(alias[50:66], b"\x0fbackground.tiff")
        self.assertEqual(struct.unpack(">II", alias[114:122]), (19, HFS_2024))
        tags = alias_tags(alias)
        self.assertEqual(tags[0], b".background")
        self.assertEqual(tags[1], struct.pack(">I", 18))
        self.assertEqual(tags[2], b"SimpleX:.background:\0background.tiff")
        self.assertEqual(tags[16], struct.pack(">Q", HFS_2024 << 16))
        self.assertEqual(tags[18], b"/.background/background.tiff")
        self.assertEqual(tags[19], b"/Volumes/SimpleX")

    def test_window_is_laid_out_as_jpackage_does(self):
        store = read_ds_store(build_script.window_store("SimpleX", "SimpleX.app", 18, 19))
        records = {(name, code): value for name, code, _, value in store["records"]}
        view = plistlib.loads(records[(".", b"icvp")])
        self.assertEqual(view["backgroundImageAlias"], build_script.background_alias("SimpleX", 18, 19))
        self.assertEqual((view["iconSize"], view["backgroundType"], view["arrangeBy"]), (128.0, 2, "none"))
        self.assertEqual(plistlib.loads(records[(".", b"bwsp")])["WindowBounds"], "{{400, 328}, {520, 340}}")
        self.assertEqual(records[("SimpleX.app", b"Iloc")][:8], struct.pack(">II", 120, 130))
        self.assertEqual(records[("Applications", b"Iloc")][:8], struct.pack(">II", 390, 130))


class FakeHfsplus:
    """Stands in for run(), catalog_entries and volume_counts: numbers and counts the items hfsplus adds, takes their
    chmod and attr, except for the target that it misses, and writes dmg's output."""

    def __init__(self, miss=None):
        self.commands = []
        self.miss = miss
        self.entries = {"/": build_script.CatalogEntry(2, 0o40755, 0)}
        self.files = self.folders = 0

    def __call__(self, args, cwd=None, env=None, input=None, check=True, capture=False, title=None):
        args = [str(a) for a in args]
        self.commands.append(args)
        if pathlib.Path(args[0]).name == "dmg":
            pathlib.Path(args[-1]).write_bytes(b"dmg")
        else:
            verb = args[2]
            target = args[4] if verb in ("add", "chmod") else args[3]
            modes = {"mkdir": 0o40755, "add": 0o100644, "symlink": 0o120644}
            if (verb, target) == self.miss:
                pass
            elif verb in modes:
                self.entries[target] = build_script.CatalogEntry(16 + self.files + self.folders, modes[verb], 0)
                self.folders += verb == "mkdir"
                self.files += verb != "mkdir"
            elif verb == "chmod" and target in self.entries:
                entry = self.entries[target]
                self.entries[target] = build_script.CatalogEntry(entry.cnid, entry.mode & 0o170000 | int(args[3], 8), 0)
            elif verb == "attr":
                self.entries[target] = build_script.CatalogEntry(2, 0o40755, 0x0400)
        return mock.Mock(returncode=0, stdout="")

    def catalog_entries(self, image):
        return dict(self.entries)

    def volume_counts(self, image):
        return build_script.VolumeCounts(self.files, self.folders, 16 + self.files + self.folders)

    @contextlib.contextmanager
    def patched(self):
        with patch_all("run", self), patch_all("catalog_entries", self.catalog_entries), patch_all("volume_counts", self.volume_counts):
            yield

    def hfsplus(self, verb):
        return [c[3:] for c in self.commands if pathlib.Path(c[0]).name == "hfsplus" and c[2] == verb]


class VolumeTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        self.app = self.root / "release/SimpleX.app"
        (self.app / "Contents/MacOS").mkdir(parents=True)
        (self.app / "Contents/Resources").mkdir()
        (self.app / "Contents/MacOS/SimpleX").write_bytes(b"exe")
        (self.app / "Contents/MacOS/SimpleX").chmod(0o700)
        (self.app / "Contents/Resources/SimpleX.icns").write_bytes(b"icon")
        (self.app / "Contents/Resources/SimpleX.icns").chmod(0o400)
        (self.app / "Contents/Resources/link").symlink_to("SimpleX.icns")
        self.java = self.root / "jdk"
        (self.java / "jmods").mkdir(parents=True)
        with open(self.java / "jmods/jdk.jpackage.jmod", "wb") as f:
            f.write(b"JM\x01\x00")
            with zipfile.ZipFile(f, "w") as z:
                z.writestr(build_script.JPACKAGE_BACKGROUND, b"tiff")
        self.tools = build_script.DmgTools(self.root / "bin/hfsplus", self.root / "bin/dmg")

    def tearDown(self):
        self.tmp.cleanup()

    def test_background_comes_from_the_jdk_jpackage_module(self):
        target = self.root / "background.tiff"
        build_script.jpackage_background(self.java, target)
        self.assertEqual(target.read_bytes(), b"tiff")

    def test_volume_is_staged_with_the_layout_of_jpackage(self):
        volume = self.root / "volume"
        paths = build_script.stage_volume(self.app, self.java, volume)
        self.assertEqual(
            [p.as_posix() for p in paths[:6]],
            [".DS_Store", ".VolumeIcon.icns", ".background", ".background/background.tiff", "Applications", "SimpleX.app"],
        )
        self.assertFalse(self.app.exists())
        self.assertEqual(os.readlink(volume / "Applications"), "/Applications")
        self.assertEqual((volume / ".VolumeIcon.icns").read_bytes(), b"icon")
        self.assertEqual((volume / ".background/background.tiff").read_bytes(), b"tiff")
        self.assertEqual((volume / "SimpleX.app/Contents/MacOS/SimpleX").stat().st_mode & 0o777, 0o755)
        self.assertEqual((volume / "SimpleX.app/Contents/Resources/SimpleX.icns").stat().st_mode & 0o777, 0o644)
        store = read_ds_store((volume / ".DS_Store").read_bytes())
        view = plistlib.loads(next(r[3] for r in store["records"] if r[1] == b"icvp"))
        self.assertEqual(view["backgroundImageAlias"], build_script.background_alias("SimpleX", 18, 19))

    def test_items_are_added_in_order_with_modes_that_hfsplus_does_not_give(self):
        volume = self.root / "volume"
        paths = build_script.stage_volume(self.app, self.java, volume)
        image = self.root / "volume.hfs"
        build_script.format_hfs(image, "SimpleX", 1000, build_script.catalog_tree(len(paths)), bytes(8))
        fake = FakeHfsplus()
        with fake.patched():
            build_script.add_to_volume(self.tools.hfsplus, image, volume, paths)
        added = [c[-1] for c in fake.commands if c[2] in ("mkdir", "add", "symlink")]
        first = ["/.DS_Store", "/.VolumeIcon.icns", "/.background", "/.background/background.tiff", "/Applications", "/SimpleX.app"]
        self.assertEqual(added[:6], first)
        self.assertIn(["/Applications", "/Applications"], fake.hfsplus("symlink"))
        self.assertIn(["/SimpleX.app/Contents/Resources/link", "SimpleX.icns"], fake.hfsplus("symlink"))
        self.assertEqual(fake.hfsplus("chmod"), [["755", "/SimpleX.app/Contents/MacOS/SimpleX"]])
        self.assertEqual(fake.volume_counts(image), build_script.VolumeCounts(7, 5, 28))

    def test_an_item_numbered_out_of_order_fails(self):
        volume = self.root / "volume"
        paths = build_script.stage_volume(self.app, self.java, volume)
        fake = FakeHfsplus()
        with fake.patched():
            build_script.add_to_volume(self.tools.hfsplus, self.root / "volume.hfs", volume, paths)
            first, second = (f"/{p.as_posix()}" for p in paths[:2])
            fake.entries[first], fake.entries[second] = fake.entries[second], fake.entries[first]
            with self.assertRaises(build_script.BuildError) as raised:
                build_script.check_volume(self.root / "volume.hfs", volume, paths)
        self.assertIn("/.DS_Store", str(raised.exception))

    def test_a_chmod_that_hfsplus_did_not_make_fails(self):
        volume = self.root / "volume"
        paths = build_script.stage_volume(self.app, self.java, volume)
        with (
            FakeHfsplus(miss=("chmod", "/SimpleX.app/Contents/MacOS/SimpleX")).patched(),
            self.assertRaises(build_script.BuildError) as raised,
        ):
            build_script.add_to_volume(self.tools.hfsplus, self.root / "volume.hfs", volume, paths)
        self.assertIn("/SimpleX.app/Contents/MacOS/SimpleX", str(raised.exception))

    def test_a_custom_icon_that_hfsplus_did_not_set_fails(self):
        fake = FakeHfsplus()
        with fake.patched():
            build_script.set_custom_icon(self.tools.hfsplus, self.root / "volume.hfs")
        self.assertEqual(fake.hfsplus("attr"), [["/", "C"]])
        with FakeHfsplus(miss=("attr", "/")).patched(), self.assertRaises(build_script.BuildError):
            build_script.set_custom_icon(self.tools.hfsplus, self.root / "volume.hfs")

    def test_an_item_that_hfsplus_did_not_add_fails(self):
        volume = self.root / "volume"
        paths = build_script.stage_volume(self.app, self.java, volume)
        fake = FakeHfsplus(miss=("add", "/SimpleX.app/Contents/MacOS/SimpleX"))
        with fake.patched(), self.assertRaises(build_script.BuildError):
            build_script.add_to_volume(self.tools.hfsplus, self.root / "volume.hfs", volume, paths)

    def test_non_ascii_names_are_rejected(self):
        (self.app / "Contents/Resources/é").write_text("x")
        volume = self.root / "volume"
        paths = build_script.stage_volume(self.app, self.java, volume)
        with FakeHfsplus().patched(), self.assertRaises(build_script.BuildError):
            build_script.add_to_volume(self.tools.hfsplus, self.root / "volume.hfs", volume, paths)

    def test_dmg_is_compressed_from_the_volume_with_its_custom_icon(self):
        work = self.root / "work"
        work.mkdir()
        fake = FakeHfsplus()
        with fake.patched():
            build_script.build_dmg(self.tools, self.app, self.java, work, self.root / "out.dmg")
        self.assertEqual(fake.hfsplus("attr"), [["/", "C"]])
        image = str(work / "volume.hfs")
        dmg = [str(self.tools.dmg), "--compression", "zlib", "--level", "9", "build", image, str(self.root / "out.dmg")]
        self.assertEqual(fake.commands[-1], dmg)
        self.assertEqual((self.root / "out.dmg").read_bytes(), b"dmg")
        self.assertEqual(pathlib.Path(image).read_bytes()[1024:1026], b"H+")


def udif(data, signature=b""):
    """A dmg's data and plist followed by its koly block, with signature before the block as codesign puts it."""
    koly = bytearray(512)
    koly[:4] = b"koly"
    struct.pack_into(">QQ", koly, 0xD8, len(data) - 4, 4)
    if signature:
        struct.pack_into(">QQ", koly, 0x128, len(data), len(signature))
    return data + signature + bytes(koly)


class DmgSignatureTest(unittest.TestCase):
    def test_signature_of_a_release_is_copied_before_the_koly_block(self):
        built = udif(b"compressed volume" * 10 + b"plst")
        released = udif(b"compressed volume" * 10 + b"plst", b"\xfa\xde\x0c\xc0 signature and ticket")
        self.assertEqual(build_script.dmg_signature(built), (0, 0))
        self.assertEqual(build_script.dmg_signature(released), (174, 25))
        self.assertEqual(build_script.copy_dmg_signature(released, built), released)
        self.assertEqual(build_script.copy_dmg_signature(built, built), built)

    def test_other_volume_or_koly_does_not_match(self):
        released = udif(b"compressed volume" * 10 + b"plst", b"signature")
        self.assertNotEqual(build_script.copy_dmg_signature(released, udif(b"compressed volumE" * 10 + b"plst")), released)
        other_koly = bytearray(udif(b"compressed volume" * 10 + b"plst"))
        other_koly[-4] = 1
        self.assertNotEqual(build_script.copy_dmg_signature(released, bytes(other_koly)), released)
