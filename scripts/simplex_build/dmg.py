"""The desktop app's dmg, the same byte for byte on each build: an HFS+ volume written here and filled by libdmg-hfsplus,
as Tor Browser builds its dmgs, with the window layout that jpackage gave it."""

import hashlib
import math
import os
import plistlib
import re
import shutil
import stat
import struct
import tarfile
import zipfile
from dataclasses import dataclass
from pathlib import Path

from .common import BuildError, fetch_pinned, normalize_permissions, run
from .desktop import SOURCE_DATE_EPOCH

LIBDMG_COMMIT = "ec239599c1f234a4e01ae3fe51214d0c77e5baa3"


LIBDMG_URL = f"https://github.com/mozilla/libdmg-hfsplus/archive/{LIBDMG_COMMIT}.tar.gz"


LIBDMG_SHA256 = "190e29e4e0bd53e31af1f617b62dcd46b300ffbb0b7cd9e1bf49adb218872763"


# hfsplus dates each file and folder that it adds with the current time, in these calls of hfs/catalog.c
LIBDMG_CLOCK = "time(NULL)"


LIBDMG_CLOCK_CALLS = 2


# dmg numbers each image with four calls of the unseeded rand(), in buildDmg and in convertToDMG of dmg/dmglib.c
LIBDMG_SEGMENT_ID = re.compile(r"(fUDIFSegmentID\.data)([1-4]) = rand\(\);")


LIBDMG_SEGMENT_ID_CALLS = 8


# the first four values of macOS's unseeded rand(), 16807^n mod 2^31-1, which the dmgs built before had as their id
UDIF_SEGMENT_ID = (16807, 282475249, 1622650073, 984943658)


# dmg build compresses with the zlib that CMake finds, which is the SDK's unless it is given
ZLIB_VERSION = "1.3.1"


ZLIB_URL = f"https://github.com/madler/zlib/releases/download/v{ZLIB_VERSION}/zlib-{ZLIB_VERSION}.tar.gz"


ZLIB_SHA256 = "9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23"


DMG_COMPRESSION = ("--compression", "zlib", "--level", "9")


# OpenSSL only decrypts FileVault images for dmg's --key, which the build doesn't use; CMake finds Homebrew's
LIBDMG_WITHOUT_OPENSSL = "-DCMAKE_DISABLE_FIND_PACKAGE_OpenSSL=TRUE"


MACOS_LIBRARY_DIRS = ("/usr/lib/", "/System/Library/")


VOLUME_NAME = "SimpleX"


# jpackage's background picture, which its dmgs show behind the app and the Applications link
JPACKAGE_BACKGROUND = "classes/jdk/jpackage/internal/resources/background_dmg.tiff"


BACKGROUND_DIR = ".background"


BACKGROUND_FILE = "background.tiff"


VOLUME_ICON = ".VolumeIcon.icns"


DS_STORE = ".DS_Store"


APPLICATIONS = "Applications"


# the window that jpackage's DMGsetup.scpt sets up: its bounds, icon size, and where the two icons are
WINDOW_BOUNDS = "{{400, 328}, {520, 340}}"


ICON_SIZE = 128.0


TEXT_SIZE = 12.0


APP_ICON_POSITION = (120, 130)


APPLICATIONS_ICON_POSITION = (390, 130)


HFS_EPOCH_OFFSET = 2082844800  # from 1904-01-01, where HFS+ dates start, to 1970-01-01


BLOCK_SIZE = 4096


VOLUME_HEADER_OFFSET = 1024


VOLUME_HEADER_SIZE = 512


# the volume header's catalog file fork, after its allocation and extents file forks
CATALOG_FORK_OFFSET = 112 + 2 * 80


# block 0, which holds the volume header, and the last block, which holds the alternate one
HEADER_BLOCKS = 2


HFS_PLUS_VERSION = 4


# kHFSVolumeUnmountedBit and kHFSUnusedNodeFixBit, as newfs_hfs sets them
VOLUME_ATTRIBUTES = 0x80000100


# kHFSPlusMountVersion of a volume that newfs_hfs wrote and no journaling driver mounted
LAST_MOUNTED_VERSION = b"10.0"


# the volume header's bitmap of the text encodings of its names: bit 0, MacRoman
ENCODINGS_BITMAP_MAC_ROMAN = 1


CLUMP_SIZE = 65536


ROOT_PARENT_ID = 1


ROOT_FOLDER_ID = 2


FIRST_USER_ID = 16


ROOT_MODE = stat.S_IFDIR | 0o755


# the modes that hfsplus gives the files and folders it adds
HFSPLUS_FILE_MODE = 0o644


HFSPLUS_FOLDER_MODE = 0o755


NODE_DESCRIPTOR = ">IIbBHH"


NODE_DESCRIPTOR_SIZE = 14


HEADER_NODE = 1


LEAF_NODE = -1


FOLDER_RECORD = 1


FILE_RECORD = 2


FOLDER_THREAD_RECORD = 3


BIG_KEYS = 2


VARIABLE_INDEX_KEYS = 4


CASE_FOLDING = 0xCF


# the header, user data and map records' offsets and the free space offset at the end of a header node
HEADER_NODE_OVERHEAD = 256


# about five times what SimpleX.app's catalog takes: a 4 KB leaf, half full, holds five items' file and thread records
CATALOG_NODES_PER_ITEM = 1


MIN_TREE_NODES = 8


# the share of the volume left free beyond the files' blocks, as Tor Browser's ddmg.sh leaves
FREE_SPACE_SHARE = 0.1


MIN_FREE_BLOCKS = 8


# the offsets in a catalog file or folder record of the fields that the build checks
RECORD_ID_OFFSET = 8


RECORD_MODE_OFFSET = 42


RECORD_FINDER_FLAGS_OFFSET = 56


HAS_CUSTOM_ICON = 0x0400


@dataclass(frozen=True, slots=True)
class BTree:
    node_size: int
    nodes: int
    max_key_length: int
    key_compare_type: int
    attributes: int

    @property
    def blocks(self) -> int:
        return self.nodes * self.node_size // BLOCK_SIZE


EXTENTS_TREE = BTree(4096, MIN_TREE_NODES, 10, 0, BIG_KEYS)


ATTRIBUTES_TREE = BTree(8192, MIN_TREE_NODES, 266, 0, BIG_KEYS | VARIABLE_INDEX_KEYS)


def catalog_tree(items: int) -> BTree:
    return BTree(4096, max(MIN_TREE_NODES, items * CATALOG_NODES_PER_ITEM), 516, CASE_FOLDING, BIG_KEYS | VARIABLE_INDEX_KEYS)


@dataclass(frozen=True, slots=True)
class DmgTools:
    hfsplus: Path
    dmg: Path


def pin_clock(catalog: Path, epoch: str) -> None:
    text = catalog.read_text()
    if text.count(LIBDMG_CLOCK) != LIBDMG_CLOCK_CALLS:
        raise BuildError(f"expected {LIBDMG_CLOCK_CALLS} calls of {LIBDMG_CLOCK} in {catalog}")
    catalog.write_text(text.replace(LIBDMG_CLOCK, f"((time_t) {epoch})"))


def pin_segment_id(dmglib: Path) -> None:
    text = dmglib.read_text()
    if len(LIBDMG_SEGMENT_ID.findall(text)) != LIBDMG_SEGMENT_ID_CALLS or text.count("rand()") != LIBDMG_SEGMENT_ID_CALLS:
        raise BuildError(f"expected {LIBDMG_SEGMENT_ID_CALLS} calls of rand(), each for the segment id, in {dmglib}")
    dmglib.write_text(LIBDMG_SEGMENT_ID.sub(lambda m: f"{m[1]}{m[2]} = {UDIF_SEGMENT_ID[int(m[2]) - 1]};", text))


def build_zlib(work: Path) -> Path:
    """The pinned zlib, only its static library, installed in the directory that it returns."""
    with tarfile.open(fetch_pinned(ZLIB_URL, ZLIB_SHA256, work / "zlib.tar.gz")) as tar:
        tar.extractall(work, filter="data")
    source = work / f"zlib-{ZLIB_VERSION}"
    prefix = work / "zlib"
    run(["./configure", "--static", f"--prefix={prefix}"], cwd=source)
    run(["make", "install"], cwd=source)
    return prefix


def check_tool_libraries(tool: Path, dependencies: str) -> None:
    """dependencies: what otool -L prints of the tool: only macOS's own libraries, without its zlib."""
    libraries = [line.split(" (", 1)[0].strip() for line in dependencies.splitlines() if line.startswith("\t")]
    if any("/libz." in library for library in libraries):
        raise BuildError(f"{tool.name} loads the system's zlib, instead of linking the pinned zlib statically")
    outside = [library for library in libraries if not library.startswith(MACOS_LIBRARY_DIRS)]
    if outside:
        raise BuildError(f"{tool.name} loads libraries that are not macOS's own: {', '.join(outside)}")


def build_libdmg(work: Path) -> DmgTools:
    """hfsplus and dmg of libdmg-hfsplus, linked with the pinned zlib, which date what they add with SOURCE_DATE_EPOCH and
    number the image with UDIF_SEGMENT_ID."""
    zlib = build_zlib(work)
    with tarfile.open(fetch_pinned(LIBDMG_URL, LIBDMG_SHA256, work / "libdmg-hfsplus.tar.gz")) as tar:
        tar.extractall(work, filter="data")
    source = work / f"libdmg-hfsplus-{LIBDMG_COMMIT}"
    pin_clock(source / "hfs/catalog.c", SOURCE_DATE_EPOCH)
    pin_segment_id(source / "dmg/dmglib.c")
    build = source / "build"
    zlib_args = [
        f"-DZLIB_INCLUDE_DIR={zlib / 'include'}",
        f"-DZLIB_LIBRARY={zlib / 'lib/libz.a'}",
        # the CMakeLists link zlib by its name, z, which the linker looks for in -L directories before the SDK's
        f"-DCMAKE_EXE_LINKER_FLAGS=-L{zlib / 'lib'}",
    ]
    run(["cmake", "-S", source, "-B", build, "-DCMAKE_BUILD_TYPE=Release", LIBDMG_WITHOUT_OPENSSL, *zlib_args])
    run(["cmake", "--build", build, "--target", "hfsplus", "dmg-bin"])
    tools = DmgTools(build / "hfs/hfsplus", build / "dmg/dmg")
    for tool in (tools.hfsplus, tools.dmg):
        check_tool_libraries(tool, run(["otool", "-L", tool], capture=True).stdout)
    return tools


def hfs_date(epoch: str) -> int:
    return int(epoch) + HFS_EPOCH_OFFSET


def hfs_name(name: str) -> bytes:
    encoded = name.encode("utf-16-be")
    return struct.pack(">H", len(encoded) // 2) + encoded


def btree_node(kind: int, height: int, records: list[bytes], size: int) -> bytes:
    """A B-tree node: its descriptor, its records, and their offsets backwards from its end, the last to its free space."""
    body = struct.pack(NODE_DESCRIPTOR, 0, 0, kind, height, len(records), 0)
    offsets = []
    for record in records:
        offsets.append(len(body))
        body += record
    offsets.append(len(body))
    tail = struct.pack(f">{len(offsets)}H", *reversed(offsets))
    if len(body) + len(tail) > size:
        raise BuildError(f"B-tree node records take {len(body) + len(tail)} bytes, more than {size}")
    return body + bytes(size - len(body) - len(tail)) + tail


def header_node(tree: BTree, leaf_records: int) -> bytes:
    """The header node of tree, which has one other node, its root leaf node 1, when it has leaf records."""
    leaf = 1 if leaf_records else 0
    used = 1 + leaf
    map_size = tree.node_size - HEADER_NODE_OVERHEAD
    if tree.nodes > map_size * 8:
        raise BuildError(f"a B-tree of {tree.nodes} nodes needs map nodes")
    header = struct.pack(
        ">HIIIIHHIIHIBBI64x",
        leaf,
        leaf,
        leaf_records,
        leaf,
        leaf,
        tree.node_size,
        tree.max_key_length,
        tree.nodes,
        tree.nodes - used,
        0,
        tree.nodes * tree.node_size,
        0,
        tree.key_compare_type,
        tree.attributes,
    )
    node_map = bytearray(map_size)
    for node in range(used):
        node_map[node // 8] |= 0x80 >> (node % 8)
    return btree_node(HEADER_NODE, 0, [header, bytes(128), bytes(node_map)], tree.node_size)


def catalog_leaf(volume_name: str, date: int, node_size: int) -> bytes:
    """The root folder's record, keyed by its parent and the volume's name, and its thread record."""
    key = struct.pack(">I", ROOT_PARENT_ID) + hfs_name(volume_name)
    permissions = struct.pack(">IIBBHI", 0, 0, 0, 0, ROOT_MODE, 0)
    folder = struct.pack(">hHII5I", FOLDER_RECORD, 0, 0, ROOT_FOLDER_ID, date, date, date, date, 0) + permissions + bytes(40)
    thread_key = struct.pack(">I", ROOT_FOLDER_ID) + hfs_name("")
    thread = struct.pack(">hhI", FOLDER_THREAD_RECORD, 0, ROOT_PARENT_ID) + hfs_name(volume_name)
    records = [struct.pack(">H", len(key)) + key + folder, struct.pack(">H", len(thread_key)) + thread_key + thread]
    return btree_node(LEAF_NODE, 1, records, node_size)


def fork(start: int, blocks: int) -> bytes:
    size = blocks * BLOCK_SIZE
    return struct.pack(">QIIII56x", size, size, blocks, start, blocks)


def volume_header(volume_name: str, total_blocks: int, catalog: BTree, volume_id: bytes) -> tuple[bytes, bytes, list[tuple[int, bytes]]]:
    """The volume header, the allocation file, and the start blocks and first nodes of the extents, catalog and
    attributes files, which follow it; the last block, which holds the alternate header, is in use."""
    allocation_blocks = math.ceil(math.ceil(total_blocks / 8) / BLOCK_SIZE)
    date = hfs_date(SOURCE_DATE_EPOCH)
    nodes = [
        header_node(EXTENTS_TREE, 0),
        header_node(catalog, 2) + catalog_leaf(volume_name, date, catalog.node_size),
        header_node(ATTRIBUTES_TREE, 0),
    ]
    files = []
    next_block = 1 + allocation_blocks
    for tree, first_nodes in zip((EXTENTS_TREE, catalog, ATTRIBUTES_TREE), nodes, strict=True):
        files.append((next_block, first_nodes))
        next_block += tree.blocks
    if next_block >= total_blocks:
        raise BuildError(f"an HFS+ volume of {total_blocks} blocks has no room for its {next_block} blocks of metadata")
    allocation = bytearray(allocation_blocks * BLOCK_SIZE)
    for block in [*range(next_block), total_blocks - 1]:
        allocation[block // 8] |= 0x80 >> (block % 8)
    header = struct.pack(
        ">2sHI4sI4I2II2II2IIIQ24x8s",
        b"H+",
        HFS_PLUS_VERSION,
        VOLUME_ATTRIBUTES,
        LAST_MOUNTED_VERSION,
        0,
        date,
        date,
        0,
        date,
        0,
        0,
        BLOCK_SIZE,
        total_blocks,
        total_blocks - next_block - 1,
        next_block,
        CLUMP_SIZE,
        CLUMP_SIZE,
        FIRST_USER_ID,
        0,
        ENCODINGS_BITMAP_MAC_ROMAN,
        volume_id,
    )
    header += fork(1, allocation_blocks)
    for (start, _), tree in zip(files, (EXTENTS_TREE, catalog, ATTRIBUTES_TREE), strict=True):
        header += fork(start, tree.blocks)
    header += fork(0, 0)
    return header, bytes(allocation), files


def format_hfs(image: Path, volume_name: str, total_blocks: int, catalog: BTree, volume_id: bytes) -> None:
    """An empty HFS+ volume, as newfs_hfs writes it, with SOURCE_DATE_EPOCH's dates and the volume id given."""
    header, allocation, files = volume_header(volume_name, total_blocks, catalog, volume_id)
    with image.open("wb") as f:
        f.truncate(total_blocks * BLOCK_SIZE)
        for offset in (VOLUME_HEADER_OFFSET, total_blocks * BLOCK_SIZE - VOLUME_HEADER_OFFSET):
            f.seek(offset)
            f.write(header)
        f.seek(BLOCK_SIZE)
        f.write(allocation)
        for start, nodes in files:
            f.seek(start * BLOCK_SIZE)
            f.write(nodes)


@dataclass(frozen=True, slots=True)
class VolumeCounts:
    """What a volume header counts: its files and folders, without the root folder, and the next catalog node id."""

    files: int
    folders: int
    next_id: int


def volume_counts(image: Path) -> VolumeCounts:
    with image.open("rb") as f:
        f.seek(VOLUME_HEADER_OFFSET)
        header = f.read(VOLUME_HEADER_SIZE)
    files, folders = struct.unpack(">II", header[32:40])
    (next_id,) = struct.unpack(">I", header[64:68])
    return VolumeCounts(files, folders, next_id)


@dataclass(frozen=True, slots=True)
class CatalogEntry:
    cnid: int
    mode: int
    finder_flags: int


def catalog_entries(image: Path) -> dict[str, CatalogEntry]:
    """The folders and files of image by their paths, "/" for the root folder, from the leaf nodes of its catalog,
    which must be in the extents of its volume header."""
    with image.open("rb") as f:
        f.seek(VOLUME_HEADER_OFFSET)
        header = f.read(VOLUME_HEADER_SIZE)
        (block_size,) = struct.unpack(">I", header[40:44])
        fields = struct.unpack(">16I", header[CATALOG_FORK_OFFSET + 16 : CATALOG_FORK_OFFSET + 80])
        extents = [(fields[i], fields[i + 1]) for i in range(0, 16, 2) if fields[i + 1]]

        def read(position: int, size: int) -> bytes:
            block, skipped = position // block_size, 0
            for start, count in extents:
                if block < skipped + count:
                    f.seek((start + block - skipped) * block_size + position % block_size)
                    return f.read(size)
                skipped += count
            raise BuildError(f"{image.name}: catalog byte {position} is past the extents in its volume header")

        (first_leaf,) = struct.unpack(">I", read(NODE_DESCRIPTOR_SIZE + 10, 4))
        (node_size,) = struct.unpack(">H", read(NODE_DESCRIPTOR_SIZE + 18, 2))
        items: dict[int, tuple[int, str, int, int]] = {}
        node_number = first_leaf
        while node_number:
            node = read(node_number * node_size, node_size)
            node_number, _, _, _, count, _ = struct.unpack(NODE_DESCRIPTOR, node[:NODE_DESCRIPTOR_SIZE])
            for offset in struct.unpack(f">{count}H", node[-2 * count :]):
                key_length, parent, name_length = struct.unpack(">HIH", node[offset : offset + 8])
                record = node[offset + 2 + key_length :]
                if struct.unpack(">h", record[:2])[0] in (FOLDER_RECORD, FILE_RECORD):
                    name = node[offset + 8 : offset + 8 + 2 * name_length].decode("utf-16-be")
                    (cnid,) = struct.unpack(">I", record[RECORD_ID_OFFSET : RECORD_ID_OFFSET + 4])
                    (mode,) = struct.unpack(">H", record[RECORD_MODE_OFFSET : RECORD_MODE_OFFSET + 2])
                    (flags,) = struct.unpack(">H", record[RECORD_FINDER_FLAGS_OFFSET : RECORD_FINDER_FLAGS_OFFSET + 2])
                    items[cnid] = (parent, name, mode, flags)

    def path(cnid: int) -> str:
        parent, name = items[cnid][:2]
        return "/" if parent == ROOT_PARENT_ID else f"{path(parent).rstrip('/')}/{name}"

    return {path(cnid): CatalogEntry(cnid, mode, flags) for cnid, (_, _, mode, flags) in items.items()}


def volume_paths(root: Path) -> list[Path]:
    """Everything in root, relative to it, sorted so that each folder comes before what it contains."""
    paths = []
    for directory, dirs, files in os.walk(root):
        paths += [Path(directory, name).relative_to(root) for name in dirs + files]
    return sorted(paths, key=lambda path: path.as_posix())


def volume_id(root: Path, paths: list[Path]) -> bytes:
    """8 bytes of the hash of the volume's content, the HFS+ volume id that newfs_hfs makes random."""
    digest = hashlib.sha256()
    for path in paths:
        source = root / path
        digest.update(path.as_posix().encode() + b"\0")
        if source.is_symlink():
            digest.update(os.readlink(source).encode())
        elif source.is_file():
            with source.open("rb") as f:
                digest.update(hashlib.file_digest(f, "sha256").digest())
    return digest.digest()[:8]


def item_blocks(source: Path) -> int:
    if source.is_symlink():
        return 1
    return 0 if source.is_dir() else math.ceil(source.stat().st_size / BLOCK_SIZE)


def volume_blocks(root: Path, paths: list[Path], catalog: BTree) -> int:
    """Blocks for the volume's metadata and paths, with FREE_SPACE_SHARE of them more."""
    data = sum(item_blocks(root / path) for path in paths)
    metadata = HEADER_BLOCKS + math.ceil(data / BLOCK_SIZE / 8) + EXTENTS_TREE.blocks + catalog.blocks + ATTRIBUTES_TREE.blocks
    return metadata + data + math.ceil(data * FREE_SPACE_SHARE) + MIN_FREE_BLOCKS


def ds_record(name: str, code: bytes, kind: bytes, value: bytes) -> bytes:
    encoded = name.encode("utf-16-be")
    if kind == b"blob":
        value = struct.pack(">I", len(value)) + value
    return struct.pack(">I", len(encoded) // 2) + encoded + code + kind + value


# The .DS_Store buddy allocator's blocks, at the offsets and of the sizes that Finder gives a store of one leaf:
# its root block, the B-tree's header block and the leaf, with the rest of its 2 GiB space in its free lists
DS_ROOT_BLOCK = (0x2000, 11)


DS_TREE_BLOCK = (0x40, 5)


DS_LEAF_BLOCK = (0x1000, 12)


DS_HEADER_SIZE = 32


DS_SPACE_BITS = 31


DS_BLOCK_ADDRESSES = 256


DS_PAGE_SIZE = 0x1000


def ds_free_lists() -> list[list[int]]:
    """The free blocks of each size 2^k of the allocator's space, around its header and the three blocks."""
    used = [(0, 5), DS_TREE_BLOCK, DS_LEAF_BLOCK, DS_ROOT_BLOCK]
    free: list[list[int]] = [[] for _ in range(DS_SPACE_BITS + 1)]

    def split(offset: int, bits: int) -> None:
        if (offset, bits) in used:
            return
        if not any(offset <= start < offset + (1 << bits) for start, _ in used):
            free[bits].append(offset)
            return
        split(offset, bits - 1)
        split(offset + (1 << (bits - 1)), bits - 1)

    split(0, DS_SPACE_BITS)
    return free


def ds_store(records: list[tuple[str, bytes, bytes, bytes]]) -> bytes:
    """A .DS_Store of records (file name, code, type, value), as Finder writes one that fits in a single leaf."""
    ordered = sorted(records, key=lambda record: (record[0].lower(), record[1]))
    leaf = struct.pack(">II", 0, len(ordered)) + b"".join(ds_record(*record) for record in ordered)
    if len(leaf) > 1 << DS_LEAF_BLOCK[1]:
        raise BuildError(f".DS_Store records take {len(leaf)} bytes, more than one leaf")
    blocks = [DS_ROOT_BLOCK, DS_TREE_BLOCK, DS_LEAF_BLOCK]
    addresses = [offset | bits for offset, bits in blocks]
    root = struct.pack(f">II{DS_BLOCK_ADDRESSES}I", len(addresses), 0, *addresses, *[0] * (DS_BLOCK_ADDRESSES - len(addresses)))
    root += struct.pack(">IB4sI", 1, 4, b"DSDB", blocks.index(DS_TREE_BLOCK))
    for offsets in ds_free_lists():
        root += struct.pack(f">I{len(offsets)}I", len(offsets), *offsets)
    tree = struct.pack(">5I", blocks.index(DS_LEAF_BLOCK), 0, len(ordered), 1, DS_PAGE_SIZE)
    root_offset, root_bits = DS_ROOT_BLOCK
    space = bytearray(root_offset + (1 << root_bits))
    # the header repeats the root block's offset after its size, then has the leaf's address, as Finder's stores do
    leaf_address = addresses[blocks.index(DS_LEAF_BLOCK)]
    space[:DS_HEADER_SIZE] = struct.pack(">4sIII4I", b"Bud1", root_offset, 1 << root_bits, root_offset, leaf_address, 0, 0, 0)
    for (offset, _), content in zip(blocks, (root, tree, leaf), strict=True):
        space[offset : offset + len(content)] = content
    return struct.pack(">I", 1) + bytes(space)


ALIAS_VERSION = 2


ALIAS_EJECTABLE_DISK = 5


# the volume attributes in the alias records that Finder writes for a disk image's volume
ALIAS_VOLUME_ATTRIBUTES = 0x0D02


ALIAS_NO_LEVEL = 0xFFFF


ALIAS_FOLDER_NAME, ALIAS_CNID_PATH, ALIAS_CARBON_PATH = 0, 1, 2


ALIAS_UNICODE_FILE_NAME, ALIAS_UNICODE_VOLUME_NAME = 14, 15


ALIAS_VOLUME_DATE, ALIAS_FILE_DATE = 16, 17


ALIAS_POSIX_PATH, ALIAS_VOLUME_PATH = 18, 19


ALIAS_END = 0xFFFF


def alias_tag(tag: int, value: bytes) -> bytes:
    return struct.pack(">HH", tag, len(value)) + value + bytes(len(value) % 2)


def background_alias(volume_name: str, folder_id: int, file_id: int) -> bytes:
    """The alias record, version 2, through which Finder finds the background picture of the volume's window."""
    date = hfs_date(SOURCE_DATE_EPOCH)
    pascal_volume = bytes([len(volume_name)]) + volume_name.encode()
    pascal_file = bytes([len(BACKGROUND_FILE)]) + BACKGROUND_FILE.encode()
    fixed = struct.pack(
        ">HH28sI2sHI64sII4s4sHHIH10x",
        ALIAS_VERSION,
        0,
        pascal_volume,
        date,
        b"H+",
        ALIAS_EJECTABLE_DISK,
        folder_id,
        pascal_file,
        file_id,
        date,
        bytes(4),
        bytes(4),
        ALIAS_NO_LEVEL,
        ALIAS_NO_LEVEL,
        ALIAS_VOLUME_ATTRIBUTES,
        0,
    )
    tags = [
        alias_tag(ALIAS_FOLDER_NAME, BACKGROUND_DIR.encode()),
        alias_tag(ALIAS_VOLUME_DATE, struct.pack(">Q", date << 16)),
        alias_tag(ALIAS_FILE_DATE, struct.pack(">Q", date << 16)),
        alias_tag(ALIAS_CNID_PATH, struct.pack(">I", folder_id)),
        alias_tag(ALIAS_CARBON_PATH, f"{volume_name}:{BACKGROUND_DIR}:\0{BACKGROUND_FILE}".encode()),
        alias_tag(ALIAS_UNICODE_FILE_NAME, hfs_name(BACKGROUND_FILE)),
        alias_tag(ALIAS_UNICODE_VOLUME_NAME, hfs_name(volume_name)),
        alias_tag(ALIAS_POSIX_PATH, f"/{BACKGROUND_DIR}/{BACKGROUND_FILE}".encode()),
        alias_tag(ALIAS_VOLUME_PATH, f"/Volumes/{volume_name}".encode()),
        struct.pack(">HH", ALIAS_END, 0),
    ]
    record = fixed + b"".join(tags)
    # the record starts with its creator application and its own size
    return struct.pack(">4sH", bytes(4), 6 + len(record)) + record


def window_store(volume_name: str, app_name: str, folder_id: int, file_id: int) -> bytes:
    """The .DS_Store with which Finder opens the volume's window as jpackage's DMGsetup.scpt sets it up."""
    window = {
        "ContainerShowSidebar": False,
        "ShowSidebar": False,
        "ShowStatusBar": False,
        "ShowTabView": False,
        "ShowToolbar": False,
        "WindowBounds": WINDOW_BOUNDS,
    }
    view = {
        "arrangeBy": "none",
        "backgroundColorBlue": 1.0,
        "backgroundColorGreen": 1.0,
        "backgroundColorRed": 1.0,
        "backgroundImageAlias": background_alias(volume_name, folder_id, file_id),
        "backgroundType": 2,
        "gridOffsetX": 0.0,
        "gridOffsetY": 0.0,
        "gridSpacing": 100.0,
        "iconSize": ICON_SIZE,
        "labelOnBottom": True,
        "showIconPreview": True,
        "showItemInfo": False,
        "textSize": TEXT_SIZE,
        "viewOptionsVersion": 1,
    }

    def location(position: tuple[int, int]) -> bytes:
        return struct.pack(">IIII", *position, 0xFFFFFFFF, 0xFFFF0000)

    return ds_store(
        [
            (".", b"bwsp", b"blob", plistlib.dumps(window, fmt=plistlib.FMT_BINARY)),
            (".", b"icvp", b"blob", plistlib.dumps(view, fmt=plistlib.FMT_BINARY)),
            (".", b"vSrn", b"long", struct.pack(">I", 1)),
            (APPLICATIONS, b"Iloc", b"blob", location(APPLICATIONS_ICON_POSITION)),
            (app_name, b"Iloc", b"blob", location(APP_ICON_POSITION)),
        ]
    )


def jpackage_background(java_home: Path, target: Path) -> None:
    with zipfile.ZipFile(java_home / "jmods/jdk.jpackage.jmod") as jmod:
        target.write_bytes(jmod.read(JPACKAGE_BACKGROUND))


def stage_volume(app: Path, java_home: Path, volume: Path) -> list[Path]:
    """Moves app into the volume's folder with the window's background and icon, the Applications link and the
    .DS_Store, with modes that don't depend on the umask; returns everything in it in the order hfsplus adds it."""
    (volume / BACKGROUND_DIR).mkdir(parents=True)
    jpackage_background(java_home, volume / BACKGROUND_DIR / BACKGROUND_FILE)
    shutil.copy(app / "Contents/Resources" / f"{app.stem}.icns", volume / VOLUME_ICON)
    (volume / APPLICATIONS).symlink_to("/Applications")
    shutil.move(app, volume / app.name)
    (volume / DS_STORE).touch()
    normalize_permissions(volume)
    paths = volume_paths(volume)
    # hfsplus numbers what it adds in order from FIRST_USER_ID
    folder_id, file_id = (FIRST_USER_ID + paths.index(p) for p in (Path(BACKGROUND_DIR), Path(BACKGROUND_DIR, BACKGROUND_FILE)))
    (volume / DS_STORE).write_bytes(window_store(VOLUME_NAME, app.name, folder_id, file_id))
    return paths


def item_mode(source: Path) -> int:
    """The mode of source in the volume: its own, or the one hfsplus gives symlinks."""
    mode = source.lstat().st_mode
    return stat.S_IFLNK | HFSPLUS_FILE_MODE if stat.S_ISLNK(mode) else mode


def check_volume(image: Path, root: Path, paths: list[Path]) -> None:
    """Fails unless image has paths with the modes of their sources, numbered in their order from FIRST_USER_ID, and
    its header counts them; hfsplus exits with 0 when it fails to add or change an item."""
    folders = sum(1 for path in paths if (root / path).is_dir() and not (root / path).is_symlink())
    expected = VolumeCounts(len(paths) - folders, folders, FIRST_USER_ID + len(paths))
    counted = volume_counts(image)
    if counted != expected:
        raise BuildError(f"{image.name} counts {counted}, not {expected}")
    entries = catalog_entries(image)
    for number, path in enumerate(paths, FIRST_USER_ID):
        target = f"/{path.as_posix()}"
        expected_entry = (number, item_mode(root / path))
        entry = entries.get(target)
        if entry is None or (entry.cnid, entry.mode) != expected_entry:
            raise BuildError(f"{target} in {image.name} is {entry}, not number {number} with mode {expected_entry[1]:o}")


def add_to_volume(hfsplus: Path, image: Path, root: Path, paths: list[Path]) -> None:
    """Adds paths, relative to root, to image in their order, with their modes, then checks the volume."""
    for path in paths:
        source = root / path
        target = f"/{path.as_posix()}"
        if not target.isascii():
            raise BuildError(f"hfsplus can't add {target}: its name isn't ASCII")
        if source.is_symlink():
            run([hfsplus, image, "symlink", target, os.readlink(source)], capture=True)
            continue
        if source.is_dir():
            run([hfsplus, image, "mkdir", target], capture=True)
            default = HFSPLUS_FOLDER_MODE
        else:
            run([hfsplus, image, "add", source, target], capture=True)
            default = HFSPLUS_FILE_MODE
        mode = stat.S_IMODE(source.stat().st_mode)
        if mode != default:
            run([hfsplus, image, "chmod", f"{mode:o}", target], capture=True)
    check_volume(image, root, paths)


def set_custom_icon(hfsplus: Path, image: Path) -> None:
    """Has Finder show .VolumeIcon.icns as the volume's icon, which it does when the root folder has a custom icon."""
    run([hfsplus, image, "attr", "/", "C"], capture=True)
    if not catalog_entries(image)["/"].finder_flags & HAS_CUSTOM_ICON:
        raise BuildError(f"hfsplus did not mark the root folder of {image.name} as having a custom icon")


def build_dmg(tools: DmgTools, app: Path, java_home: Path, work: Path, target: Path) -> None:
    """A compressed dmg of app with the layout of jpackage's dmgs, with SOURCE_DATE_EPOCH's dates."""
    volume = work / "volume"
    paths = stage_volume(app, java_home, volume)
    catalog = catalog_tree(len(paths))
    image = work / "volume.hfs"
    format_hfs(image, VOLUME_NAME, volume_blocks(volume, paths, catalog), catalog, volume_id(volume, paths))
    add_to_volume(tools.hfsplus, image, volume, paths)
    set_custom_icon(tools.hfsplus, image)
    run([tools.dmg, *DMG_COMPRESSION, "build", image, target])


# The koly block that ends a dmg, and where in it codesign records the offset and length of the signature it puts
# before the block, which holds the notarization ticket that stapler adds too
KOLY_SIZE = 512


KOLY_CODE_SIGNATURE = 0x128


KOLY_CODE_SIGNATURE_FIELDS = ">QQ"


def dmg_signature(dmg: bytes) -> tuple[int, int]:
    """The offset and length of the dmg's signature, a length of 0 when it has none."""
    offset, length = struct.unpack_from(KOLY_CODE_SIGNATURE_FIELDS, dmg, len(dmg) - KOLY_SIZE + KOLY_CODE_SIGNATURE)
    return int(offset), int(length)


def copy_dmg_signature(released: bytes, built: bytes) -> bytes:
    """built with released's signature before its koly block, as codesign puts it, and the koly fields that locate it."""
    offset, length = dmg_signature(released)
    if not length:
        return built
    koly = bytearray(built[-KOLY_SIZE:])
    fields = slice(KOLY_CODE_SIGNATURE, KOLY_CODE_SIGNATURE + struct.calcsize(KOLY_CODE_SIGNATURE_FIELDS))
    koly[fields] = released[len(released) - KOLY_SIZE :][fields]
    return built[:-KOLY_SIZE] + released[offset : offset + length] + bytes(koly)
