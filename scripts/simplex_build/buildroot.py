"""The fixed directory that the macOS, iOS and Windows release builds run in, so that their files don't depend on the
machine's paths, and the check that the files contain none of those paths."""

import contextlib
import os
import shutil
import zipfile
from collections.abc import Iterator, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path

from .common import BuildError, normalize_permissions, remove_tree, warn

# the umask of GitHub's runners
BUILD_UMASK = 0o022


# what earlier builds left in the checkout, which the release build makes again: copied, they would only take space
CHECKOUT_BUILD_DIRS = ("dist-newstyle", "dist-openssl")


@dataclass(frozen=True, slots=True)
class BuildRoot:
    """The copy of the checkout that a release builds in, an empty directory for the rest of its build, and its
    temporary directory."""

    source: Path
    work: Path
    tmp: Path

    @staticmethod
    def at(directory: Path) -> "BuildRoot":
        return BuildRoot(directory / "simplex-chat", directory / "work", directory / "tmp")

    @property
    def directory(self) -> Path:
        return self.source.parent


def clear_build_root(directory: Path) -> None:
    try:
        remove_tree(directory)
    except OSError as e:
        raise BuildError(f"cannot remove {directory}, where the release builds run: {e}") from e


def checkout_build_dirs(source: Path, directory: str, names: list[str]) -> list[str]:
    return [name for name in names if name in CHECKOUT_BUILD_DIRS] if Path(directory) == source else []


@contextlib.contextmanager
def fixed_build_root(source: Path, root: BuildRoot, env: Mapping[str, str]) -> Iterator[BuildRoot]:
    """source copied into root, with the modes of normalize_permissions, and BUILD_UMASK and the variables of env set
    while it is used; root's directory is removed before and after."""
    if source.is_relative_to(root.directory):
        raise BuildError(f"{source} is in {root.directory}, which the build replaces: build from a checkout elsewhere")
    clear_build_root(root.directory)
    root.work.mkdir(parents=True)
    root.tmp.mkdir()
    shutil.copytree(source, root.source, symlinks=True, ignore=lambda directory, names: checkout_build_dirs(source, directory, names))
    normalize_permissions(root.source)
    saved = {name: os.environ.get(name) for name in env}
    umask = os.umask(BUILD_UMASK)
    os.environ.update(env)
    built = False
    try:
        yield root
        built = True
    finally:
        os.umask(umask)
        for name, value in saved.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value
        try:
            clear_build_root(root.directory)
        except BuildError as e:
            # the error of a failed build says more than the cleanup's
            if built:
                raise
            warn(str(e))


# fewer parts would match unrelated paths, as / would match every path
MIN_MACHINE_PATH_PARTS = 3


def find_machine_paths(
    roots: Sequence[Path], paths: Sequence[str], skip: Sequence[Path] = (), encodings: Sequence[str] = ("utf-8",)
) -> list[str]:
    """'<file>: <path>' for each of paths, in each of encodings, in each file of roots, and in each entry of their jars,
    except in skip."""
    encoded = [(path, path.encode(encoding)) for path in paths for encoding in encodings]
    files = []
    for root in roots:
        files += [root] if root.is_file() else sorted(p for p in root.rglob("*") if p.is_file() and not p.is_symlink())
    files = [file for file in files if not any(file.is_relative_to(directory) for directory in skip)]
    found: list[str] = []
    for file in files:
        contents = [(str(file), file.read_bytes())]
        if file.suffix == ".jar":
            with zipfile.ZipFile(file) as jar:
                contents += [(f"{file}!{name}", jar.read(name)) for name in jar.namelist()]
        found += dict.fromkeys(f"{name}: {path}" for name, data in contents for path, raw in encoded if raw in data)
    return found


def check_machine_paths(found: Sequence[str]) -> None:
    if found:
        raise BuildError("these files contain paths of the machine that built them:\n" + "\n".join(found))
