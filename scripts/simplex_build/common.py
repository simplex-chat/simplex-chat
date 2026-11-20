"""Logging, commands, files, downloads and git, which every build uses."""

import contextlib
import enum
import hashlib
import os
import shutil
import stat
import subprocess
import sys
import urllib.request
import zipfile
from collections.abc import Callable, Iterator, Mapping, Sequence
from pathlib import Path

SIMPLEX_REPO = "simplex-chat/simplex-chat"


REPO = f"https://github.com/{SIMPLEX_REPO}"


GHC = "9.6.3"


class BuildError(Exception):
    pass


class Platform(enum.StrEnum):
    LINUX = "linux"
    MACOS = "macos"
    WINDOWS = "windows"


def host_platform() -> Platform:
    if os.name == "nt":
        return Platform.WINDOWS
    return Platform.MACOS if sys.platform == "darwin" else Platform.LINUX


def require_platform(required: Platform) -> None:
    if host_platform() != required:
        raise BuildError(f"this build runs on {required}, not on {host_platform()}")


def log(message: str) -> None:
    print(f"\033[36m{message}\033[0m", flush=True)


def on_github() -> bool:
    return os.environ.get("GITHUB_ACTIONS") == "true"


def github_escape(message: str) -> str:
    # https://github.com/actions/toolkit/blob/main/packages/core/src/command.ts
    return message.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def warn(message: str) -> None:
    """An annotation on the run's page on GitHub Actions, yellow text elsewhere."""
    if on_github():
        print(f"::warning::{github_escape(message)}", flush=True)
    else:
        print(f"\033[33mwarning: {message}\033[0m", file=sys.stderr, flush=True)


def error(message: str) -> None:
    if on_github():
        print(f"::error::{github_escape(message)}", flush=True)
    else:
        print(f"\033[31merror: {message}\033[0m", file=sys.stderr, flush=True)


@contextlib.contextmanager
def log_group(title: str) -> Iterator[None]:
    """A collapsible section of the log on GitHub Actions."""
    if not on_github():
        log(f"$ {title}")
        yield
        return
    print(f"::group::{github_escape(title)}", flush=True)
    try:
        yield
    finally:
        print("::endgroup::", flush=True)


def add_summary(markdown: str) -> None:
    """Appended to the job's summary page on GitHub Actions."""
    path = os.environ.get("GITHUB_STEP_SUMMARY")
    if on_github() and path:
        with open(path, "a") as f:
            f.write(markdown)


def require_tools(tools: Sequence[str]) -> None:
    missing = [tool for tool in tools if shutil.which(tool) is None]
    if missing:
        raise BuildError(f"not found on PATH, please install: {', '.join(missing)}")
    if "docker" in tools and subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
        raise BuildError("docker is installed but its daemon is not reachable: start it, or check your user's access")


def run(
    args: Sequence[str | Path],
    cwd: Path | None = None,
    env: Mapping[str, str] | None = None,
    input: str | None = None,
    check: bool = True,
    capture: bool = False,
    title: str | None = None,
) -> subprocess.CompletedProcess[str]:
    """Commands with output to show run in their own log group, titled with the command unless title is given."""
    with contextlib.nullcontext() if capture else log_group(title or " ".join(map(str, args))):
        result = subprocess.run(
            [str(a) for a in args],
            cwd=cwd,
            env={**os.environ, **env} if env else None,
            input=input,
            check=False,
            text=True,
            stdout=subprocess.PIPE if capture else None,
        )
    if check and result.returncode != 0:
        raise BuildError(f"failed ({result.returncode}): {' '.join(map(str, args))}")
    return result


def sha256(path: Path) -> str:
    with open(path, "rb") as f:
        return hashlib.file_digest(f, "sha256").hexdigest()


def write_sha256sums(directory: Path, target: Path | None = None) -> None:
    files = sorted(p for p in directory.iterdir() if p.is_file() and p.name not in ("SHA256SUMS", "_sha256sums"))
    hashes = {p.name: sha256(p) for p in files}
    lines = "".join(f"{digest}  {name}\n" for name, digest in hashes.items())
    (target or directory / "SHA256SUMS").write_text(lines)
    print(lines, end="")
    rows = "".join(f"| `{p.name}` | {p.stat().st_size:,} | `{hashes[p.name]}` |\n" for p in files)
    add_summary(f"| File | Bytes | SHA-256 |\n|---|--:|---|\n{rows}\n")


def compare_dirs(built: Path, released: Path) -> list[str]:
    """The names of the released files that differ from the built ones or are not built; a build also makes files
    that releases don't publish."""
    mismatched = []
    for path in sorted(released.iterdir()):
        other = built / path.name
        if not other.is_file() or sha256(path) != sha256(other):
            mismatched.append(path.name)
    return mismatched


def normalize_permissions(root: Path) -> None:
    """Directories and executable files 755, other files 644, so that checkouts on any machine build the same."""
    for directory, dirs, files in os.walk(root):
        for name in dirs:
            path = Path(directory, name)
            if not path.is_symlink():
                path.chmod(0o755)
        for name in files:
            path = Path(directory, name)
            if not path.is_symlink():
                path.chmod(0o755 if path.stat().st_mode & 0o111 else 0o644)


def remove_tree(path: Path) -> None:
    """A file or directory tree, with its read-only files, which cabal's git checkouts have."""

    def make_writable(function: Callable[[str], object], name: str, error: BaseException) -> None:
        # Windows refuses to delete read-only files, POSIX to delete entries of read-only directories
        os.chmod(os.path.dirname(name), stat.S_IRWXU)
        os.chmod(name, stat.S_IRWXU)
        function(name)

    if path.is_dir() and not path.is_symlink():
        shutil.rmtree(path, onexc=make_writable)
    elif path.exists() or path.is_symlink():
        path.unlink()


def single_file(directory: Path, pattern: str) -> Path:
    found = sorted(directory.glob(pattern))
    if len(found) != 1:
        raise BuildError(f"expected one {pattern} in {directory}, found {len(found)}")
    return found[0]


def download(url: str, target: Path) -> None:
    log(f"downloading {url}")
    try:
        with urllib.request.urlopen(url) as response, open(target, "wb") as f:
            shutil.copyfileobj(response, f)
    except OSError as e:
        raise BuildError(f"cannot download {url}: {e}") from e


def fetch_pinned(url: str, digest: str, target: Path) -> Path:
    download(url, target)
    if sha256(target) != digest:
        raise BuildError(f"{url} doesn't match its pinned SHA-256 {digest}")
    return target


def release_url(tag: str, name: str, repo: str = SIMPLEX_REPO) -> str:
    return f"https://github.com/{repo}/releases/download/{tag}/{name}"


def download_release(tag: str, repo: str, names: Sequence[str], target: Path) -> None:
    target.mkdir(parents=True, exist_ok=True)
    for name in names:
        download(release_url(tag, name, repo), target / name)


def byte_differences(released: Path, built: Path) -> list[str]:
    return [] if sha256(released) == sha256(built) else ["sha256 doesn't match"]


def compare_release(names: Sequence[str], out: Path, differences: Callable[[Path, Path], list[str]]) -> list[str]:
    """The names of the released files in out/prebuilt that differ from the built ones in out/from-source other than
    as differences allows; when none does, out/_sha256sums lists the released files."""
    released, built = out / "prebuilt", out / "from-source"
    failed = []
    for name in names:
        found = differences(released / name, built / name) if (built / name).is_file() else ["it is not built"]
        for difference in found:
            log(f"{name}: {difference}")
        log(f"{name} is {'NOT ' if found else ''}reproduced")
        if found:
            failed.append(name)
    if not failed:
        write_sha256sums(released, out / "_sha256sums")
    return failed


def tag_version(tag: str) -> str:
    """v7.1.0-beta.7 -> 7.1.0, as build.yml versions the desktop CLI deb: without the tag's last -suffix"""
    return tag.lstrip("v").rsplit("-", 1)[0]


def clone(workdir: Path, repo: str = SIMPLEX_REPO) -> Path:
    # the files as they are committed: Git for Windows converts their line endings to CRLF by default
    run(["git", "-C", workdir, "clone", "-c", "core.autocrlf=false", f"https://github.com/{repo}.git", "simplex-chat"])
    return workdir / "simplex-chat"


def checkout(source: Path, tag: str) -> None:
    run(["git", "-C", source, "reset", "--hard", "--quiet"])
    run(["git", "-C", source, "clean", "-dfxq"])
    run(["git", "-C", source, "checkout", "--quiet", tag])
    run(["git", "-C", source, "submodule", "update", "--init", "--recursive", "--quiet"])


def extract_flat(package: Path, target: Path, prefix: str = "") -> None:
    """The files of a zip under prefix, in target without their directories."""
    target.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(package) as z:
        for member in z.infolist():
            if not member.is_dir() and member.filename.startswith(prefix):
                (target / Path(member.filename).name).write_bytes(z.read(member))


def single_dir(directory: Path, pattern: str) -> Path:
    found = [p for p in sorted(directory.glob(pattern)) if p.is_dir()]
    if len(found) != 1:
        raise BuildError(f"expected one {pattern} directory in {directory}, found {len(found)}")
    return found[0]


def touch_tree(root: Path, epoch: str) -> None:
    """Sets the times of root and everything in it, through symlinks as touch does, for deterministic archives."""
    paths = [root]
    for directory, dirs, files in os.walk(root):
        paths += [Path(directory, name) for name in dirs + files]
    for path in paths:
        if path.exists():
            os.utime(path, (int(epoch), int(epoch)))


def edit_lines(path: Path, edit: Callable[[str], str | None]) -> None:
    """Rewrites each line of path, without its newline, with edit; a line that edit maps to None is removed."""
    lines = []
    for line in path.read_text().splitlines(keepends=True):
        text = line.rstrip("\n")
        edited = edit(text)
        if edited is not None:
            lines.append(edited + line[len(text) :])
    path.write_text("".join(lines))
