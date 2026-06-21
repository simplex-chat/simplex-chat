# SimpleX Chat Python library — implementation plan

> **For agentic workers:** Use superpowers-extended-cc:subagent-driven-development (if subagents available) or superpowers-extended-cc:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `simplex-chat` on PyPI — a Python 3 client library for SimpleX bots with the same capability as the existing Node.js library.

**Architecture:** Two repositories of work in this monorepo: extend the Haskell type generator (`bots/src/API/Docs/`) to emit Python types alongside TypeScript, and add a new Python package (`packages/simplex-chat-python/`) that wraps the existing prebuilt `libsimplex.{so,dylib,dll}` via ctypes. Lazy download of libs from `simplex-chat/simplex-chat-libs` GitHub releases. Async-only public API with decorator-registered handlers. Single PyPI wheel.

**Tech Stack:** Haskell (existing codegen), Python 3.11+ (ctypes, asyncio, hatchling), GitHub Actions (publish-python job in existing build.yml).

**Spec:** [`plans/2026-05-07-simplex-chat-python-design.md`](./2026-05-07-simplex-chat-python-design.md)

---

## Plan structure

Eight phases, executed in order:

1. Type generation (Haskell) — `Generate/Python.hs` + `tests/APIDocs.hs` wiring.
2. Python package scaffold — `pyproject.toml`, `_version.py`, layout, hatch config.
3. Native FFI layer — `_native.py` (lazy download, ctypes, hs_init, buffer ownership).
4. Core wrapper — `core.py` (typed async FFI).
5. ChatApi — escape-hatch class with raw + ~40 high-level methods.
6. Bot class — decorators, filters, Message wrapper, lifecycle, middleware.
7. Tests + CLI — pytest suite, `python -m simplex_chat install`.
8. CI publishing — append `publish-python` job to `.github/workflows/build.yml`.

Each phase is a chunk. Phase boundaries are natural review points.

---

## Chunk 1: Type generation (Haskell)

Add `bots/src/API/Docs/Generate/Python.hs`, mirror `Generate/TypeScript.hs`, wire into the test suite.

### Task 1.1: Create `Generate/Python.hs` skeleton

**Files:**
- Create: `bots/src/API/Docs/Generate/Python.hs`

- [ ] **Step 1: Copy `Generate/TypeScript.hs` as starting point**

```bash
cp bots/src/API/Docs/Generate/TypeScript.hs bots/src/API/Docs/Generate/Python.hs
```

- [ ] **Step 2: Rename module and update output paths**

Edit the new file:
- Module: `module API.Docs.Generate.Python where`
- File constants:
  ```haskell
  commandsCodeFile  = "./packages/simplex-chat-python/src/simplex_chat/types/_commands.py"
  responsesCodeFile = "./packages/simplex-chat-python/src/simplex_chat/types/_responses.py"
  eventsCodeFile    = "./packages/simplex-chat-python/src/simplex_chat/types/_events.py"
  typesCodeFile     = "./packages/simplex-chat-python/src/simplex_chat/types/_types.py"
  ```

- [ ] **Step 3: Register module in cabal manifest**

In `simplex-chat.cabal`, find the `other-modules:` list of the `simplex-chat-test` test-suite stanza (`type: exitcode-stdio-1.0`, `main-is: Test.hs`). Insert `API.Docs.Generate.Python` alphabetically between `API.Docs.Generate` and `API.Docs.Responses`.

- [ ] **Step 4: Verify cabal compiles**

```
cabal build simplex-chat-test
```
Expected: builds without error (the file is still TS-shaped but Haskell-valid).

- [ ] **Step 5: Commit scaffolding**

```bash
git add bots/src/API/Docs/Generate/Python.hs simplex-chat.cabal
git commit -m "feat(bots): add Python codegen module skeleton"
```

### Task 1.2: Implement Python type rendering

Replace TypeScript-specific output with Python equivalents per the spec's [type-mapping rules](./2026-05-07-simplex-chat-python-design.md#type-representation).

**Files:**
- Modify: `bots/src/API/Docs/Generate/Python.hs`

- [ ] **Step 1: Rewrite `typesCodeText`**

Output structure:
```python
# API Types
# This file is generated automatically.
from typing import Literal, NotRequired, TypedDict

# … one class / type alias per chatTypesDocs entry …
```

Translation rules (mirror `TypeScript.hs` `typeCode`):
- `ATDRecord fields` → `class <name>(TypedDict):` with translated field types.
- `ATDEnum cs` → `<name> = Literal["c1", "c2", …]`.
- `ATDUnion cs` → tagged TypedDicts + union alias + tag alias (see `unionTypeCode` in TS).

Field-type translation (table in spec; mirrors `TypeScript.hs` `fieldsCode` `typeText`):
- `ATPrim` primitives → Python primitives (`bool`, `str`, `int`, `float`, `dict[str, object]`).
- `ATOptional t` inside TypedDict → `NotRequired[<t>]`; elsewhere `<t> | None`.
- `ATArray {elemType, nonEmpty}` → `list[<elem>]`, append `# non-empty` comment when `nonEmpty=True`.
- `ATMap (PT k) v` → `dict[<k>, <v>]`.
- `ATDef` / `ATRef` → forward-string reference `"<name>"`.

- [ ] **Step 2: Rewrite `responsesCodeText` and `eventsCodeText`**

Both produce union types — same shape as TS's `unionTypeCode`. Output structure:

```python
# API Responses
# This file is generated automatically.
from . import _types as T

ChatResponse_<Tag1> = TypedDict(...)
ChatResponse_<Tag2> = TypedDict(...)
…
ChatResponse = ChatResponse_<Tag1> | ChatResponse_<Tag2> | …
ChatResponse_Tag = Literal["<tag1>", "<tag2>", …]
```

- [ ] **Step 3: Rewrite `commandsCodeText`**

Each command becomes a TypedDict + a `<Type>_cmd_string(self) -> str` function + a Response alias. Function body comes from `pySyntaxText` in `Syntax.hs:160` (no changes to that function — it's already correct and used today by Markdown docs).

```python
# API Commands
# This file is generated automatically.
import json
from typing import TypedDict
from . import _types as T
from . import _responses as CR

class APICreateMyAddress(TypedDict):
    userId: int

def APICreateMyAddress_cmd_string(self: APICreateMyAddress) -> str:
    return '/_address ' + str(self['userId'])

APICreateMyAddress_Response = CR.UserContactLinkCreated | CR.ChatCmdError
```

The `cmdString` body: invoke `pySyntaxText (constrName, params) syntax` analogously to TS's `funcCode`.

- [ ] **Step 4: Build to verify Haskell compiles**

```
cabal build simplex-chat-test
```
Expected: clean build.

- [ ] **Step 5: Commit**

```bash
git add bots/src/API/Docs/Generate/Python.hs
git commit -m "feat(bots): implement Python type generation"
```

### Task 1.3: Wire generators into `tests/APIDocs.hs`

**Files:**
- Modify: `tests/APIDocs.hs`

- [ ] **Step 1: Add import**

Insert after line 11 (`import qualified API.Docs.Generate.TypeScript as TS`):

```haskell
import qualified API.Docs.Generate.Python as Py
```

- [ ] **Step 2: Add four `testGenerate` calls**

Inside `apiDocsTest`, after the existing `describe "TypeScript"` block (line 40-44), add:

```haskell
describe "Python" $ do
  it "generate python commands code"  $ testGenerate Py.commandsCodeFile  Py.commandsCodeText
  it "generate python responses code" $ testGenerate Py.responsesCodeFile Py.responsesCodeText
  it "generate python events code"    $ testGenerate Py.eventsCodeFile    Py.eventsCodeText
  it "generate python types code"     $ testGenerate Py.typesCodeFile     Py.typesCodeText
```

- [ ] **Step 3: Create empty target directory**

```bash
mkdir -p packages/simplex-chat-python/src/simplex_chat/types
```

- [ ] **Step 4: Run the API docs tests — they will write the four files**

```
cabal test simplex-chat-test --test-options="--match \"API\""
```

First run: tests fail because the on-disk files are empty / missing. The `testGenerate` mechanism overwrites the file with generated content, so the second run passes.

```
cabal test simplex-chat-test --test-options="--match \"Python\""
```

Expected: PASS on the second run.

- [ ] **Step 5: Sanity-check generated output**

Eyeball each of the four generated files:

```bash
head -50 packages/simplex-chat-python/src/simplex_chat/types/_types.py
head -30 packages/simplex-chat-python/src/simplex_chat/types/_commands.py
head -30 packages/simplex-chat-python/src/simplex_chat/types/_responses.py
head -30 packages/simplex-chat-python/src/simplex_chat/types/_events.py
```

Verify: starts with `# This file is generated automatically.`, contains valid-looking Python, no obvious junk like `<RECURSIVE>` markers.

- [ ] **Step 6: Run them through Python parser to verify syntax**

```
python -c "import ast; [ast.parse(open(f).read()) for f in ['packages/simplex-chat-python/src/simplex_chat/types/_types.py', 'packages/simplex-chat-python/src/simplex_chat/types/_commands.py', 'packages/simplex-chat-python/src/simplex_chat/types/_responses.py', 'packages/simplex-chat-python/src/simplex_chat/types/_events.py']]"
```

Expected: no exception. Any `SyntaxError` indicates a generator bug — fix in `Generate/Python.hs` and re-run cabal test.

- [ ] **Step 7: Commit generated artifacts**

```bash
git add tests/APIDocs.hs packages/simplex-chat-python/src/simplex_chat/types/
git commit -m "feat(bots): wire Python generators into APIDocs test suite"
```

### Task 1.4: Add `types/__init__.py` re-exporting namespaces

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/types/__init__.py`

- [ ] **Step 1: Write namespace re-exports**

```python
from . import _types as T
from . import _commands as CC
from . import _responses as CR
from . import _events as CEvt

__all__ = ["T", "CC", "CR", "CEvt"]
```

- [ ] **Step 2: Verify import works**

```
python -c "from simplex_chat.types import T, CC, CR, CEvt; print(T, CC, CR, CEvt)"
```

(Run from `packages/simplex-chat-python/src/`.)

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/types/__init__.py
git commit -m "feat(python): add types namespace re-exports"
```

---

## Chunk 2: Python package scaffold

Set up the package skeleton — `pyproject.toml`, version pinning, top-level `__init__.py`, AGPL license, README placeholder.

### Task 2.1: Create `pyproject.toml`

**Files:**
- Create: `packages/simplex-chat-python/pyproject.toml`

- [ ] **Step 1: Write hatchling build config**

```toml
[build-system]
requires = ["hatchling>=1.24"]
build-backend = "hatchling.build"

[project]
name = "simplex-chat"
description = "SimpleX Chat Python library for chat bots"
readme = "README.md"
license = "AGPL-3.0-only"
authors = [{name = "SimpleX Chat"}]
requires-python = ">=3.11"
keywords = ["simplex", "messenger", "chat", "privacy", "security", "bots"]
classifiers = [
    "Development Status :: 4 - Beta",
    "License :: OSI Approved :: GNU Affero General Public License v3",
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.11",
    "Programming Language :: Python :: 3.12",
    "Programming Language :: Python :: 3.13",
    "Topic :: Communications :: Chat",
]
dynamic = ["version"]

[project.urls]
Homepage = "https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-python"
Issues = "https://github.com/simplex-chat/simplex-chat/issues"

[project.optional-dependencies]
test = ["pytest>=8", "pytest-asyncio>=0.23"]
dev  = ["pytest>=8", "pytest-asyncio>=0.23", "pyright>=1.1.380", "ruff>=0.6"]

[tool.hatch.version]
path = "src/simplex_chat/_version.py"

[tool.hatch.build.targets.wheel]
packages = ["src/simplex_chat"]

[tool.pytest.ini_options]
asyncio_mode = "auto"
```

- [ ] **Step 2: Commit**

```bash
git add packages/simplex-chat-python/pyproject.toml
git commit -m "feat(python): add pyproject.toml with hatchling backend"
```

### Task 2.2: Create `_version.py`

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/_version.py`

- [ ] **Step 1: Write version constants**

```python
"""Single source of truth for both the Python package version and the
simplex-chat-libs release tag we depend on.

Bump both together for normal releases. For wrapper-only fixes use a PEP 440
post-release: __version__ = "6.5.1.post1", LIBS_VERSION unchanged.
"""

__version__ = "6.5.1"        # PEP 440 — read by hatchling for wheel metadata
LIBS_VERSION = "6.5.1"       # simplex-chat-libs release tag (no 'v' prefix)
```

- [ ] **Step 2: Verify hatchling can read the version**

```
cd packages/simplex-chat-python && python -m build --wheel
```

Expected: produces `dist/simplex_chat-6.5.1-py3-none-any.whl`. (Wheel will be incomplete — only the types module is in src/ at this point — but build should succeed.)

Clean up: `rm -rf packages/simplex-chat-python/dist packages/simplex-chat-python/src/simplex_chat.egg-info`

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/_version.py
git commit -m "feat(python): add _version.py with package + libs version pinning"
```

### Task 2.3: Add `py.typed` marker, README placeholder, AGPL license

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/py.typed`
- Create: `packages/simplex-chat-python/README.md`
- Create: `packages/simplex-chat-python/LICENSE`

- [ ] **Step 1: Touch the empty `py.typed` marker**

```bash
touch packages/simplex-chat-python/src/simplex_chat/py.typed
```

- [ ] **Step 2: Copy AGPL license from a sibling package**

```bash
cp packages/simplex-chat-nodejs/LICENSE packages/simplex-chat-python/LICENSE
```

- [ ] **Step 3: Write a minimal README**

```markdown
# SimpleX Chat Python library

Python 3 client library for [SimpleX Chat](https://simplex.chat) bots.

Equivalent to the [Node.js library](https://www.npmjs.com/package/simplex-chat).

## Installation

    pip install simplex-chat

Requires Python 3.11+.

## Quick start

[example to be added]

## License

[AGPL-3.0](./LICENSE)
```

- [ ] **Step 4: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/py.typed \
        packages/simplex-chat-python/README.md \
        packages/simplex-chat-python/LICENSE
git commit -m "feat(python): add py.typed marker, README, AGPL license"
```

### Task 2.4: Top-level `__init__.py` with empty exports

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/__init__.py`

- [ ] **Step 1: Write a placeholder that re-exports from submodules as they appear**

```python
"""SimpleX Chat — Python client library for chat bots."""

from ._version import __version__

__all__ = ["__version__"]
```

(Will be expanded as `Bot`, `ChatApi`, etc. land in later phases.)

- [ ] **Step 2: Verify import**

```
python -c "import simplex_chat; print(simplex_chat.__version__)"
```

Run from `packages/simplex-chat-python/src/`.

Expected: prints `6.5.1`.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/__init__.py
git commit -m "feat(python): add top-level package __init__.py"
```

---

## Chunk 3: Native FFI layer (`_native.py`)

Lazy lib download, platform detection, ctypes signatures, `hs_init_with_rtsopts`, atomic install, buffer ownership. Single most-error-prone piece of the package — give it tests.

### Task 3.1: `_native.py` — platform detection + URL building

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/_native.py`
- Create: `packages/simplex-chat-python/tests/test_native_url.py`

- [ ] **Step 1: Write platform-detection tests**

```python
# tests/test_native_url.py
from unittest.mock import patch
import pytest
from simplex_chat._native import _platform_tag, _libs_url, _libname

@patch("sys.platform", "linux")
@patch("platform.machine", return_value="x86_64")
def test_platform_linux_x64(_):
    assert _platform_tag() == "linux-x86_64"

@patch("sys.platform", "darwin")
@patch("platform.machine", return_value="arm64")
def test_platform_macos_arm64(_):
    assert _platform_tag() == "macos-aarch64"

@patch("sys.platform", "win32")
@patch("platform.machine", return_value="AMD64")
def test_platform_windows_x64(_):
    assert _platform_tag() == "windows-x86_64"

@patch("sys.platform", "freebsd")
@patch("platform.machine", return_value="x86_64")
def test_platform_unsupported(_):
    with pytest.raises(RuntimeError, match="Unsupported"):
        _platform_tag()

def test_libname_per_platform():
    with patch("sys.platform", "linux"):
        assert _libname() == "libsimplex.so"
    with patch("sys.platform", "darwin"):
        assert _libname() == "libsimplex.dylib"
    with patch("sys.platform", "win32"):
        assert _libname() == "libsimplex.dll"

@patch("simplex_chat._native._platform_tag", return_value="linux-x86_64")
def test_url_sqlite(_):
    assert _libs_url("sqlite") == \
        "https://github.com/simplex-chat/simplex-chat-libs/releases/download/" \
        "v6.5.1/simplex-chat-libs-linux-x86_64.zip"

@patch("simplex_chat._native._platform_tag", return_value="linux-x86_64")
def test_url_postgres(_):
    assert _libs_url("postgres") == \
        "https://github.com/simplex-chat/simplex-chat-libs/releases/download/" \
        "v6.5.1/simplex-chat-libs-linux-x86_64-postgres.zip"
```

- [ ] **Step 2: Write `_native.py` skeleton with platform + URL helpers**

```python
"""Native libsimplex loader: platform detection, lazy download, ctypes setup.

Internal — users interact with `Bot` / `ChatApi`, never with this module.
"""
from __future__ import annotations

import ctypes
import errno
import os
import platform
import sys
import tempfile
import threading
import urllib.request
import zipfile
from ctypes import POINTER, c_char_p, c_int, c_uint8, c_void_p
from pathlib import Path
from typing import Literal

from ._version import LIBS_VERSION

Backend = Literal["sqlite", "postgres"]

_GITHUB_REPO = "simplex-chat/simplex-chat-libs"

_PLATFORM_MAP = {
    "linux":  ("linux",   {"x86_64": "x86_64", "aarch64": "aarch64"}),
    "darwin": ("macos",   {"x86_64": "x86_64", "arm64":   "aarch64"}),
    "win32":  ("windows", {"AMD64":  "x86_64", "x86_64":  "x86_64"}),
}

_LIBNAME = {"linux": "libsimplex.so", "darwin": "libsimplex.dylib", "win32": "libsimplex.dll"}

SUPPORTED = (
    "linux-x86_64", "linux-aarch64",
    "macos-x86_64", "macos-aarch64",
    "windows-x86_64",
)


def _platform_tag() -> str:
    info = _PLATFORM_MAP.get(sys.platform)
    if not info:
        raise RuntimeError(f"Unsupported platform: {sys.platform}")
    sysname, archs = info
    arch = archs.get(platform.machine())
    if not arch:
        raise RuntimeError(f"Unsupported architecture: {sys.platform}/{platform.machine()}")
    tag = f"{sysname}-{arch}"
    if tag not in SUPPORTED:
        raise RuntimeError(f"Unsupported combination: {tag}; supported: {SUPPORTED}")
    return tag


def _libname() -> str:
    return _LIBNAME[sys.platform]


def _libs_url(backend: Backend) -> str:
    suffix = "-postgres" if backend == "postgres" else ""
    return (
        f"https://github.com/{_GITHUB_REPO}/releases/download/"
        f"v{LIBS_VERSION}/simplex-chat-libs-{_platform_tag()}{suffix}.zip"
    )
```

- [ ] **Step 3: Run tests**

```
cd packages/simplex-chat-python && pip install -e . && pip install pytest
PYTHONPATH=src pytest tests/test_native_url.py -v
```

Expected: all PASS.

- [ ] **Step 4: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/_native.py \
        packages/simplex-chat-python/tests/test_native_url.py
git commit -m "feat(python): _native platform detection + URL building"
```

### Task 3.2: `_native.py` — cache resolution + lazy download

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/_native.py`
- Create: `packages/simplex-chat-python/tests/test_native_cache.py`

- [ ] **Step 1: Write cache-resolution tests**

```python
# tests/test_native_cache.py
import zipfile
from pathlib import Path

import pytest

from simplex_chat._native import _cache_root, _resolve_libs_dir, _download


def test_cache_root_linux(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    assert _cache_root() == tmp_path / "simplex-chat"

def test_cache_root_macos(tmp_path, monkeypatch):
    monkeypatch.setattr("sys.platform", "darwin")
    monkeypatch.setattr("pathlib.Path.home", lambda: tmp_path)
    assert _cache_root() == tmp_path / "Library" / "Caches" / "simplex-chat"

def test_override_via_env(tmp_path, monkeypatch):
    # _resolve_libs_dir intentionally does not validate the override directory —
    # it returns it verbatim; the eventual ctypes.CDLL call surfaces any mistake.
    monkeypatch.setenv("SIMPLEX_LIBS_DIR", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    assert _resolve_libs_dir("sqlite") == tmp_path

def test_resolve_downloads_when_missing(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    monkeypatch.setattr("simplex_chat._native._platform_tag", lambda: "linux-x86_64")

    called = {}
    def fake_download(target_root: Path, backend: str) -> None:
        called["target"] = target_root
        called["backend"] = backend
        target_root.mkdir(parents=True, exist_ok=True)
        (target_root / "libsimplex.so").touch()

    monkeypatch.setattr("simplex_chat._native._download", fake_download)
    libs_dir = _resolve_libs_dir("sqlite")
    assert libs_dir == tmp_path / "simplex-chat" / "v6.5.1" / "sqlite"
    assert called["backend"] == "sqlite"
    assert (libs_dir / "libsimplex.so").exists()

def test_resolve_uses_cache_on_second_call(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path))
    monkeypatch.setattr("sys.platform", "linux")
    cached = tmp_path / "simplex-chat" / "v6.5.1" / "sqlite"
    cached.mkdir(parents=True)
    (cached / "libsimplex.so").touch()
    # Should NOT call _download — use the cached file.
    monkeypatch.setattr("simplex_chat._native._download",
                        lambda *a: pytest.fail("download should not be called"))
    assert _resolve_libs_dir("sqlite") == cached

def test_postgres_on_macos_rejected(monkeypatch):
    monkeypatch.setattr("sys.platform", "darwin")
    monkeypatch.setattr("simplex_chat._native._platform_tag", lambda: "macos-aarch64")
    with pytest.raises(RuntimeError, match="postgres.*linux-x86_64"):
        _resolve_libs_dir("postgres")

def test_atomic_install(tmp_path, monkeypatch):
    """Build a fake libs zip, mock urlretrieve, verify extraction + atomic rename."""
    # Build zip: libs/libsimplex.so + libs/libHS-stub.so
    src = tmp_path / "src" / "libs"
    src.mkdir(parents=True)
    (src / "libsimplex.so").write_text("fake-so")
    (src / "libHS-stub.so").write_text("fake-hs")
    zip_path = tmp_path / "fake-libs.zip"
    with zipfile.ZipFile(zip_path, "w") as zf:
        for f in src.iterdir():
            zf.write(f, f"libs/{f.name}")

    def fake_urlretrieve(url: str, dest: str) -> None:
        import shutil
        shutil.copy(zip_path, dest)

    monkeypatch.setattr("urllib.request.urlretrieve", fake_urlretrieve)
    monkeypatch.setattr("simplex_chat._native._platform_tag", lambda: "linux-x86_64")

    target = tmp_path / "out"
    _download(target, "sqlite")
    assert (target / "libsimplex.so").read_text() == "fake-so"
    assert (target / "libHS-stub.so").read_text() == "fake-hs"
```

- [ ] **Step 2: Implement `_cache_root`, `_resolve_libs_dir`, `_download`**

Append to `_native.py`:

```python
def _cache_root() -> Path:
    if sys.platform == "darwin":
        return Path.home() / "Library" / "Caches" / "simplex-chat"
    if sys.platform == "win32":
        return Path(os.environ["LOCALAPPDATA"]) / "simplex-chat"
    base = os.environ.get("XDG_CACHE_HOME") or str(Path.home() / ".cache")
    return Path(base) / "simplex-chat"


def _resolve_libs_dir(backend: Backend) -> Path:
    if override := os.environ.get("SIMPLEX_LIBS_DIR"):
        return Path(override)
    if backend == "postgres" and _platform_tag() != "linux-x86_64":
        raise RuntimeError(
            "postgres backend is only supported on linux-x86_64; "
            f"current platform is {_platform_tag()}"
        )
    target = _cache_root() / f"v{LIBS_VERSION}" / backend
    if not (target / _libname()).exists():
        _download(target, backend)
    return target


def _download(target: Path, backend: Backend) -> None:
    """Download libs zip → atomic rename into `target`. Concurrent processes safe.

    Atomicity strategy: each process extracts to its own sibling tempdir on the same
    filesystem, then `os.rename` the `libs/` subdir to `target`. POSIX `os.rename`
    onto a NON-EXISTENT path is atomic; if the target exists (another process won
    the race), `os.rename` fails on most platforms — we then verify the winner has
    what we need and proceed. NEVER rmtree the target: that creates a TOCTOU
    window where another process is reading/loading the file we're deleting.
    """
    target.parent.mkdir(parents=True, exist_ok=True)
    print(
        f"Downloading libsimplex ({_platform_tag()}, {backend}) "
        f"v{LIBS_VERSION} ...",
        file=sys.stderr,
        flush=True,
    )
    with tempfile.TemporaryDirectory(dir=target.parent) as tmp:
        zip_path = Path(tmp) / "libs.zip"
        urllib.request.urlretrieve(_libs_url(backend), zip_path)
        with zipfile.ZipFile(zip_path) as zf:
            zf.extractall(tmp)
        # zip layout: <tmp>/libs/libsimplex.* + libHS*.*
        extracted_libs = Path(tmp) / "libs"
        if not extracted_libs.is_dir():
            raise RuntimeError(f"libs/ missing from {_libs_url(backend)}")
        try:
            os.rename(extracted_libs, target)
        except OSError as e:
            # EEXIST / ENOTEMPTY mean another process won the race — fall through
            # and check that the winner left a usable libsimplex behind. Anything
            # else (ENOSPC, EACCES, EROFS, Windows codes mapped to None) is a real
            # failure and must propagate. Same VERSION cached → same content →
            # safe to proceed once we've confirmed the file is there.
            if e.errno not in (errno.EEXIST, errno.ENOTEMPTY):
                raise
            if not (target / _libname()).exists():
                raise RuntimeError(
                    f"another process partially populated {target} but libsimplex "
                    f"is missing; remove the directory manually and retry"
                ) from e
```

- [ ] **Step 3: Run tests**

```
PYTHONPATH=src pytest tests/test_native_cache.py -v
```

Expected: all PASS.

- [ ] **Step 4: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/_native.py \
        packages/simplex-chat-python/tests/test_native_cache.py
git commit -m "feat(python): _native cache resolution and lazy download"
```

### Task 3.3: `_native.py` — ctypes signatures, `hs_init`, lib loader

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/_native.py`

- [ ] **Step 1: Append the loader**

(Imports for `ctypes`, `threading`, and the `from ctypes import …` line were already hoisted to the top of `_native.py` in Task 3.1 — do not re-add them here.)

```python
_lock = threading.Lock()
_lib: ctypes.CDLL | None = None
_libc: ctypes.CDLL | None = None
_backend: Backend | None = None


def _load_libc() -> ctypes.CDLL:
    if sys.platform == "win32":
        return ctypes.CDLL("msvcrt")
    return ctypes.CDLL(None)         # libc on POSIX is the process's own symbol table


def _setup_signatures(lib: ctypes.CDLL) -> None:
    """Declare argtypes/restype for the 8 chat_* functions exported by libsimplex.

    All result strings come back as raw c_void_p so the caller can free them
    after copying — matches HandleCResult in cpp/simplex.cc:157-165.
    """
    lib.chat_migrate_init.argtypes  = [c_char_p, c_char_p, c_char_p, POINTER(c_void_p)]
    lib.chat_migrate_init.restype   = c_void_p
    lib.chat_close_store.argtypes   = [c_void_p]
    lib.chat_close_store.restype    = c_void_p
    lib.chat_send_cmd.argtypes      = [c_void_p, c_char_p]
    lib.chat_send_cmd.restype       = c_void_p
    lib.chat_recv_msg_wait.argtypes = [c_void_p, c_int]
    lib.chat_recv_msg_wait.restype  = c_void_p
    lib.chat_write_file.argtypes    = [c_void_p, c_char_p, POINTER(c_uint8), c_int]
    lib.chat_write_file.restype     = c_void_p
    lib.chat_read_file.argtypes     = [c_char_p, c_char_p, c_char_p]
    lib.chat_read_file.restype      = POINTER(c_uint8)
    lib.chat_encrypt_file.argtypes  = [c_void_p, c_char_p, c_char_p]
    lib.chat_encrypt_file.restype   = c_void_p
    lib.chat_decrypt_file.argtypes  = [c_char_p, c_char_p, c_char_p, c_char_p]
    lib.chat_decrypt_file.restype   = c_void_p


def _hs_init(lib: ctypes.CDLL) -> None:
    """Initialize the Haskell runtime exactly once. Mirrors cpp/simplex.cc:13-32."""
    if sys.platform == "win32":
        argv_strs = [b"simplex", b"+RTS", b"-A64m", b"-H64m", b"--install-signal-handlers=no"]
    else:
        argv_strs = [b"simplex", b"+RTS", b"-A64m", b"-H64m", b"-xn", b"--install-signal-handlers=no"]
    argc = c_int(len(argv_strs))
    arr = (c_char_p * (len(argv_strs) + 1))(*argv_strs, None)
    arr_ptr = ctypes.byref(ctypes.cast(arr, POINTER(c_char_p)))
    lib.hs_init_with_rtsopts.argtypes = [POINTER(c_int), POINTER(POINTER(c_char_p))]
    lib.hs_init_with_rtsopts.restype = None
    lib.hs_init_with_rtsopts(ctypes.byref(argc), arr_ptr)


def lib_for(backend: Backend) -> ctypes.CDLL:
    """Resolve, load, and initialize libsimplex for the given backend.

    Idempotent for the same backend; raises if called with a different backend.
    Concurrent calls serialize on the module-level lock.
    """
    global _lib, _libc, _backend
    with _lock:
        if _lib is not None:
            if _backend != backend:
                raise RuntimeError(
                    f"libsimplex already loaded with backend={_backend!r}; "
                    f"cannot switch to {backend!r} in the same process"
                )
            return _lib
        libs_dir = _resolve_libs_dir(backend)
        lib = ctypes.CDLL(str(libs_dir / _libname()))
        _setup_signatures(lib)
        _hs_init(lib)
        _libc = _load_libc()
        _lib = lib
        _backend = backend
        return lib


def libc() -> ctypes.CDLL:
    """libc — needed by `core` to free Haskell-allocated result strings."""
    if _libc is None:
        raise RuntimeError("lib_for() must be called before libc()")
    return _libc
```

- [ ] **Step 2: Sanity-check imports**

```
PYTHONPATH=src python -c "import simplex_chat._native; print('ok')"
```

Expected: prints `ok`.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/_native.py
git commit -m "feat(python): _native ctypes signatures, hs_init, lib loader"
```

---

## Chunk 4: Core wrapper (`core.py`)

Typed async wrappers around the 8 FFI functions. Handles JSON parse, buffer free, error translation. No public API yet — that lands in `ChatApi`.

### Task 4.1: `core.py` — exceptions, enums, `chat_send_cmd`, `chat_recv_msg_wait`

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/core.py`

- [ ] **Step 1: Write the core module**

```python
"""Internal typed async wrapper around libsimplex's 8 C ABI functions.

Users interact with `Bot` / `ChatApi`. This module is exposed as
`simplex_chat.core` for tests and the api.ChatApi class only.
"""
from __future__ import annotations

import asyncio
import ctypes
import json
from enum import StrEnum
from typing import TypedDict

from . import _native
from .types import T, CR, CEvt


class ChatAPIError(Exception):
    """Raised when chat_send_cmd / chat_recv_msg_wait returns a chat error."""
    def __init__(self, message: str, chat_error: T.ChatError | None = None):
        super().__init__(message)
        self.chat_error = chat_error


class ChatInitError(Exception):
    """Raised when chat_migrate_init returns a DBMigrationResult error."""
    def __init__(self, message: str, db_migration_error):
        super().__init__(message)
        self.db_migration_error = db_migration_error


class MigrationConfirmation(StrEnum):
    YES_UP = "yesUp"
    YES_UP_DOWN = "yesUpDown"
    CONSOLE = "console"
    ERROR = "error"


class CryptoArgs(TypedDict):    # wire-format JSON; camelCase fields
    fileKey: str
    fileNonce: str


def _read_and_free(ptr: int | None) -> str:
    """Copy a Haskell-allocated null-terminated UTF-8 string and free its buffer.

    Mirrors HandleCResult in packages/simplex-chat-nodejs/cpp/simplex.cc:157-165.
    """
    if not ptr:
        raise RuntimeError("null pointer returned from libsimplex")
    try:
        return ctypes.string_at(ptr).decode("utf-8")
    finally:
        _native.libc().free(ctypes.c_void_p(ptr))


async def chat_send_cmd(ctrl: int, cmd: str) -> CR.ChatResponse:
    def _call() -> str:
        ptr = _native._lib.chat_send_cmd(ctrl, cmd.encode("utf-8"))
        return _read_and_free(ptr)
    raw = await asyncio.to_thread(_call)
    parsed = json.loads(raw)
    if "result" in parsed and isinstance(parsed["result"], dict):
        return parsed["result"]                                                                                    # type: ignore[return-value]
    err = parsed.get("error")
    if isinstance(err, dict):
        raise ChatAPIError(f"chat command error: {err.get('type')}", err)
    raise ChatAPIError(f"invalid chat command result: {raw[:200]}")


async def chat_recv_msg_wait(ctrl: int, wait_us: int = 5_000_000) -> CEvt.ChatEvent | None:
    def _call() -> str:
        ptr = _native._lib.chat_recv_msg_wait(ctrl, wait_us)
        if not ptr:
            return ""
        return _read_and_free(ptr)
    raw = await asyncio.to_thread(_call)
    if not raw:
        return None
    parsed = json.loads(raw)
    if "result" in parsed and isinstance(parsed["result"], dict):
        return parsed["result"]                                                                                    # type: ignore[return-value]
    err = parsed.get("error")
    if isinstance(err, dict):
        raise ChatAPIError(f"chat event error: {err.get('type')}", err)
    raise ChatAPIError(f"invalid chat event: {raw[:200]}")
```

- [ ] **Step 2: Verify import**

```
PYTHONPATH=src python -c "from simplex_chat.core import chat_send_cmd, ChatAPIError, MigrationConfirmation; print('ok')"
```

Expected: `ok`.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/core.py
git commit -m "feat(python): core typed wrappers for chat_send_cmd + chat_recv_msg_wait"
```

### Task 4.2: `core.py` — remaining FFI functions (`chat_migrate_init`, `chat_close_store`, file ops)

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/core.py`

- [ ] **Step 1: Append the remaining functions**

```python
async def chat_migrate_init(
    db_path: str, db_key: str, confirm: MigrationConfirmation
) -> int:
    """Initialize chat controller. Returns opaque ctrl pointer as Python int."""
    def _call() -> tuple[int, str]:
        ctrl = ctypes.c_void_p()
        ptr = _native._lib.chat_migrate_init(
            db_path.encode("utf-8"),
            db_key.encode("utf-8"),
            confirm.encode("utf-8"),
            ctypes.byref(ctrl),
        )
        return (ctrl.value or 0, _read_and_free(ptr))
    ctrl_val, raw = await asyncio.to_thread(_call)
    parsed = json.loads(raw)
    if parsed.get("type") == "ok":
        return ctrl_val
    raise ChatInitError(
        "Database or migration error (see db_migration_error)",
        parsed,
    )


async def chat_close_store(ctrl: int) -> None:
    def _call() -> str:
        ptr = _native._lib.chat_close_store(ctrl)
        return _read_and_free(ptr)
    res = await asyncio.to_thread(_call)
    if res:
        raise RuntimeError(res)


async def chat_write_file(ctrl: int, path: str, data: bytes) -> CryptoArgs:
    def _call() -> str:
        buf = (ctypes.c_uint8 * len(data)).from_buffer_copy(data)
        ptr = _native._lib.chat_write_file(ctrl, path.encode("utf-8"), buf, len(data))
        return _read_and_free(ptr)
    raw = await asyncio.to_thread(_call)
    return _crypto_args_result(raw)


async def chat_read_file(path: str, args: CryptoArgs) -> bytes:
    def _call() -> bytes:
        ptr = _native._lib.chat_read_file(
            path.encode("utf-8"),
            args["fileKey"].encode("utf-8"),
            args["fileNonce"].encode("utf-8"),
        )
        if not ptr:
            raise RuntimeError("chat_read_file returned null")
        addr = ctypes.cast(ptr, ctypes.c_void_p).value or 0
        try:
            status = ctypes.cast(addr, ctypes.POINTER(ctypes.c_uint8))[0]
            if status == 1:
                msg = ctypes.string_at(addr + 1).decode("utf-8")
                raise RuntimeError(msg)
            if status != 0:
                raise RuntimeError(f"unexpected status {status} from chat_read_file")
            length = ctypes.cast(addr + 1, ctypes.POINTER(ctypes.c_uint32))[0]
            return ctypes.string_at(addr + 5, length)
        finally:
            _native.libc().free(ctypes.c_void_p(addr))
    return await asyncio.to_thread(_call)


async def chat_encrypt_file(ctrl: int, src: str, dst: str) -> CryptoArgs:
    def _call() -> str:
        ptr = _native._lib.chat_encrypt_file(
            ctrl, src.encode("utf-8"), dst.encode("utf-8")
        )
        return _read_and_free(ptr)
    return _crypto_args_result(await asyncio.to_thread(_call))


async def chat_decrypt_file(src: str, args: CryptoArgs, dst: str) -> None:
    def _call() -> str:
        ptr = _native._lib.chat_decrypt_file(
            src.encode("utf-8"),
            args["fileKey"].encode("utf-8"),
            args["fileNonce"].encode("utf-8"),
            dst.encode("utf-8"),
        )
        return _read_and_free(ptr)
    res = await asyncio.to_thread(_call)
    if res:
        raise RuntimeError(res)


def _crypto_args_result(raw: str) -> CryptoArgs:
    parsed = json.loads(raw)
    if parsed.get("type") == "result":
        return parsed["cryptoArgs"]
    if parsed.get("type") == "error":
        raise RuntimeError(parsed.get("writeError", "unknown write error"))
    raise RuntimeError(f"unexpected result: {raw[:200]}")
```

- [ ] **Step 2: Re-verify import**

```
PYTHONPATH=src python -c "from simplex_chat import core; print(dir(core))"
```

Expected: prints a list including `chat_migrate_init`, `chat_close_store`, all eight functions.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/core.py
git commit -m "feat(python): core wrappers for migrate, close, file ops"
```

---

## Chunk 5: ChatApi class

Escape-hatch class with the 6 control methods plus ~40 `api_xxx` methods, one per Node `apiXxx`. Repetitive — done in batches grouped by domain.

### Task 5.1: `api.py` — `ChatApi` class with control methods + `Db` config

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/api.py`

- [ ] **Step 1: Write Db config dataclasses + ChatApi base**

```python
"""Low-level escape-hatch API. Most users go through `Bot` instead."""
from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from . import core
from .core import ChatAPIError, ChatInitError, MigrationConfirmation
from .types import CC, CEvt, CR, T


@dataclass(slots=True)
class SqliteDb:
    file_prefix: str
    encryption_key: str | None = None


@dataclass(slots=True)
class PostgresDb:
    connection_string: str
    schema_prefix: str | None = None


Db = SqliteDb | PostgresDb


def _db_to_migrate_args(db: Db) -> tuple[str, str, str]:
    """Returns (path-or-prefix, key-or-conn, backend)."""
    if isinstance(db, SqliteDb):
        return (db.file_prefix, db.encryption_key or "", "sqlite")
    if isinstance(db, PostgresDb):
        return (db.schema_prefix or "", db.connection_string, "postgres")
    raise TypeError(f"Unknown db: {db!r}")


class ChatCommandError(Exception):
    def __init__(self, message: str, response: CR.ChatResponse):
        super().__init__(message)
        self.response = response


class ChatApi:
    def __init__(self, ctrl: int, backend: str):
        self._ctrl = ctrl
        self._backend = backend

    @classmethod
    async def init(
        cls,
        db: Db,
        confirm: MigrationConfirmation = MigrationConfirmation.YES_UP,
    ) -> "ChatApi":
        from . import _native
        path_or_prefix, key_or_conn, backend = _db_to_migrate_args(db)
        # Trigger lazy lib load with the right backend BEFORE chat_migrate_init.
        _native.lib_for(backend)         # type: ignore[arg-type]
        ctrl = await core.chat_migrate_init(path_or_prefix, key_or_conn, confirm)
        return cls(ctrl, backend)

    @property
    def ctrl(self) -> int:
        return self._ctrl

    async def start_chat(self) -> None:
        r = await self.send_chat_cmd(
            CC.StartChat_cmd_string({"mainApp": True, "enableSndFiles": True})
        )
        if r.get("type") not in ("chatStarted", "chatRunning"):
            raise ChatCommandError("error starting chat", r)

    async def stop_chat(self) -> None:
        r = await self.send_chat_cmd("/_stop")
        if r.get("type") != "chatStopped":
            raise ChatCommandError("error stopping chat", r)

    async def close(self) -> None:
        await core.chat_close_store(self._ctrl)

    async def send_chat_cmd(self, cmd: str) -> CR.ChatResponse:
        return await core.chat_send_cmd(self._ctrl, cmd)

    async def recv_chat_event(self, wait_us: int = 5_000_000) -> CEvt.ChatEvent | None:
        return await core.chat_recv_msg_wait(self._ctrl, wait_us)
```

- [ ] **Step 2: Verify import**

```
PYTHONPATH=src python -c "from simplex_chat.api import ChatApi, SqliteDb, PostgresDb; print('ok')"
```

Expected: `ok`.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/api.py
git commit -m "feat(python): ChatApi base class with control methods + Db config"
```

### Task 5.2: `api.py` — implement ~40 `api_xxx` methods

Mirror methods from `packages/simplex-chat-nodejs/src/api.ts:344-958`. Each method is the same shape:

```python
async def api_<snake_name>(self, ...args) -> <ReturnType>:
    r = await self.send_chat_cmd(CC.<NodeName>_cmd_string({"...": args, ...}))
    if r["type"] == "<expectedTag>":
        return r["<field>"]
    raise ChatCommandError("error <verb>", r)
```

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/api.py`

- [ ] **Step 1: Reference the Node lib while implementing**

```bash
# Open the Node source side-by-side
sed -n '344,958p' packages/simplex-chat-nodejs/src/api.ts | less
```

- [ ] **Step 2: Implement methods in groups, one commit per group**

The Node file groups by domain — port them in the same order:

| Group | Methods (Node names → Python snake_case) | Lines in api.ts |
|---|---|---|
| Address | apiCreateUserAddress, apiDeleteUserAddress, apiGetUserAddress, apiSetProfileAddress, apiSetAddressSettings | 344-409 |
| Messages | apiSendMessages, apiSendTextMessage, apiSendTextReply, apiUpdateChatItem, apiDeleteChatItems, apiDeleteMemberChatItem, apiChatItemReaction | 411-505 |
| Files | apiReceiveFile, apiCancelFile | 507-525 |
| Groups | apiAddMember, apiJoinGroup, apiAcceptMember, apiSetMembersRole, apiBlockMembersForAll, apiRemoveMembers, apiLeaveGroup, apiListMembers, apiNewGroup, apiUpdateGroupProfile | 527-625 |
| Group links | apiCreateGroupLink, apiSetGroupLinkMemberRole, apiDeleteGroupLink, apiGetGroupLink, apiGetGroupLinkStr | 627-672 |
| Connections | apiCreateLink, apiConnectPlan, apiConnect, apiConnectActiveUser, apiAcceptContactRequest, apiRejectContactRequest | 674-746 |
| Chats | apiListContacts, apiListGroups, apiGetChats, apiDeleteChat, apiSetGroupCustomData, apiSetContactCustomData, apiSetAutoAcceptMemberContacts, apiGetChat | 748-841 |
| Users | apiGetActiveUser, apiCreateActiveUser, apiListUsers, apiSetActiveUser, apiDeleteUser, apiUpdateProfile, apiSetContactPrefs | 843-928 |
| Member contacts | apiCreateMemberContact, apiSendMemberContactInvitation | 930-957 |

For each method:
- TS `apiCreateUserAddress(userId)` → Python `api_create_user_address(self, user_id: int) -> T.CreatedConnLink`
- Use the autogenerated `CC.<NodeName>_cmd_string({...})` to build the command string. Field names inside the dict are camelCase wire format.
- Use type narrowing on `r["type"]` to extract the expected response field.

Example port:

```python
async def api_create_user_address(self, user_id: int) -> T.CreatedConnLink:
    r = await self.send_chat_cmd(CC.APICreateMyAddress_cmd_string({"userId": user_id}))
    if r["type"] == "userContactLinkCreated":
        return r["connLinkContact"]
    raise ChatCommandError("error creating user address", r)
```

Commit pattern: one commit per group from the table above. Commit messages:

```bash
git commit -m "feat(python): ChatApi address methods"
git commit -m "feat(python): ChatApi message methods"
git commit -m "feat(python): ChatApi file methods"
# … etc, one per group
```

- [ ] **Step 3: After all groups land, verify all methods are present**

```bash
grep -c "async def api_" packages/simplex-chat-python/src/simplex_chat/api.py
```

Expected: ≥40 (matches the count of `apiXxx` methods in api.ts).

- [ ] **Step 4: Verify import after all groups**

```
PYTHONPATH=src python -c "from simplex_chat.api import ChatApi; api = ChatApi.__init__; print('ok')"
```

Expected: `ok`.

---

## Chunk 6: Bot class

User-facing `Bot`: decorator-registered handlers, kwarg filters, `Message` wrapper with content-narrowed subclasses, dual lifecycle, middleware.

### Task 6.1: `Message` wrapper class + content-narrowed aliases

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/bot.py`

- [ ] **Step 1: Write `Message` and aliases**

```python
"""User-facing `Bot` API: decorators, filters, Message wrapper, lifecycle."""
from __future__ import annotations

import asyncio
import logging
import re
import signal as _signal
from dataclasses import dataclass
from typing import (
    Any, Awaitable, Callable, Generic, Literal, TYPE_CHECKING, TypeVar, overload
)

from . import _native
from .api import ChatApi, ChatCommandError, Db, PostgresDb, SqliteDb
from .core import ChatAPIError, MigrationConfirmation
from .types import CC, CEvt, CR, T

if TYPE_CHECKING:
    from typing_extensions import TypeAlias

log = logging.getLogger("simplex_chat")

C = TypeVar("C", bound=T.MsgContent)


@dataclass(slots=True)
class BotProfile:
    display_name: str
    full_name: str = ""
    short_descr: str | None = None
    image: str | None = None


@dataclass(slots=True)
class BotCommand:
    keyword: str
    label: str


@dataclass(slots=True, frozen=True)
class ParsedCommand:
    keyword: str
    args: str


@dataclass(slots=True, frozen=True)
class Message(Generic[C]):
    chat_item: T.AChatItem
    content: C
    bot: "Bot"

    @property
    def chat_info(self) -> T.ChatInfo:
        return self.chat_item["chatInfo"]

    @property
    def text(self) -> str | None:
        c = self.content
        if isinstance(c, dict):
            return c.get("text")                                       # type: ignore[return-value]
        return None

    async def reply(self, text: str) -> "Message":
        items = await self.bot.api.api_send_text_reply(self.chat_item, text)
        return Message(chat_item=items[0], content=items[0]["chatItem"]["content"], bot=self.bot)

    async def reply_content(self, content: T.MsgContent) -> "Message":
        items = await self.bot.api.api_send_messages(
            self.chat_info, [{"msgContent": content, "mentions": {}}]
        )
        return Message(chat_item=items[0], content=items[0]["chatItem"]["content"], bot=self.bot)

    async def react(self, emoji: str) -> None:
        # Implementation defers to ChatApi.api_chat_item_reaction
        ...

    async def delete(self) -> None: ...
    async def forward(self, to: T.ChatRef) -> "Message": ...


# Concrete narrowed aliases — exported from package __init__.py
TextMessage    = Message[T.MsgContent_Text]
ImageMessage   = Message[T.MsgContent_Image]
FileMessage    = Message[T.MsgContent_File]
VoiceMessage   = Message[T.MsgContent_Voice]
VideoMessage   = Message[T.MsgContent_Video]
LinkMessage    = Message[T.MsgContent_Link]
# … one per T.MsgContent_* variant; full list mirrors what's emitted in _types.py
```

- [ ] **Step 2: Verify import**

```
PYTHONPATH=src python -c "from simplex_chat.bot import Message, TextMessage, BotProfile; print('ok')"
```

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/bot.py
git commit -m "feat(python): Bot module skeleton — Message wrapper + aliases"
```

### Task 6.2: Filter compilation (`filters.py`)

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/filters.py`
- Create: `packages/simplex-chat-python/tests/test_filters.py`

- [ ] **Step 1: Write filter tests**

```python
# tests/test_filters.py
import re
import pytest
from simplex_chat.filters import compile_message_filter

def _msg(content_type="text", text=None, chat_type="direct",
         from_role=None, from_contact_id=None, group_id=None):
    """Build a minimal mock Message-like object for filter testing."""
    class M:
        pass
    m = M()
    m.content = {"type": content_type, "text": text} if text is not None else {"type": content_type}
    m.chat_item = {"chatInfo": {
        "type": chat_type,
        **({"groupInfo": {"groupId": group_id}} if chat_type == "group" else {}),
    }}
    # Sender extraction is implementation-detail of the filter — keep test fixture pragmatic.
    m._from_role = from_role
    m._from_contact_id = from_contact_id
    return m

def test_no_filters_matches_all():
    f = compile_message_filter({})
    assert f(_msg(content_type="text"))
    assert f(_msg(content_type="image"))

def test_content_type_singular():
    f = compile_message_filter({"content_type": "text"})
    assert f(_msg(content_type="text"))
    assert not f(_msg(content_type="image"))

def test_content_type_tuple_or():
    f = compile_message_filter({"content_type": ("text", "image")})
    assert f(_msg(content_type="text"))
    assert f(_msg(content_type="image"))
    assert not f(_msg(content_type="voice"))

def test_text_exact():
    f = compile_message_filter({"text": "hello"})
    assert f(_msg(text="hello"))
    assert not f(_msg(text="world"))

def test_text_regex():
    f = compile_message_filter({"text": re.compile(r"^\d+$")})
    assert f(_msg(text="123"))
    assert not f(_msg(text="abc"))

def test_when_callable():
    f = compile_message_filter({"when": lambda m: m.content["type"] == "voice"})
    assert f(_msg(content_type="voice"))
    assert not f(_msg(content_type="text"))

def test_combined_and():
    f = compile_message_filter({"content_type": "text", "text": re.compile(r"\d")})
    assert f(_msg(content_type="text", text="abc123"))
    assert not f(_msg(content_type="text", text="abc"))
    assert not f(_msg(content_type="image"))
```

- [ ] **Step 2: Implement `compile_message_filter`**

```python
# filters.py
from __future__ import annotations
import re
from typing import Any, Callable

def compile_message_filter(kw: dict[str, Any]) -> Callable[[Any], bool]:
    """Compile filter kwargs into a single predicate function.

    Multiple kwargs combine with AND; tuples within a kwarg combine with OR.
    `when` is the last predicate evaluated.
    """
    predicates: list[Callable[[Any], bool]] = []

    if (ct := kw.get("content_type")) is not None:
        ct_set = (ct,) if isinstance(ct, str) else tuple(ct)
        predicates.append(lambda m: m.content.get("type") in ct_set)

    if (t := kw.get("text")) is not None:
        if isinstance(t, re.Pattern):
            predicates.append(lambda m: bool(t.search(m.content.get("text", "") or "")))
        else:
            predicates.append(lambda m: m.content.get("text") == t)

    if (ct := kw.get("chat_type")) is not None:
        ct_set = (ct,) if isinstance(ct, str) else tuple(ct)
        predicates.append(lambda m: m.chat_item["chatInfo"]["type"] in ct_set)

    if (gid := kw.get("group_id")) is not None:
        gid_set = (gid,) if isinstance(gid, int) else tuple(gid)
        def gid_match(m: Any) -> bool:
            ci = m.chat_item["chatInfo"]
            return ci["type"] == "group" and ci["groupInfo"]["groupId"] in gid_set
        predicates.append(gid_match)

    # from_role, from_contact_id, from_member_id are looked up via helpers in filters.py
    # — defer to integration with ChatInfo / GroupMember accessors implemented later.

    if (when := kw.get("when")) is not None:
        predicates.append(when)

    if not predicates:
        return lambda _m: True
    return lambda m: all(p(m) for p in predicates)
```

- [ ] **Step 3: Run filter tests**

```
PYTHONPATH=src pytest tests/test_filters.py -v
```

Expected: all PASS.

- [ ] **Step 4: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/filters.py \
        packages/simplex-chat-python/tests/test_filters.py
git commit -m "feat(python): filter kwarg compilation + tests"
```

### Task 6.3: `Bot` class — construction, decorators, registration

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/bot.py`

- [ ] **Step 1: Add `Bot` class with `__init__`, decorator methods, registration storage**

```python
# Append to bot.py

MessageHandler = Callable[[Message], Awaitable[None]]
CommandHandler = Callable[[Message, ParsedCommand], Awaitable[None]]
EventHandler   = Callable[[CEvt.ChatEvent], Awaitable[None]]
# Note: handlers are async-only (matches spec). Users must `async def handler(...)`.


class Middleware:
    async def __call__(
        self,
        handler: Callable[[Message, dict[str, object]], Awaitable[None]],
        message: Message,
        data: dict[str, object],
    ) -> None:
        await handler(message, data)


class Bot:
    def __init__(
        self,
        *,
        profile: BotProfile,
        db: Db,
        welcome: str | T.MsgContent | None = None,
        commands: list[BotCommand] | None = None,
        confirm_migrations: MigrationConfirmation = MigrationConfirmation.YES_UP,
        create_address: bool = True,
        update_address: bool = True,
        update_profile: bool = True,
        auto_accept: bool = True,
        business_address: bool = False,
        allow_files: bool = False,
        use_bot_profile: bool = True,
        log_contacts: bool = True,
        log_network: bool = False,
    ) -> None:
        self._profile = profile
        self._db = db
        self._welcome = welcome
        self._commands = commands or []
        self._confirm_migrations = confirm_migrations
        self._opts = {
            "create_address": create_address,
            "update_address": update_address,
            "update_profile": update_profile,
            "auto_accept": auto_accept,
            "business_address": business_address,
            "allow_files": allow_files,
            "use_bot_profile": use_bot_profile,
            "log_contacts": log_contacts,
            "log_network": log_network,
        }
        self._api: ChatApi | None = None
        self._serving = False
        self._stop_event = asyncio.Event()
        self._message_handlers: list[tuple[Callable[[Message], bool], MessageHandler]] = []
        self._command_handlers: list[tuple[tuple[str, ...], Callable[[Message], bool], CommandHandler]] = []
        self._event_handlers: dict[str, list[EventHandler]] = {}
        self._middleware: list[Middleware] = []

    @property
    def api(self) -> ChatApi:
        if self._api is None:
            raise RuntimeError("Bot not initialized — call bot.run() or use `async with bot:`")
        return self._api

    # --- decorator registration (overloads omitted for brevity; see spec for full list) ---

    def on_message(self, **filter_kw: Any) -> Callable[[MessageHandler], MessageHandler]:
        from .filters import compile_message_filter
        predicate = compile_message_filter(filter_kw)
        def deco(fn: MessageHandler) -> MessageHandler:
            self._message_handlers.append((predicate, fn))
            return fn
        return deco

    def on_command(
        self, name: str | tuple[str, ...], **filter_kw: Any
    ) -> Callable[[CommandHandler], CommandHandler]:
        names = (name,) if isinstance(name, str) else tuple(name)
        from .filters import compile_message_filter
        predicate = compile_message_filter(filter_kw)
        def deco(fn: CommandHandler) -> CommandHandler:
            self._command_handlers.append((names, predicate, fn))
            return fn
        return deco

    def on_event(self, event: CEvt.Tag, /) -> Callable[[EventHandler], EventHandler]:
        def deco(fn: EventHandler) -> EventHandler:
            self._event_handlers.setdefault(event, []).append(fn)
            return fn
        return deco

    def use(self, middleware: Middleware) -> None:
        self._middleware.append(middleware)
```

- [ ] **Step 2: Discover the full `MsgContent` variant list from generated types**

After Chunk 1 lands, the generated file lists every variant. Get the canonical list:

```bash
grep -E '^class MsgContent_' packages/simplex-chat-python/src/simplex_chat/types/_types.py | sed 's/class \([^(]*\).*/\1/'
```

Expected output (approximately — exact list comes from Haskell `MsgContent` defined at `bots/src/API/Docs/Types.hs:309` with prefix `"MC"`):

```
MsgContent_Text
MsgContent_Link
MsgContent_Image
MsgContent_Video
MsgContent_Voice
MsgContent_File
MsgContent_Report
MsgContent_Chat
MsgContent_Unknown
```

For each variant in that list, define both a `<Name>Message` alias (in Step 1 above — extend the alias block to cover every variant) and one decorator overload (this step).

- [ ] **Step 3: Add the typed overloads — one per variant from Step 2**

After the plain `on_message` definition, add a typed overload for each variant. The pattern below shows two; repeat for every variant the previous step found:

```python
    # Type-only overloads — compiler-visible, no runtime effect.
    # MUST cover every variant from `grep '^class MsgContent_' _types.py`.
    @overload
    def on_message(self, *, content_type: Literal["text"], **rest: Any
                  ) -> Callable[[Callable[[TextMessage], Awaitable[None]]],
                                Callable[[TextMessage], Awaitable[None]]]: ...
    @overload
    def on_message(self, *, content_type: Literal["image"], **rest: Any
                  ) -> Callable[[Callable[[ImageMessage], Awaitable[None]]],
                                Callable[[ImageMessage], Awaitable[None]]]: ...
    # … one per MsgContent variant; verify count matches Step 2's grep output …
    @overload
    def on_message(self, **rest: Any
                  ) -> Callable[[MessageHandler], MessageHandler]: ...
```

The per-variant tag string (`"text"`, `"image"`, …) is the lowercased suffix after `MsgContent_` — see the `Literal[...]` member on each generated TypedDict's `type` field for the canonical spelling.

- [ ] **Step 3: Verify import + decorator binding**

```python
# tests/test_bot_registration.py
from simplex_chat.bot import Bot, BotProfile
from simplex_chat.api import SqliteDb

def test_decorator_registers_handler():
    bot = Bot(profile=BotProfile(display_name="x"), db=SqliteDb(file_prefix="/tmp/test"))
    @bot.on_message(content_type="text")
    async def h(msg): pass
    assert len(bot._message_handlers) == 1
```

```
PYTHONPATH=src pytest tests/test_bot_registration.py -v
```

Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/bot.py \
        packages/simplex-chat-python/tests/test_bot_registration.py
git commit -m "feat(python): Bot class — construction + decorator registration"
```

### Task 6.4: Lifecycle: `run()`, `serve_forever()`, `__aenter__/__aexit__`, `stop()`

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/bot.py`

- [ ] **Step 1: Append lifecycle methods**

```python
class Bot:
    # … existing methods …

    async def __aenter__(self) -> "Bot":
        self._api = await ChatApi.init(self._db, self._confirm_migrations)
        await self._api.start_chat()
        # TODO Task 6.5: profile + address sync via mkBotProfile/createOrUpdateAddress
        return self

    async def __aexit__(self, *exc_info: object) -> None:
        self.stop()
        if self._api is not None:
            try:
                await self._api.stop_chat()
            finally:
                await self._api.close()
                self._api = None

    def run(self) -> None:
        """Blocking entry: runs serve_forever() with a SIGINT handler installed."""
        async def _main() -> None:
            async with self:
                loop = asyncio.get_running_loop()
                if hasattr(_signal, "SIGINT"):
                    try:
                        loop.add_signal_handler(_signal.SIGINT, self.stop)
                        loop.add_signal_handler(_signal.SIGTERM, self.stop)
                    except NotImplementedError:           # Windows
                        _signal.signal(_signal.SIGINT, lambda *_: self.stop())
                await self.serve_forever()
        asyncio.run(_main())

    async def serve_forever(self) -> None:
        if self._serving:
            raise RuntimeError("already serving")
        self._serving = True
        self._stop_event.clear()
        try:
            await self._receive_loop()
        finally:
            self._serving = False

    def stop(self) -> None:
        self._stop_event.set()

    async def _receive_loop(self) -> None:
        while not self._stop_event.is_set():
            try:
                event = await self.api.recv_chat_event(wait_us=5_000_000)
            except ChatAPIError as e:
                log.error("chat event error: %s", e)
                continue
            if event is None:
                continue
            await self._dispatch_event(event)
```

- [ ] **Step 2: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/bot.py
git commit -m "feat(python): Bot lifecycle (run, serve_forever, async-context, stop)"
```

### Task 6.5: Event dispatch + handler invocation through middleware

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/bot.py`

- [ ] **Step 1: Append `_dispatch_event` and helpers**

```python
class Bot:
    async def _dispatch_event(self, event: CEvt.ChatEvent) -> None:
        # 1. Tag-targeted on_event handlers (registration order)
        tag = event["type"]
        for h in self._event_handlers.get(tag, []):
            try:
                await h(event)
            except Exception:
                log.exception("on_event handler failed")
        # 2. If newChatItems → message + command dispatch.
        # Tag check narrows the union; pyright sees event as CEvt.ChatEvent_NewChatItems below.
        if tag == "newChatItems":
            evt: CEvt.ChatEvent_NewChatItems = event       # type: ignore[assignment]
            for ci in evt["chatItems"]:
                content = ci["chatItem"]["content"]
                if content["type"] != "rcvMsgContent":
                    continue
                msg_content = content["msgContent"]
                msg = Message(chat_item=ci, content=msg_content, bot=self)
                await self._dispatch_message(msg)

    async def _dispatch_message(self, msg: Message) -> None:
        # Run all matching message handlers
        for predicate, handler in self._message_handlers:
            if predicate(msg):
                await self._invoke_with_middleware(handler, msg)
        # Then any matching command handlers
        cmd = self._parse_command(msg)
        if cmd is not None:
            for names, predicate, handler in self._command_handlers:
                if cmd.keyword in names and predicate(msg):
                    await self._invoke_command_with_middleware(handler, msg, cmd)

    async def _invoke_with_middleware(
        self, handler: MessageHandler, message: Message
    ) -> None:
        async def call(m: Message, _data: dict[str, object]) -> None:
            await handler(m)

        chain: Callable[[Message, dict[str, object]], Awaitable[None]] = call
        # mw=mw, inner=inner bind the loop variable (late-binding fix)
        for mw in reversed(self._middleware):
            inner = chain
            async def _wrapped(m: Message, d: dict[str, object], mw=mw, inner=inner) -> None:
                await mw(inner, m, d)
            chain = _wrapped

        try:
            await chain(message, {})
        except Exception:
            log.exception("message handler failed")

    async def _invoke_command_with_middleware(
        self, handler: CommandHandler, message: Message, cmd: ParsedCommand
    ) -> None:
        # Same shape as _invoke_with_middleware but the inner call gets cmd too.
        async def call(m: Message, _data: dict[str, object]) -> None:
            await handler(m, cmd)
        chain: Callable[[Message, dict[str, object]], Awaitable[None]] = call
        for mw in reversed(self._middleware):
            inner = chain
            async def _wrapped(m: Message, d: dict[str, object], mw=mw, inner=inner) -> None:
                await mw(inner, m, d)
            chain = _wrapped
        try:
            await chain(message, {})
        except Exception:
            log.exception("command handler failed")

    @staticmethod
    def _parse_command(msg: Message) -> ParsedCommand | None:
        text = msg.text
        if not text or not text.startswith("/"):
            return None
        body = text[1:].lstrip()
        if not body:
            return None
        if " " in body:
            kw, args = body.split(" ", 1)
            return ParsedCommand(keyword=kw, args=args.strip())
        return ParsedCommand(keyword=body, args="")
```

- [ ] **Step 2: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/bot.py
git commit -m "feat(python): Bot event dispatch + middleware chaining"
```

### Task 6.6: Profile + address sync (mirror Node `bot.ts:158-214`)

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/bot.py`

- [ ] **Step 1: Implement initial profile + address sync inside `__aenter__`**

Mirror `bot.ts` `createBotUser`, `createOrUpdateAddress`, `updateBotUserProfile`, `mkBotProfile`. Each is straightforward — fetch via `self._api.api_get_active_user()`, compare with `self._profile`, update if `update_profile=True`, etc. Reference: `packages/simplex-chat-nodejs/src/bot.ts:158-214`.

- [ ] **Step 2: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/bot.py
git commit -m "feat(python): Bot profile + address sync on init"
```

### Task 6.7: Update package `__init__.py` to export public API

**Files:**
- Modify: `packages/simplex-chat-python/src/simplex_chat/__init__.py`

- [ ] **Step 1: Add exports**

```python
"""SimpleX Chat — Python client library for chat bots."""
from ._version import __version__
from .api import ChatApi, ChatCommandError, Db, PostgresDb, SqliteDb
from .bot import (
    Bot,
    BotCommand,
    BotProfile,
    FileMessage,
    ImageMessage,
    Message,
    Middleware,
    ParsedCommand,
    TextMessage,
    VoiceMessage,
    # … all aliases …
)
from .core import ChatAPIError, ChatInitError, MigrationConfirmation, CryptoArgs

__all__ = [
    "__version__",
    # … all the names above …
]
```

- [ ] **Step 2: Verify clean import**

```
PYTHONPATH=src python -c "from simplex_chat import Bot, TextMessage, SqliteDb, BotProfile; print('ok')"
```

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/__init__.py
git commit -m "feat(python): export public API from package __init__"
```

---

## Chunk 7: Tests + CLI

Pytest suite covering native, codegen, smoke; pre-fetch CLI; example bot.

### Task 7.1: `__main__.py` — `python -m simplex_chat install`

**Files:**
- Create: `packages/simplex-chat-python/src/simplex_chat/__main__.py`

- [ ] **Step 1: Write the CLI**

```python
"""CLI: python -m simplex_chat install [--backend=sqlite|postgres]"""
from __future__ import annotations
import argparse
import sys
from . import _native


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(prog="simplex_chat")
    sub = p.add_subparsers(dest="command", required=True)
    install = sub.add_parser("install", help="Pre-fetch libsimplex into the user cache")
    install.add_argument(
        "--backend", choices=["sqlite", "postgres"], default="sqlite",
        help="which libsimplex variant to download (default: sqlite)",
    )
    args = p.parse_args(argv)
    if args.command == "install":
        try:
            path = _native._resolve_libs_dir(args.backend)
            print(f"libsimplex installed at: {path}")
            return 0
        except Exception as e:
            print(f"install failed: {e}", file=sys.stderr)
            return 1
    return 1


if __name__ == "__main__":
    sys.exit(main())
```

- [ ] **Step 2: Smoke-check the CLI exists**

```
PYTHONPATH=src python -m simplex_chat install --help
```

Expected: prints argparse help.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/src/simplex_chat/__main__.py
git commit -m "feat(python): python -m simplex_chat install CLI"
```

### Task 7.2: Codegen smoke test

**Files:**
- Create: `packages/simplex-chat-python/tests/test_codegen.py`

- [ ] **Step 1: Write the test**

```python
"""Sanity checks on auto-generated wire types — catches generator regressions."""
import typing
from simplex_chat.types import T, CC, CR, CEvt


def test_types_module_imports():
    """Every generated module imports cleanly with no SyntaxError."""
    assert T is not None and CC is not None and CR is not None and CEvt is not None


def test_chat_type_is_literal_enum():
    """ChatType should be a Literal of expected member set."""
    origin = typing.get_origin(T.ChatType)
    args = typing.get_args(T.ChatType)
    # Python ≥3.11 typing.Literal: origin is Literal, args is the tuple of values
    assert "direct" in args
    assert "group" in args
    assert "local" in args


def test_known_command_has_cmd_string():
    s = CC.APICreateMyAddress_cmd_string({"userId": 1})
    assert s == "/_address 1"


def test_chat_response_tag_alias_present():
    """ChatResponse_Tag union of literals exists."""
    assert hasattr(CR, "ChatResponse_Tag")
```

- [ ] **Step 2: Run**

```
PYTHONPATH=src pytest tests/test_codegen.py -v
```

Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/tests/test_codegen.py
git commit -m "test(python): codegen sanity checks"
```

### Task 7.3: Smoke test with stub libsimplex

**Files:**
- Create: `packages/simplex-chat-python/tests/test_smoke.py`
- Create: `packages/simplex-chat-python/tests/_stub_libsimplex.c`

- [ ] **Step 1: Write a tiny C stub that returns canned JSON**

```c
// _stub_libsimplex.c  — compile to libsimplex.so for smoke testing
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void hs_init_with_rtsopts(int *argc, char ***argv) { (void)argc; (void)argv; }

static char *dup_str(const char *s) { return strdup(s); }

char *chat_migrate_init(const char *path, const char *key, const char *confirm, void **ctrl) {
    (void)path; (void)key; (void)confirm;
    *ctrl = (void*)0x1;
    return dup_str("{\"type\":\"ok\"}");
}

char *chat_close_store(void *ctrl) { (void)ctrl; return dup_str(""); }

char *chat_send_cmd(void *ctrl, const char *cmd) {
    (void)ctrl; (void)cmd;
    return dup_str("{\"result\":{\"type\":\"chatStarted\"}}");
}

char *chat_recv_msg_wait(void *ctrl, int wait) {
    (void)ctrl; (void)wait;
    return NULL;
}
// stubs for the file functions:
char *chat_write_file() { return dup_str("{\"type\":\"result\",\"cryptoArgs\":{\"fileKey\":\"k\",\"fileNonce\":\"n\"}}"); }
char *chat_read_file()  { return NULL; }
char *chat_encrypt_file() { return dup_str("{\"type\":\"result\",\"cryptoArgs\":{\"fileKey\":\"k\",\"fileNonce\":\"n\"}}"); }
char *chat_decrypt_file() { return dup_str(""); }
```

- [ ] **Step 2: Write the smoke test that compiles and uses the stub**

```python
import asyncio
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
import pytest


@pytest.fixture
def stub_libs_dir(tmp_path):
    """Compile _stub_libsimplex.c into a libsimplex.so in tmp_path."""
    src = Path(__file__).parent / "_stub_libsimplex.c"
    if sys.platform == "win32":
        pytest.skip("stub compilation not implemented for Windows")
    libname = "libsimplex.dylib" if sys.platform == "darwin" else "libsimplex.so"
    out = tmp_path / libname
    cc = shutil.which("cc") or shutil.which("gcc") or pytest.skip("no C compiler")
    subprocess.run([cc, "-shared", "-fPIC", str(src), "-o", str(out)], check=True)
    return tmp_path


@pytest.mark.asyncio
async def test_chat_api_init_and_start(stub_libs_dir, monkeypatch):
    monkeypatch.setenv("SIMPLEX_LIBS_DIR", str(stub_libs_dir))
    from simplex_chat.api import ChatApi, SqliteDb
    api = await ChatApi.init(SqliteDb(file_prefix=str(stub_libs_dir / "db")))
    await api.start_chat()
    await api.close()
```

- [ ] **Step 3: Run**

`test` extras are already declared in `pyproject.toml` (Task 2.1).

```
pip install -e ".[test]"
PYTHONPATH=src pytest tests/test_smoke.py -v
```

Expected: PASS on Linux/macOS; SKIPPED on Windows.

- [ ] **Step 4: Commit**

```bash
git add packages/simplex-chat-python/tests/_stub_libsimplex.c \
        packages/simplex-chat-python/tests/test_smoke.py
git commit -m "test(python): smoke test against stub libsimplex"
```

### Task 7.4: Squaring-bot example

**Files:**
- Create: `packages/simplex-chat-python/examples/squaring_bot.py`

- [ ] **Step 1: Write the example from the spec**

```python
"""Squaring bot — receives numbers, replies with their squares.

Run: python examples/squaring_bot.py
"""
import re
from simplex_chat import (
    Bot, BotProfile, BotCommand, SqliteDb, TextMessage, Message, ParsedCommand
)

bot = Bot(
    profile=BotProfile(display_name="Squaring bot"),
    db=SqliteDb(file_prefix="./squaring_bot"),
    welcome="Send me a number, I'll square it.",
    commands=[BotCommand(keyword="help", label="Show help")],
)

NUMBER_RE = re.compile(r"^-?\d+(\.\d+)?$")

@bot.on_message(content_type="text", text=NUMBER_RE)
async def square(msg: TextMessage) -> None:
    n = float(msg.text)
    await msg.reply(f"{n} * {n} = {n * n}")

@bot.on_message(content_type="text")          # fallback
async def fallback(msg: Message) -> None:
    await msg.reply("Send me a number, like 7 or 3.14.")

@bot.on_command("help")
async def help_cmd(msg: Message, _cmd: ParsedCommand) -> None:
    await msg.reply("Send a number, I'll square it.")

if __name__ == "__main__":
    bot.run()
```

- [ ] **Step 2: Document in README**

Replace the placeholder in `packages/simplex-chat-python/README.md` with the example and `pip install simplex-chat` quick-start.

- [ ] **Step 3: Commit**

```bash
git add packages/simplex-chat-python/examples/squaring_bot.py \
        packages/simplex-chat-python/README.md
git commit -m "docs(python): squaring bot example + README quick-start"
```

---

## Chunk 8: CI publishing

Append `publish-python` job to existing `.github/workflows/build.yml` after `release-nodejs-libs`. Configure PyPI trusted publisher.

### Task 8.1: Add `publish-python` job to `build.yml`

**Files:**
- Modify: `.github/workflows/build.yml` (append new job after `release-nodejs-libs:`)

- [ ] **Step 1: Append the job**

After line ~803 (end of `release-nodejs-libs`), add:

```yaml
# =========================
#   Python package release
# =========================

# Publishes simplex-chat to PyPI on release tags.
# Depends on release-nodejs-libs because the Python package downloads
# its libsimplex from the simplex-chat-libs release at runtime, so the
# libs release must exist before the Python package is published.
#
# Trusted publishing is configured on PyPI: no API token, OIDC only.

  publish-python:
    runs-on: ubuntu-latest
    needs: [release-nodejs-libs]
    if: startsWith(github.ref, 'refs/tags/v')
    permissions:
      id-token: write     # OIDC for trusted publishing
    steps:
      - uses: actions/checkout@v6
      - uses: actions/setup-python@v5
        with:
          python-version: "3.11"
      - run: pip install build
      - run: python -m build --wheel
        working-directory: packages/simplex-chat-python
      - uses: pypa/gh-action-pypi-publish@release/v1
        with:
          packages-dir: packages/simplex-chat-python/dist
```

- [ ] **Step 2: Validate YAML locally**

```
python -c "import yaml; yaml.safe_load(open('.github/workflows/build.yml'))"
```

Expected: no exception.

- [ ] **Step 3: Commit**

```bash
git add .github/workflows/build.yml
git commit -m "ci: add publish-python job for PyPI release on tag"
```

### Task 8.2: One-time PyPI setup (manual, document in README)

- [ ] **Step 1: Verify package name `simplex-chat` is available on PyPI**

```bash
curl -s https://pypi.org/pypi/simplex-chat/json | head -c 200
```

If it 404s, the name is free. If it returns metadata, the name is taken — coordinate with the team.

- [ ] **Step 2: On PyPI, create a pending publisher**

Navigate to https://pypi.org/manage/account/publishing/ and add:

| Field | Value |
|---|---|
| PyPI Project Name | simplex-chat |
| Owner | simplex-chat |
| Repository name | simplex-chat |
| Workflow name | build.yml |
| Environment name | (leave blank) |

- [ ] **Step 3: Add a section to `packages/simplex-chat-python/README.md` documenting the release process**

Brief checklist for the maintainer:
1. Bump `_version.py` `__version__` (and `LIBS_VERSION` if libs changed).
2. Tag with `vX.Y.Z` matching `__version__`.
3. Push the tag → CI runs the existing build matrix, then `release-nodejs-libs`, then `publish-python`.
4. Verify the wheel appears at https://pypi.org/project/simplex-chat/.

- [ ] **Step 4: Commit doc update**

```bash
git add packages/simplex-chat-python/README.md
git commit -m "docs(python): release process + PyPI trusted publisher setup"
```

---

## Final acceptance

After all phases:

- [ ] **Type generation parity.** `cabal test simplex-chat-test` passes for all four `Python` test cases.
- [ ] **Python package builds.** `cd packages/simplex-chat-python && python -m build --wheel` produces a single `.whl` ≤ 200 KB.
- [ ] **All Python tests pass.** `pytest packages/simplex-chat-python/tests` — green on Linux + macOS.
- [ ] **Pyright clean.** `pyright packages/simplex-chat-python/src` — zero errors.
- [ ] **Squaring bot smoke.** Run `python examples/squaring_bot.py` against a fresh database; verify (a) lazy lib download succeeds, (b) `bot.run()` blocks, (c) Ctrl-C exits cleanly.
- [ ] **CI dry run.** Push a `v0.0.0-test` tag to a fork; verify the `publish-python` job runs after `release-nodejs-libs` and the wheel uploads to TestPyPI (configure a separate test publisher if doing this).
