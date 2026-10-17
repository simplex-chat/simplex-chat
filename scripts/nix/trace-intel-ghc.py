#!/usr/bin/env python3
"""Replay one observed Intel iOS compiler SIGBUS with an in-process recorder."""

import argparse
import json
import os
from pathlib import Path
import platform
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time


ATTRIBUTE = "hydraJobs.x86_64-darwin.x86_64-darwin-ios:lib:simplex-chat"
LOG_LIMIT = 8 * 1024 * 1024
NIX_HASH = r"[0123456789abcdfghijklmnpqrsvwxyz]{32}"


def select_failure(text):
    if re.search(r"Failed to build simplex-chat-[^\s]+\. The build process terminated with exit code -10\b", text) is None:
        return None
    component = rf"/nix/store/{NIX_HASH}-simplex-chat-lib-simplex-chat-[0-9][0-9.]*\.drv"
    matches = re.findall(rf"error: Cannot build '({component})'|builder for '({component})' failed", text)
    paths = {value for match in matches for value in match if value}
    if len(paths) > 1:
        raise ValueError("Multiple SimpleX library components failed")
    return next(iter(paths), None)


def nix_string(value):
    return '"' + str(value).replace("\\", "\\\\").replace('"', '\\"').replace("${", "\\${") + '"'


def command_status(arguments, seconds, **streams):
    # A fresh session gives this command and its descendants a group owned by
    # this launcher. macOS need not provide GNU timeout (or a Homebrew alias).
    child = subprocess.Popen(list(map(str, arguments)), start_new_session=True, **streams)
    try:
        return child.wait(timeout=seconds)
    except subprocess.TimeoutExpired:
        # wait() has not reaped this PID, so its group cannot have been reused.
        # Kill the whole owned group, including children that ignore SIGTERM.
        try:
            os.killpg(child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        try:
            child.wait(timeout=5)
        except subprocess.TimeoutExpired as error:
            raise TimeoutError("Owned diagnostic PID " + str(child.pid)
                               + " was not reaped within five seconds after SIGKILL") from error
        raise TimeoutError("Diagnostic command reached its wall-clock limit") from None


def run_step(arguments, deadline, seconds, capture=False):
    remaining = min(seconds, int(deadline - time.monotonic()))
    if remaining < 1:
        raise TimeoutError("Diagnostic wall-clock limit reached")
    if capture:
        # Keep subprocess output out of memory until its size is known. These
        # three metadata queries have tiny successful outputs; errors remain
        # visible through a bounded stderr suffix.
        with tempfile.TemporaryFile() as output, tempfile.TemporaryFile() as errors:
            status = command_status(arguments, remaining, stdout=output, stderr=errors)
            if status:
                errors.seek(0, os.SEEK_END)
                errors.seek(max(0, errors.tell() - 4096))
                print(errors.read(4096).decode(errors="replace"), file=sys.stderr, flush=True)
                raise RuntimeError("Diagnostic command returned " + str(status))
            output.seek(0)
            data = output.read(4097)
            if len(data) > 4096:
                raise ValueError("Diagnostic command exceeded the metadata output limit")
            return data.decode().strip()
    status = command_status(arguments, remaining)
    if status:
        raise RuntimeError("Diagnostic command returned " + str(status))
    return None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, required=True)
    parser.add_argument("--attribute", required=True)
    parser.add_argument("--log", type=Path, required=True)
    args = parser.parse_args()
    # This remains separate from the canonical build. No ordinary local build,
    # native component, different platform or profiling trial is replayed.
    if (os.environ.get("GITHUB_ACTIONS") != "true" or platform.system() != "Darwin"
            or platform.machine() != "x86_64" or args.attribute != ATTRIBUTE):
        print("Skipping Intel GHC diagnostic: outside the observed CI target.", flush=True)
        return 0
    try:
        deadline = time.monotonic() + 360
        with args.log.open("rb") as source:
            source.seek(0, os.SEEK_END)
            source.seek(max(0, source.tell() - LOG_LIMIT))
            text = source.read(LOG_LIMIT).decode(errors="replace")
        expected = select_failure(text)
        if expected is None:
            print("Skipping Intel GHC diagnostic: no unique observed SimpleX SIGBUS component.", flush=True)
            return 0
        missing = [tool for tool in ("xcrun", "git", "nix") if not shutil.which(tool)]
        if missing:
            raise ValueError("Required existing diagnostic tools unavailable: " + ", ".join(missing))
        repo = args.repo.resolve()
        revision = run_step(["git", "-C", repo, "rev-parse", "HEAD"], deadline, 10, True)
        if re.fullmatch(r"[0-9a-f]{40}", revision) is None:
            raise ValueError("Invalid checkout revision")
        compiler = run_step(["xcrun", "--find", "clang"], deadline, 10, True)
        sdk = run_step(["xcrun", "--sdk", "macosx", "--show-sdk-path"], deadline, 10, True)
        if not Path(compiler).is_absolute() or not Path(sdk).is_absolute():
            raise ValueError("Expected absolute existing Apple tool paths")
        scripts = repo / "scripts/nix"
        with tempfile.TemporaryDirectory(prefix="simplex-intel-ghc-") as folder:
            tracer = Path(folder) / "ghc-sigbus-recorder.dylib"
            run_step([compiler, "-isysroot", sdk, "-arch", "x86_64", "-std=c11", "-Werror=vla",
                      "-pedantic", "-Wall", "-Wextra", "-Werror", "-O0", "-g", "-dynamiclib",
                      scripts / "trace_ghc_sigbus.c", "-o", tracer], deadline, 30)
            # Pin evaluation to this checkout. The component's exact drvPath is
            # checked again by the Nix adapter before any diagnostic realization.
            flake = "builtins.getFlake " + nix_string("git+" + repo.as_uri() + "?rev=" + revision)
            original = ("let f = " + flake + "; in f.hydraJobs.x86_64-darwin."
                        + nix_string("x86_64-darwin-ios:lib:simplex-chat"))
            python = ("let f = " + flake + "; in (import "
                      + nix_string(scripts / "native-packages.nix")
                      + ' { haskellNix = f.inputs.haskellNix; } "x86_64-darwin").python3')
            # Python is the only added producer input. The observed failed
            # component already realized its other inputs. Obtain Python from
            # the local store or substitutes, and skip rather than compile it.
            run_step(["nix", "build", "--expr", python, "--no-link", "--max-jobs", "0",
                      "--builders", "", "--option", "external-builders", "[]",
                      "--timeout", "30"], deadline, 30)
            print("Intel GHC diagnostic: one exact-component replay; original failure remains authoritative.", flush=True)
            print(json.dumps({"expectedDrv": expected, "checkout": revision,
                              "perDerivationSeconds": 300, "overallSeconds": 360,
                              "sandboxOverride": False}), flush=True)
            run_step(["nix", "build", "--file", scripts / "trace-intel-ghc.nix",
                      "--arg", "original", original, "--arg", "python", python,
                      "--argstr", "expectedDrv", expected, "--argstr", "tracerPath", tracer,
                      "--no-link", "-L", "--keep-failed", "--max-jobs", "1", "--timeout", "300"],
                     deadline, 300)
    except (OSError, ValueError, RuntimeError, TimeoutError) as error:
        # The build driver always returns its saved canonical status. A failed
        # or skipped diagnostic must never turn that failure into success.
        print("Intel GHC diagnostic ended: " + str(error), file=sys.stderr, flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
