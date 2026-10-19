#!/usr/bin/env python3
"""Build the canonical Windows TH component before the full Windows outputs.

Only its direct derivation inputs are prepared under the normal build budget.
The component then has a short budget; its failure remains a CI failure even
when the separate strace diagnostic succeeds.
"""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

STORE = "/nix/store/"
DRV_NAME = re.compile(r"[0-9abcdfghijklmnpqrsvwxyz]{32}-[A-Za-z0-9+._?=-]+\.drv\Z")
OUTPUT = re.compile(r"[A-Za-z_][A-Za-z0-9_+-]*\Z")
COMPONENT = "wai-app-static-lib-wai-app-static-x86_64-w64-mingw32-3.1.9.1"


def drv_path(value, *, basename=False):
    if not isinstance(value, str):
        raise ValueError("derivation path must be a string")
    name = value[len(STORE):] if value.startswith(STORE) else value if basename else ""
    if not DRV_NAME.fullmatch(name):
        raise ValueError(f"invalid derivation path: {value!r}")
    return STORE + name


def direct_inputs(document, expected):
    """Return explicit drv^output goals from supported Nix JSON formats."""
    expected = drv_path(expected)
    if not isinstance(document, dict):
        raise ValueError("derivation JSON must be an object")
    if "derivations" in document or "version" in document:
        if (set(document) != {"derivations", "version"} or
                type(document["version"]) is not int or document["version"] != 4):
            raise ValueError("unsupported derivation JSON version")
        rows = document["derivations"]
        modern = True
    else:
        rows = document
        modern = False
    if not isinstance(rows, dict) or len(rows) != 1:
        raise ValueError("expected exactly the selected component derivation")
    key, row = next(iter(rows.items()))
    if drv_path(key, basename=modern) != expected or not isinstance(row, dict):
        raise ValueError("selected component derivation does not match")
    if (row.get("system") != "x86_64-linux" or not isinstance(row.get("env"), dict) or
            row["env"].get("name") != COMPONENT):
        raise ValueError("expected the native-Linux-built Windows wai-app-static component")
    if not isinstance(row.get("outputs"), dict) or set(row["outputs"]) != {"out"}:
        raise ValueError("expected the component's single out output")
    if modern:
        if (type(row.get("version")) is not int or row["version"] != 4 or
                not isinstance(row.get("inputs"), dict) or set(row["inputs"]) != {"drvs", "srcs"} or
                not isinstance(row["inputs"]["srcs"], list)):
            raise ValueError("unsupported derivation input schema")
        inputs = row["inputs"]["drvs"]
    else:
        if "inputs" in row or "version" in row or not isinstance(row.get("inputDrvs"), dict):
            raise ValueError("unsupported legacy derivation input schema")
        inputs = row["inputDrvs"]
    if not isinstance(inputs, dict):
        raise ValueError("derivation inputs must be an object")
    goals = []
    seen = set()
    for key, value in sorted(inputs.items()):
        path = drv_path(key, basename=modern)
        if path == expected or path in seen:
            raise ValueError("duplicate or self-referencing input derivation")
        seen.add(path)
        if modern:
            if not isinstance(value, dict) or set(value) != {"outputs", "dynamicOutputs"}:
                raise ValueError("unsupported input derivation schema")
            if value["dynamicOutputs"] != {}:
                raise ValueError("dynamic derivation outputs are not supported")
            outputs = value["outputs"]
        else:
            outputs = value
        if (not isinstance(outputs, list) or not outputs or
                any(not isinstance(o, str) or not OUTPUT.fullmatch(o) for o in outputs) or
                len(set(outputs)) != len(outputs)):
            raise ValueError("expected explicit, unique named input outputs")
        goals.append(path + "^" + ",".join(outputs))
    return goals


def call(argv, *, capture=False, timeout=600):
    print("+ " + " ".join(argv), flush=True)
    result = subprocess.run(argv, text=True, stdout=subprocess.PIPE if capture else None,
                            check=True, timeout=timeout)
    return result.stdout if capture else None


def flake_context():
    root = Path(call(["git", "rev-parse", "--show-toplevel"], capture=True).strip())
    revision = call(["git", "rev-parse", "HEAD"], capture=True).strip()
    if not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise ValueError("expected a committed checkout")
    ref = root.as_uri().replace("file:", "git+file:", 1) + "?rev=" + revision
    original = (f'(builtins.getFlake {json.dumps(ref)}).hydraJobs.x86_64-linux.'
                '"x86_64-windows:exe:simplex-chat".project.hsPkgs.wai-app-static.components.library')
    return revision, ref, original


def read_state(path):
    state = json.loads(path.read_text())
    revision, ref, original = flake_context()
    if (state["revision"], state["flakeRef"], state["original"]) != (revision, ref, original):
        raise ValueError("gate state belongs to a different checkout")
    drv_path(state["drv"])
    if direct_inputs(state["derivation"], state["drv"]) != state["inputGoals"]:
        raise ValueError("gate input goals do not match the saved derivation")
    return state


def prepare(path):
    # Evaluation can realize planning/compiler IFDs. Give those the same normal
    # budget as the direct inputs; only the ready component gets 300 seconds.
    deadline = time.monotonic() + 14400

    def remaining():
        seconds = deadline - time.monotonic()
        if seconds <= 0:
            raise subprocess.TimeoutExpired("Windows TH input preparation", 14400)
        return seconds

    revision, ref, original = flake_context()
    # A patched C runtime can be cold. Prepare only the exact C payload and Wine
    # first; the filesystem execution still gets its short limit. Both stages
    # precede Haskell planning and share the existing preparation deadline.
    call(["nix", "build", ref + "#checks.x86_64-linux.windows-rts-fs.inputs", "--no-link",
          "-L", "--keep-failed", "--max-jobs", "1", "--timeout", "7200"],
         timeout=remaining())
    call(["nix", "build", ref + "#checks.x86_64-linux.windows-rts-fs", "--no-link",
          "-L", "--keep-failed", "--max-jobs", "1", "--timeout", "300"],
         timeout=min(600, remaining()))
    drv = call(["nix", "eval", "--impure", "--raw", "--expr", f"({original}).drvPath",
                "-L", "--keep-failed", "--max-jobs", "1", "--timeout", "7200"],
               capture=True, timeout=remaining()).strip()
    drv_path(drv)
    document = json.loads(call(["nix", "derivation", "show", drv], capture=True,
                               timeout=min(600, remaining())))
    goals = direct_inputs(document, drv)
    state = dict(revision=revision, flakeRef=ref, original=original, drv=drv,
                 derivation=document, inputGoals=goals, inputsReady=False)
    path.write_text(json.dumps(state, indent=2) + "\n")
    # No bare derivations or ^*: preserve only outputs actually requested by
    # this canonical component. Empty goals must never build the default flake.
    if goals:
        call(["nix", "build", *goals, "--no-link", "-L", "--keep-failed",
              "--max-jobs", "1", "--timeout", "7200"], timeout=remaining())
    state["inputsReady"] = True
    path.write_text(json.dumps(state, indent=2) + "\n")


def build(path):
    state = read_state(path)
    if state.get("inputsReady") is not True:
        raise ValueError("canonical component inputs are not ready")
    if output := os.environ.get("GITHUB_OUTPUT"):
        with open(output, "a") as handle:
            handle.write("attempted=true\n")
    state["canonicalResult"] = "running"
    path.write_text(json.dumps(state, indent=2) + "\n")
    try:
        call(["nix", "build", state["drv"] + "^out", "--no-link", "-L", "--keep-failed",
              "--max-jobs", "1", "--timeout", "300"], timeout=330)
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        state["canonicalResult"] = "failure"
        path.write_text(json.dumps(state, indent=2) + "\n")
        raise
    state["canonicalResult"] = "success"
    path.write_text(json.dumps(state, indent=2) + "\n")


def trace(path):
    state = read_state(path)
    if state.get("inputsReady") is not True:
        raise ValueError("canonical component inputs are not ready")
    if state.get("canonicalResult") != "failure":
        raise ValueError("diagnostic requires a failed canonical component attempt")
    tool = f'(import ((builtins.getFlake {json.dumps(state["flakeRef"])}).inputs.nixpkgs) {{ system = "x86_64-linux"; }}).strace'
    # A cache miss must not turn the diagnostic into a toolchain build.
    call(["nix", "build", "--impure", "--expr", tool, "--no-link", "--max-jobs", "0",
          "--builders", "", "--option", "external-builders", "[]"], timeout=120)
    call(["nix", "build", "--impure", "--file", "scripts/nix/trace-windows-th.nix",
          "--arg", "original", state["original"], "--arg", "strace", tool,
          "--argstr", "expectedDrv", state["drv"], "--no-link", "-L", "--keep-failed",
          "--max-jobs", "1", "--timeout", "300"], timeout=600)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=["prepare", "build", "trace"])
    parser.add_argument("--state", type=Path, default=Path("windows-th-gate.json"))
    args = parser.parse_args()
    try:
        {"prepare": prepare, "build": build, "trace": trace}[args.action](args.state)
    except (ValueError, KeyError, subprocess.CalledProcessError, subprocess.TimeoutExpired) as error:
        print(f"Windows TH {args.action} failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
