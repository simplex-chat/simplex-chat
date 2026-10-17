#!/usr/bin/env python3
"""Focused controls for input selection and the compile-only Windows trace."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("gate", HERE / "windows-th-gate.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)
SELECTED = "/nix/store/" + "0" * 32 + "-" + gate.COMPONENT + ".drv"
DEP = "/nix/store/" + "1" * 32 + "-dependency.drv"
NEEDLE = '    printf \'#!/bin/sh\\nexec %s "$@"\\n\' "$g" > "$wrapBin/$gName"'
WINEDEBUG = "WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag"


def store_iserv(temp, *, normal=None, profiled=None):
    """Import tiny script inputs, not a derivation or a compiler."""
    source = Path(temp) / "iserv-wrapper"
    (source / "bin").mkdir(parents=True)
    for suffix, content in [("", normal), ("-prof", profiled)]:
        path = source / ("bin/iserv-wrapper" + suffix)
        path.write_text(content if content is not None else "#!/bin/sh\n" + WINEDEBUG + " true \"$@\"\n")
        path.chmod(0o755)
    return subprocess.check_output(["nix-store", "--add", str(source)], text=True).strip() + "/bin/iserv-wrapper"


def document(modern=True):
    row = {"system": "x86_64-linux", "env": {"name": gate.COMPONENT},
           "outputs": {"out": {"path": "unused"}}}
    if modern:
        row.update(version=4, inputs={"srcs": [], "drvs": {
            Path(DEP).name: {"outputs": ["out", "dev"], "dynamicOutputs": {}}}})
        return {"version": 4, "derivations": {Path(SELECTED).name: row}}
    row["inputDrvs"] = {DEP: ["out", "dev"]}
    return {SELECTED: row}


class Inputs(unittest.TestCase):
    def test_fs_check_precedes_component_eval_and_input_build(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(gate, "flake_context", return_value=("a" * 40, "ref", "original")), patch.object(gate, "call", side_effect=[None, None, SELECTED, json.dumps(document()), None]) as call:
            path = Path(temp) / "state.json"
            gate.prepare(path)
            commands = [c.args[0] for c in call.call_args_list]
            self.assertEqual(commands[0], ["nix", "build", "ref#checks.x86_64-linux.windows-rts-fs.inputs",
                             "--no-link", "-L", "--keep-failed", "--max-jobs", "1", "--timeout", "7200"])
            self.assertEqual(commands[1], ["nix", "build", "ref#checks.x86_64-linux.windows-rts-fs",
                             "--no-link", "-L", "--keep-failed", "--max-jobs", "1", "--timeout", "300"])
            self.assertEqual(commands[2][:2], ["nix", "eval"])
            self.assertEqual(commands[3], ["nix", "derivation", "show", SELECTED])
            self.assertEqual(commands[4], ["nix", "build", DEP + "^out,dev", "--no-link",
                             "-L", "--keep-failed", "--max-jobs", "1", "--timeout", "7200"])
            self.assertTrue(json.loads(path.read_text())["inputsReady"])

    def test_failed_c_inputs_never_run_fs_or_evaluate_haskell(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(gate, "flake_context", return_value=("a" * 40, "ref", "original")), patch.object(gate, "call", side_effect=subprocess.CalledProcessError(1, "C inputs")) as call:
            path = Path(temp) / "state.json"
            with self.assertRaises(subprocess.CalledProcessError):
                gate.prepare(path)
            self.assertEqual(call.call_count, 1)
            self.assertIn("ref#checks.x86_64-linux.windows-rts-fs.inputs", call.call_args.args[0])
            self.assertFalse(path.exists())

    def test_failed_fs_check_never_evaluates_or_builds_haskell_inputs(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(gate, "flake_context", return_value=("a" * 40, "ref", "original")), patch.object(gate, "call", side_effect=[None, subprocess.CalledProcessError(1, "filesystem check")]) as call:
            path = Path(temp) / "state.json"
            with self.assertRaises(subprocess.CalledProcessError):
                gate.prepare(path)
            self.assertEqual(call.call_count, 2)
            self.assertIn("ref#checks.x86_64-linux.windows-rts-fs", call.call_args.args[0])
            self.assertFalse(path.exists())

    def test_fs_check_consumes_existing_shared_preparation_deadline(self):
        now = [0]
        def run(argv, **_kwargs):
            if "ref#checks.x86_64-linux.windows-rts-fs.inputs" in argv:
                now[0] = 4000
                return None
            if "ref#checks.x86_64-linux.windows-rts-fs" in argv:
                now[0] = 4100
                return None
            if argv[:2] == ["nix", "eval"]:
                now[0] = 8100
                return SELECTED
            if argv[:3] == ["nix", "derivation", "show"]:
                now[0] = 8600
                return json.dumps(document())
            return None
        with tempfile.TemporaryDirectory() as temp, patch.object(gate, "flake_context", return_value=("a" * 40, "ref", "original")), patch.object(gate.time, "monotonic", side_effect=lambda: now[0]), patch.object(gate, "call", side_effect=run) as call:
            gate.prepare(Path(temp) / "state.json")
            self.assertEqual(call.call_args_list[0].kwargs["timeout"], 14400)
            self.assertEqual(call.call_args_list[1].kwargs["timeout"], 600)
            self.assertEqual(call.call_args_list[2].kwargs["timeout"], 10300)
            self.assertEqual(call.call_args_list[-1].kwargs["timeout"], 5800)

    def test_c_inputs_exhausting_deadline_never_run_runtime_check(self):
        now = [0]
        def finish_inputs(_argv, **_kwargs):
            now[0] = 14400
        with tempfile.TemporaryDirectory() as temp, patch.object(gate, "flake_context", return_value=("a" * 40, "ref", "original")), patch.object(gate.time, "monotonic", side_effect=lambda: now[0]), patch.object(gate, "call", side_effect=finish_inputs) as call:
            path = Path(temp) / "state.json"
            with self.assertRaises(subprocess.TimeoutExpired):
                gate.prepare(path)
            self.assertEqual(call.call_count, 1)
            self.assertFalse(path.exists())

    def test_both_schemas_preserve_requested_outputs(self):
        for modern in [True, False]:
            with self.subTest(modern=modern):
                self.assertEqual(gate.direct_inputs(document(modern), SELECTED), [DEP + "^out,dev"])

    def test_rejects_unknown_dynamic_unsafe_or_wrong_component(self):
        cases = []
        def changed(edit):
            value = document()
            edit(value, next(iter(value["derivations"].values())))
            cases.append(value)
        changed(lambda d, r: d.update(version=5))
        changed(lambda d, r: r.update(version=3))
        changed(lambda d, r: r.update(system="aarch64-linux"))
        changed(lambda d, r: r["env"].update(name="another-component"))
        changed(lambda d, r: r["outputs"].update(dev={}))
        changed(lambda d, r: r["inputs"].update(future={}))
        changed(lambda d, r: r["inputs"]["drvs"][Path(DEP).name].update(dynamicOutputs={"out": {}}))
        changed(lambda d, r: r["inputs"]["drvs"][Path(DEP).name].update(future={}))
        for outputs in [[], ["out", "out"], ["*"], ["out,dev"], ["../bad"], [1]]:
            changed(lambda d, r, outputs=outputs: r["inputs"]["drvs"][Path(DEP).name].update(outputs=outputs))
        for key in ["../dependency.drv", "/tmp/" + Path(DEP).name, "bad.drv"]:
            changed(lambda d, r, key=key: r["inputs"].update(drvs={key: {"outputs": ["out"], "dynamicOutputs": {}}}))
        changed(lambda d, r: r["inputs"]["drvs"].update({DEP: {"outputs": ["out"], "dynamicOutputs": {}}}))
        changed(lambda d, r: r["inputs"]["drvs"].update({Path(SELECTED).name: {"outputs": ["out"], "dynamicOutputs": {}}}))
        cases += [{}, [], {"version": 4}, {"version": 4, "derivations": {}, "other": 1}]
        for index, value in enumerate(cases):
            with self.subTest(case=index), self.assertRaises(ValueError):
                gate.direct_inputs(value, SELECTED)
        legacy = document(False)
        row = legacy.pop(SELECTED)
        legacy[Path(SELECTED).name] = row
        with self.assertRaises(ValueError):
            gate.direct_inputs(legacy, SELECTED)

    def test_empty_input_set_never_builds_default_flake(self):
        value = document()
        next(iter(value["derivations"].values()))["inputs"]["drvs"] = {}
        with tempfile.TemporaryDirectory() as temp, patch.object(gate, "flake_context", return_value=("a" * 40, "ref", "original")), patch.object(gate, "call", side_effect=[None, None, SELECTED, json.dumps(value)]) as call:
            state = Path(temp) / "state.json"
            gate.prepare(state)
            self.assertEqual(call.call_count, 4)
            initial = call.call_args_list[2]
            self.assertEqual(initial.args[0][-4:], ["--max-jobs", "1", "--timeout", "7200"])
            self.assertGreater(initial.kwargs["timeout"], 14000)
            self.assertTrue(json.loads(state.read_text())["inputsReady"])

    def test_canonical_failure_is_saved_and_trace_requires_it(self):
        state = dict(revision="a" * 40, flakeRef="ref", original="original", drv=SELECTED,
                     derivation=document(), inputGoals=[DEP + "^out,dev"], inputsReady=True)
        with tempfile.TemporaryDirectory() as temp, patch.object(gate, "flake_context", return_value=("a" * 40, "ref", "original")):
            path = Path(temp) / "state.json"
            path.write_text(json.dumps(state))
            with self.assertRaises(ValueError):
                gate.trace(path)
            with patch.object(gate, "call", side_effect=subprocess.CalledProcessError(1, "nix")) as call:
                with self.assertRaises(subprocess.CalledProcessError):
                    gate.build(path)
                argv = call.call_args.args[0]
                self.assertIn(SELECTED + "^out", argv)
                self.assertNotIn("--cores", argv)
                self.assertEqual(argv[-2:], ["--timeout", "300"])
            self.assertEqual(json.loads(path.read_text())["canonicalResult"], "failure")
            with patch.object(gate, "call") as call:
                gate.trace(path)
                tool, replay = [c.args[0] for c in call.call_args_list]
                self.assertIn("--max-jobs", tool)
                self.assertEqual(tool[tool.index("--max-jobs") + 1], "0")
                self.assertEqual(tool[tool.index("--builders") + 1], "")
                self.assertIn("[]", tool)
                self.assertIn(SELECTED, replay)
                self.assertNotIn("--cores", replay)
            with patch.object(gate, "flake_context", return_value=("b" * 40, "ref", "original")), self.assertRaises(ValueError):
                gate.read_state(path)


class Wrapper(unittest.TestCase):
    def render(self, phase, temp, **overrides):
        path = Path(temp) / "phase.sh"
        path.write_text(phase)
        original = dict(system="x86_64-linux", name=gate.COMPONENT,
                        identifier={"name": "wai-app-static"}, drvPath=SELECTED)
        original.update(overrides)
        # Pure fake derivation uses the actual override callback and builder seam.
        attrs = " ".join(f"{k} = builtins.fromJSON {json.dumps(json.dumps(v))};" for k, v in original.items())
        expression = f'''let original = {{ {attrs}
          overrideAttrs = f: f {{ buildPhase = builtins.readFile {path}; nativeBuildInputs = [ "compiler" ]; }};
        }}; in import {HERE / 'trace-windows-th.nix'} {{ inherit original;
          strace = {json.dumps(str(Path(temp) / 'trace'))}; expectedDrv = {json.dumps(SELECTED)}; }}'''
        return subprocess.run(["nix", "eval", "--impure", "--json", "--expr", expression], capture_output=True, text=True)

    def test_unique_seam_and_component_guards(self):
        with tempfile.TemporaryDirectory() as temp:
            for phase in ["no seam", NEEDLE + "\n" + NEEDLE]:
                with self.subTest(phase=phase):
                    self.assertNotEqual(self.render(phase, temp).returncode, 0)
            for changes in [{"system": "aarch64-linux"}, {"name": "other"}, {"identifier": {"name": "other"}}, {"drvPath": DEP}]:
                with self.subTest(changes=changes):
                    self.assertNotEqual(self.render(NEEDLE, temp, **changes).returncode, 0)

    def test_actual_rendered_shell_preserves_arguments_and_status(self):
        with tempfile.TemporaryDirectory() as temp:
            temp = Path(temp)
            (temp / "trace/bin").mkdir(parents=True)
            (temp / "wrappers").mkdir()
            selected_iserv = store_iserv(temp)
            fake = '''#!/usr/bin/env python3
import json, os, sys
kind = os.path.basename(sys.argv[0])
with open(os.environ["CAPTURE"], "a") as f:
 f.write(json.dumps({"kind": kind, "args": sys.argv[1:], "iserv": os.environ.get("ISERV_ARGS")}) + "\\n")
if kind == "strace": os.execv(sys.argv[6], sys.argv[6:])
if kind == "timeout": os.execv(sys.argv[3], sys.argv[3:])
print("query-control")
sys.exit(int(os.environ.get("GHC_EXIT", "0")))
'''
            for name in ["ghc", "timeout", "trace/bin/strace"]:
                path = temp / name
                path.write_text(fake.replace("import json, os, sys", f"# -pgmi {selected_iserv}\nimport json, os, sys"))
                path.chmod(0o755)
            phase = f'g={temp}/ghc\nwrapBin={temp}/wrappers\ngName=ghc\n' + NEEDLE + '\nchmod +x "$wrapBin/$gName"\nexit 0\n'
            rendered = self.render(phase, temp)
            self.assertEqual(rendered.returncode, 0, rendered.stderr)
            result = json.loads(rendered.stdout)
            self.assertEqual(result["nativeBuildInputs"], ["compiler", str(temp / "trace")])
            phasepath = temp / "rendered.sh"
            phasepath.write_text(result["buildPhase"])
            subprocess.run(["bash", "-n", str(phasepath)], check=True)
            subprocess.run(["bash", str(phasepath)], check=True)
            wrapper = temp / "wrappers/ghc"
            subprocess.run(["sh", "-n", str(wrapper)], check=True)
            for args, compile_call in [(["--numeric-version"], False), (["@some response", "--info"], False), (["-M", "-c"], False), (["--make", "a b.hs", "-j4", "-j1"], True), (["-c", "quote'and\"space", "-pgmi", "/original/wine wrapper", "-opti", "unchanged"], True), (["@some response"], True), (["unrecognized"], False)]:
                for status in [0, 7, 139]:
                    capture = temp / "capture.jsonl"
                    capture.write_text("")
                    env = dict(os.environ, CAPTURE=str(capture), GHC_EXIT=str(status),
                               ISERV_ARGS="ambient", NIX_BUILD_TOP=str(temp),
                               PATH=str(temp) + os.pathsep + os.environ["PATH"])
                    actual = subprocess.run([str(wrapper), *args], env=env, capture_output=True, text=True)
                    self.assertEqual(actual.returncode, status)
                    records = [json.loads(line) for line in capture.read_text().splitlines()]
                    self.assertEqual([r["kind"] for r in records], ["timeout", "strace", "ghc"] if compile_call else ["ghc"])
                    ghc_args = records[-1]["args"]
                    if compile_call:
                        self.assertEqual(ghc_args[:-2], args)
                        self.assertEqual(ghc_args[-2], "-pgmi")
                        selected_debug = Path(ghc_args[-1])
                        self.assertEqual(selected_debug.name, "iserv-wrapper")
                        for suffix in ["", "-prof"]:
                            copied = Path(str(selected_debug) + suffix).read_text()
                            original = Path(selected_iserv + suffix).read_text()
                            self.assertEqual(copied.replace(WINEDEBUG + ",+seh,+loaddll", WINEDEBUG), original)
                    else:
                        self.assertEqual(ghc_args, args)
                    self.assertEqual(records[-1]["iserv"], "-v" if compile_call else "ambient")
                    self.assertEqual(actual.stdout, "query-control\n")
                    if compile_call:
                        self.assertEqual(records[1]["args"][:5], ["-f", "-tt", "-T", "-s", "80"])


class WineDebug(unittest.TestCase):
    def run_copy(self, ghc, destination):
        return subprocess.run(["sh", str(HERE / "windows-th-wine-debug.sh"), str(ghc), str(destination)],
                              capture_output=True, text=True)

    def test_copies_only_debug_value_for_both_interpreters(self):
        with tempfile.TemporaryDirectory() as temp:
            temp = Path(temp)
            script = """#!/bin/sh
unset configureFlags
REMOTE_ISERV=unchanged
WINEDLLOVERRIDES=unchanged WINEDEBUG_VALUE WINEPREFIX="$TMP" python3 -c 'import json,os,sys; print(json.dumps([os.environ["WINEDEBUG"],os.environ["WINEPREFIX"],sys.argv[1:]]))' "$@"
""".replace("WINEDEBUG_VALUE", WINEDEBUG)
            original = store_iserv(temp, normal=script, profiled=script.replace("unchanged", "profiled"))
            ghc = temp / "ghc"
            ghc.write_text("#!/bin/sh\n# -pgmi " + original + "\n")
            copied = self.run_copy(ghc, temp / "debug")
            self.assertEqual(copied.returncode, 0, copied.stderr)
            self.assertEqual(copied.stdout.strip(), str(temp / "debug/iserv-wrapper"))
            for suffix in ["", "-prof"]:
                target = Path(copied.stdout.strip() + suffix)
                subprocess.run(["sh", "-n", str(target)], check=True)
                self.assertEqual(target.read_text().replace(WINEDEBUG + ",+seh,+loaddll", WINEDEBUG),
                                 Path(original + suffix).read_text())
                env = dict(os.environ, TMP="same-prefix")
                result = subprocess.run([str(target), "a b", "quote'and\"", "-v"], env=env,
                                        capture_output=True, text=True, check=True)
                debug, prefix, args = json.loads(result.stdout)
                self.assertEqual(debug, WINEDEBUG.removeprefix("WINEDEBUG=") + ",+seh,+loaddll")
                self.assertEqual(prefix, "same-prefix")
                self.assertEqual(args, ["a b", "quote'and\"", "-v"])

    def test_rejects_changed_or_ambiguous_originals(self):
        for case in ["missing-path", "two-paths", "missing-assignment", "two-assignments", "missing-profile"]:
            with self.subTest(case=case), tempfile.TemporaryDirectory() as temp:
                temp = Path(temp)
                script = "#!/bin/sh\n" + WINEDEBUG + " true\n"
                if case == "missing-assignment": script = "#!/bin/sh\ntrue\n"
                if case == "two-assignments": script += WINEDEBUG + " true\n"
                selected = store_iserv(temp, normal=script)
                if case == "missing-profile":
                    # A valid store path without the paired script must fail,
                    # without changing any existing store object.
                    source = temp / "unpaired/iserv-wrapper/bin"
                    source.mkdir(parents=True)
                    path = source / "iserv-wrapper"
                    path.write_text(script)
                    path.chmod(0o755)
                    selected = subprocess.check_output(["nix-store", "--add", str(source.parent)], text=True).strip() + "/bin/iserv-wrapper"
                ghc = temp / "ghc"
                ghc.write_text("none" if case == "missing-path" else selected + ("\n" + selected if case == "two-paths" else ""))
                result = self.run_copy(ghc, temp / "debug")
                self.assertEqual(result.returncode, 1, result.stderr)


if __name__ == "__main__":
    unittest.main()
