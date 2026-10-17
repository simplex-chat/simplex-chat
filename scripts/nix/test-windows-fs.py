#!/usr/bin/env python3
"""Check the probe's scope and owned fixtures without executing Windows code."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent


class ProbeDefinition(unittest.TestCase):
    def test_build_phase_does_not_leak_nounset_into_fixup(self):
        text = (HERE / "check-windows-fs.nix").read_text()
        self.assertEqual(text.count("buildPhase = ''\n"), 1)
        phase = text.split("buildPhase = ''\n", 1)[1].split("\n    '';", 1)[0]
        phase = phase.replace("${source}", "/locked-source").replace(
            "${./windows-fs/Probe.c}", "/unused-probe.c")
        with tempfile.TemporaryDirectory() as temp:
            temp = Path(temp)
            for name, body in [("sha256sum", "cat >/dev/null"),
                               ("fake-cc", "touch probe.exe")]:
                path = temp / name
                path.write_text("#!/usr/bin/env bash\n" + body + "\n")
                path.chmod(0o755)
            # Standard fixup hooks read optional variables without nounset.
            suffix = '\n[ -f probe.exe ]\n[ -z "$exit_code" ]\n'
            env = dict(os.environ, CC=str(temp / "fake-cc"),
                       PATH=str(temp) + os.pathsep + os.environ["PATH"])
            passed = subprocess.run(["bash", "-c", "set -e\n" + phase + suffix],
                                    cwd=temp, env=env, capture_output=True, text=True, timeout=10)
            self.assertEqual(passed.returncode, 0, passed.stderr)
            # The same phase without its option-isolating subshell reproduces
            # the real packaging failure, independently of the C compiler.
            unscoped = phase.replace("      (\n", "", 1).rsplit("      )", 1)[0]
            failed = subprocess.run(["bash", "-c", "set -e\n" + unscoped + suffix],
                                    cwd=temp, env=env, capture_output=True, text=True, timeout=10)
            self.assertNotEqual(failed.returncode, 0)
            self.assertIn("exit_code: unbound variable", failed.stderr)

    def render(self, *, linux=True, x64=True, native=True):
        # Use throwing compiler-project attributes: this C check must not
        # accidentally grow a GHC planning/bootstrap dependency.
        expression = f'''
          let pkgs = {{
            stdenv = {{ hostPlatform = {{ isLinux = {str(linux).lower()}; isx86_64 = {str(x64).lower()}; config = "host"; }};
                         buildPlatform.config = "{'host' if native else 'other'}"; }};
            pkgsCross.mingwW64 = {{
              stdenv.mkDerivation = attrs:
                assert attrs.strictDeps;
                assert attrs.buildInputs == [ "/target-C-runtime" ];
                assert builtins.replaceStrings [ "-DFS_NAMESPACE=rts" ] [ "" ] attrs.buildPhase != attrs.buildPhase;
                assert builtins.replaceStrings [ "/locked-GHC-source/rts-fs/fs.c" ] [ "" ] attrs.buildPhase != attrs.buildPhase;
                attrs // {{ outPath = "/C-payload"; }};
              windows.mcfgthreads = "/target-C-runtime";
              haskell-nix = throw "No target GHC project permitted";
            }};
            haskell-nix = {{ sources.ghc914-sh = "/locked-GHC-source";
                            compiler = throw "No native GHC project permitted"; }};
            pkgsBuildBuild.wine64Packages.minimal = "/same-Wine";
            coreutils = "/coreutils";
            bash = "/bash";
            lib.optionalString = condition: value: if condition then value else "";
            lib.escapeShellArg = value: value;
            linkFarm = name: entries:
              assert map (entry: entry.name) entries == [ "payload" "wine" ];
              assert map (entry: toString entry.path) entries == [ "/C-payload" "/same-Wine" ];
              {{ outPath = "/C-and-Wine-inputs"; inherit name; }};
            runCommand = name: attrs: script: {{ inherit name attrs script; }};
          }}; in import {HERE / 'check-windows-fs.nix'} {{ inherit pkgs; }}
        '''
        return subprocess.run(["nix", "eval", "--offline", "--impure", "--json",
                               "--expr", expression], capture_output=True, text=True, timeout=20)

    def test_check_is_native_linux_x64_only_and_uses_no_ghc_project(self):
        result = self.render()
        self.assertEqual(result.returncode, 0, result.stderr)
        value = json.loads(result.stdout)
        self.assertIn("/same-Wine/bin/wine", value["script"])
        self.assertIn("/C-payload/bin/windows-fs-probe.exe", value["script"])
        self.assertIn("set -euo pipefail", value["script"])
        self.assertEqual(value["attrs"]["passthru"]["inputs"],
                         "/C-and-Wine-inputs")
        for kwargs in [{"linux": False}, {"x64": False}, {"native": False}]:
            with self.subTest(kwargs=kwargs):
                self.assertNotEqual(self.render(**kwargs).returncode, 0)

    def test_runner_creates_exact_owned_payloads_and_preserves_exit_status(self):
        # This is runner orchestration only. The fake process inspects files;
        # no Wine/PE/Win32 execution or filesystem-regression result is claimed.
        with tempfile.TemporaryDirectory() as temp:
            temp = Path(temp)
            wine = temp / "fake-wine"
            wine.write_text('''#!/usr/bin/env python3
import json,os,pathlib,sys
exe,strict,short,long,archive=sys.argv[1:]
assert strict=="--strict"
assert len(long)>260 and len(archive)==319
assert all(pathlib.Path(p).read_bytes()==bytes([1,0]) for p in [short,long,archive])
assert pathlib.Path.cwd().is_relative_to(pathlib.Path(os.environ["TMPDIR"]).resolve())
assert os.environ["WINEPREFIX"].startswith(os.environ["TMPDIR"])
pathlib.Path(os.environ["CAPTURE"]).write_text(json.dumps({"args":sys.argv[1:],"prefix":os.environ["WINEPREFIX"]}))
sys.exit(int(os.environ["PROBE_EXIT"]))
''')
            wine.chmod(0o755)
            server = temp / "fake-wineserver"
            server.write_text('''#!/usr/bin/env python3
import os,pathlib,sys
assert sys.argv[1:]==["-k"]
pathlib.Path(os.environ["CLEANUP"]).write_text(os.environ["WINEPREFIX"])
''')
            server.chmod(0o755)
            for status in [0, 1, 2, 7]:
                capture = temp / "capture.json"
                cleanup = temp / "cleanup.txt"
                env = dict(os.environ, TMPDIR=str(temp), CAPTURE=str(capture),
                           CLEANUP=str(cleanup), PROBE_EXIT=str(status))
                result = subprocess.run(["bash", str(HERE / "windows-fs/run.sh"),
                                         str(wine), str(server), "/unused/probe.exe"],
                                        env=env, capture_output=True, text=True, timeout=15)
                self.assertEqual(result.returncode, status, result.stderr)
                self.assertEqual(cleanup.read_text(), json.loads(capture.read_text())["prefix"])


if __name__ == "__main__":
    unittest.main()
