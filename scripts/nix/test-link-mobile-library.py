#!/usr/bin/env python3
"""Check mobile linker argv and, optionally, MinGW archive extraction.

The optional PE link control uses the pinned GHC initializer body with a tiny
fixture-only HsIface layout. It checks BFD archive extraction, not GHC ABI or
Windows execution.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest


SCRIPTS = Path(__file__).resolve().parent
ROOT_OPTION = "-optl-Wl,-u,init_ghc_hs_iface"


class MobileLinkArguments(unittest.TestCase):
    def invoke(self, kind, status=0):
        with tempfile.TemporaryDirectory(prefix="mobile-link-argv-") as directory:
            root = Path(directory)
            library = root / "lib"
            library.mkdir()
            (library / "libHSfixture.a").touch()
            compiler = root / "ghc"
            compiler.write_text(
                "#!/usr/bin/env python3\nimport json,os,sys\n"
                "if sys.argv[1:]==['--numeric-version']:\n"
                " print('9.14.1');sys.exit(0)\n"
                "with open(os.environ['ARGV_LOG'],'a') as f:\n"
                " f.write(json.dumps(sys.argv[1:])+'\\n')\n"
                "sys.exit(int(os.environ['COMPILER_STATUS']))\n"
            )
            compiler.chmod(0o755)
            package_tool = root / "ghc-pkg"
            package_tool.write_text(
                "#!/usr/bin/env python3\nimport os,sys\n"
                "field=sys.argv[-2]\n"
                "print({'id':'fixture-unit','hs-libraries':'HSfixture',"
                "'library-dirs':os.environ['FIXTURE_LIBRARY']}[field])\n"
            )
            package_tool.chmod(0o755)
            log = root / "argv.jsonl"
            database = root / "package.conf.d"
            database.mkdir()
            output = root / "bundle"
            environment = os.environ.copy()
            environment.pop("MOBILE_LIBRARY_PACKAGE_DBS", None)
            environment.update(ARGV_LOG=str(log), FIXTURE_LIBRARY=str(library),
                               COMPILER_STATUS=str(status))
            result = subprocess.run(
                ["bash", str(SCRIPTS / "link-mobile-library.sh"),
                 str(compiler), str(package_tool), str(database), "fixture",
                 kind, str(output), "-optl-example with spaces"],
                env=environment, capture_output=True, text=True, timeout=15,
            )
            argv = json.loads(log.read_text()) if log.exists() else None
            common = ["-v0", "-no-hs-main", "-hide-all-packages",
                      "-package-db", str(database)]
            return result, argv, common, str(library / "libHSfixture.a")

    def test_windows_roots_only_the_real_initializer(self):
        result, argv, common, _ = self.invoke("windows")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(argv, common + [
            "-shared", "-threaded", "-flink-rts", "-package-id", "fixture-unit",
            ROOT_OPTION, "-optl-example with spaces", "MobileBundle.hs",
            "-o", "libsimplex.dll",
        ])

    def test_android_argv_is_unchanged(self):
        result, argv, common, _ = self.invoke("android")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(argv, common + [
            "-shared", "-threaded", "-flink-rts", "-package-id", "fixture-unit",
            "-optl-example with spaces", "MobileBundle.hs", "-o", "libsimplex.so",
        ])

    def test_support_does_not_link_a_second_rts(self):
        result, argv, common, archive = self.invoke("android-support")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(argv, common + [
            "-shared", "-fno-link-rts", "-no-auto-link-packages",
            "-optl-example with spaces", archive, "MobileBundle.hs",
            "-o", "libsupport.so",
        ])

    def test_ios_argv_is_unchanged(self):
        result, argv, common, _ = self.invoke("ios")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(argv, common + [
            "-staticlib", "-threaded", "-package-id", "fixture-unit",
            "-optl-example with spaces", "MobileBundle.hs",
            "-o", "dist/libHSfixture-ghc9.14.1.a",
        ])

    def test_compiler_failure_is_preserved(self):
        result, _, _, _ = self.invoke("windows", status=17)
        self.assertEqual(result.returncode, 17)

    def test_unknown_kind_is_rejected(self):
        result, argv, _, _ = self.invoke("unknown")
        self.assertEqual(result.returncode, 2)
        self.assertIsNone(argv)


def archive_control(args):
    root = args.link_output.resolve()
    root.mkdir(parents=True, exist_ok=True)
    source = args.ghc_source / "libraries/ghc-internal/cbits/RtsIface.c"
    contents = source.read_text()
    signature = "void init_ghc_hs_iface(void)\n{"
    if contents.count(signature) != 1:
        raise RuntimeError("expected the one pinned GHC initializer definition")
    function = contents[contents.index(signature):]
    # Preserve the upstream copyright and exact initializer body. Only the
    # interface layout and RTS caller are miniature archive-order fixtures.
    copyright_header = contents[:contents.index('#include "Rts.h"')]
    (root / "Iface.h").write_text(
        "typedef struct { int marker; } HsIface;\n"
        "extern HsIface *ghc_hs_iface;\n"
        "void init_ghc_hs_iface(void);\nvoid hs_init_fixture(void);\n"
        "void mobile_probe(void);\n"
    )
    (root / "RtsIface.c").write_text(
        copyright_header + '#include <stddef.h>\n#include "Iface.h"\n'
        "static HsIface the_ghc_hs_iface = {1};\n" + function
    )
    (root / "RtsStartup.c").write_text(
        '#include <stddef.h>\n#include "Iface.h"\n'
        "HsIface *ghc_hs_iface = NULL;\n"
        "void hs_init_fixture(void) { init_ghc_hs_iface(); }\n"
    )
    (root / "Bundle.c").write_text(
        '#include "Iface.h"\n'
        "void mobile_probe(void) { hs_init_fixture(); }\n"
    )
    (root / "Bundle.def").write_text("EXPORTS\n  mobile_probe\n")
    commands = []
    deadline = time.monotonic() + 120

    def run(argv):
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("archive control reached its wall-clock limit")
        result = subprocess.run(argv, cwd=root, capture_output=True, text=True,
                                timeout=min(30, remaining))
        commands.append({"argv": argv, "returncode": result.returncode,
                         "output": result.stdout + result.stderr})
        (root / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        return result

    flags = ["-std=c11", "-Werror=vla", "-pedantic", "-Wall", "-Wextra", "-O0"]
    for name in ["RtsIface", "RtsStartup", "Bundle"]:
        run([args.cc, *flags, "-c", name + ".c", "-o", name + ".o"]).check_returncode()
    for name, member in [("ghc-internal", "RtsIface"), ("rts", "RtsStartup")]:
        run([args.ar, "rcs", "lib" + name + ".a", member + ".o"]).check_returncode()
    results = {}
    for variant in ["original", "prefix", "suffix", "missing-provider"]:
        root_flag = ["-Wl,-u,init_ghc_hs_iface"]
        argv = [args.cc, "-shared", "Bundle.o", "Bundle.def", *args.link_option]
        if variant == "prefix":
            argv += root_flag
        if variant != "missing-provider":
            argv += ["libghc-internal.a"]
        argv += ["librts.a"]
        if variant in ["suffix", "missing-provider"]:
            argv += root_flag
        argv += ["-Wl,-Map," + variant + ".map", "-o", variant + ".dll"]
        result = run(argv)
        results[variant] = result.returncode
        if variant in ["original", "missing-provider"]:
            if (result.returncode != 1 or "undefined reference" not in result.stderr
                    or "init_ghc_hs_iface" not in result.stderr):
                raise RuntimeError("expected the specific missing-initializer link failure")
        else:
            result.check_returncode()
            if "libghc-internal.a(RtsIface.o)" not in (root / (variant + ".map")).read_text():
                raise RuntimeError("link map did not extract the real initializer body")
    receipt = {"results": results, "source": str(source),
               "initializerSHA256": hashlib.sha256(function.encode()).hexdigest(),
               "qualification": "PE archive extraction only; no GHC ABI/runtime execution"}
    (root / "result.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(json.dumps(receipt))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cc", help="also run the real MinGW archive link control")
    parser.add_argument("--ar")
    parser.add_argument("--ghc-source", type=Path)
    parser.add_argument("--link-output", type=Path)
    parser.add_argument("--link-option", action="append", default=[])
    args = parser.parse_args()
    if args.cc and not all([args.ar, args.ghc_source, args.link_output]):
        parser.error("--cc also requires --ar, --ghc-source and --link-output")
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(MobileLinkArguments)
    if not unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful():
        return 1
    if args.cc:
        archive_control(args)
    return 0


if __name__ == "__main__":
    sys.exit(main())
