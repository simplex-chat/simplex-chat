#!/usr/bin/env python3
"""Exercise the shared packaging shell with real PE imports, without executing PE code."""
import argparse
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time
import zipfile

ROOT = Path(__file__).resolve().parent


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cc', required=True, help='cached MinGW C compiler')
    parser.add_argument('--baseline-flake', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    deadline = time.monotonic() + 120
    commands = []

    def run(argv, *, cwd=None, env=None):
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise RuntimeError('Packaging check reached its 120-second deadline')
        result = subprocess.run(argv, cwd=cwd, env=env, capture_output=True,
                                text=True, timeout=min(remaining, 30))
        commands.append({'argv': list(map(str, argv)), 'returncode': result.returncode,
                         'stdout': result.stdout, 'stderr': result.stderr})
        (args.output / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if result.returncode:
            raise RuntimeError(f'Command failed ({result.returncode}): {argv[0]}: {result.stderr}')
        return result.stdout

    with tempfile.TemporaryDirectory(prefix='windows-packaging-') as temporary:
        fixture = Path(temporary)
        tools = fixture / 'tools'
        (tools / 'bin').mkdir(parents=True)
        for name in ['strings', 'zip']:
            target = shutil.which(name)
            if not target:
                raise RuntimeError(f'Missing cached host tool: {name}')
            (tools / 'bin' / name).symlink_to(target)
        tree = tools / 'bin' / 'tree'
        tree.write_text('#!/bin/sh\nexit 0\n')
        tree.chmod(0o755)
        providers = {}
        for name in ['openssl', 'zlib', 'libffi', 'gmp', 'gcc', 'mcfgthreads']:
            providers[name] = str(fixture / name)
            (fixture / name).mkdir()
        # Integer-only C fixtures establish a direct and a transitive DLL import.
        # They implement no cryptography and are never executed.
        sources = {
            'leaf.c': 'int fixture_leaf(void) { return 17; }\n',
            'first.c': 'extern int fixture_leaf(void);\nint fixture_first(void) { return fixture_leaf(); }\n',
            'root.c': 'extern int fixture_first(void);\nint fixture_root(void) { return fixture_first(); }\n',
            'main.c': 'extern int fixture_first(void);\nint fixture_entry(void) { return fixture_first(); }\n',
        }
        for name, contents in sources.items():
            (fixture / name).write_text(contents)
        flags = ['-std=c11', '-Werror=vla', '-pedantic', '-Wall', '-Wextra', '-nostdlib']
        leaf = fixture / 'zlib' / 'fixture_leaf.dll'
        first = fixture / 'openssl' / 'fixture_first.dll'
        run([args.cc, *flags, '-shared', 'leaf.c', '-Wl,--entry,0',
             '-Wl,--out-implib,leaf.a', '-o', str(leaf)], cwd=fixture)
        run([args.cc, *flags, '-shared', 'first.c', 'leaf.a', '-Wl,--entry,0',
             '-Wl,--out-implib,first.a', '-o', str(first)], cwd=fixture)
        run([args.cc, *flags, '-shared', 'root.c', 'first.a', '-Wl,--entry,0',
             '-Wl,--out-implib,libsimplex.dll.a', '-o', 'libsimplex.dll'], cwd=fixture)
        run([args.cc, *flags, 'main.c', 'first.a', '-Wl,--entry,fixture_entry',
             '-o', 'simplex-chat.exe'], cwd=fixture)
        # A duplicated read-only provider must keep the first collected bytes.
        duplicate = fixture / 'gcc' / first.name
        shutil.copyfile(first, duplicate)
        duplicate.chmod(0o444)
        windows = {name: providers[name] for name in ['openssl', 'zlib', 'libffi', 'gmp']}
        windows.update({'stdenv': {'cc': {'cc': providers['gcc']}},
                        'windows': {'mcfgthreads': providers['mcfgthreads']}})
        data = {'pkgs': {'binutils': str(tools), 'tree': str(tools), 'zip': str(tools)},
                'windowsPkgs': windows}
        data_path = fixture / 'data.json'
        data_path.write_text(json.dumps(data))
        baseline = args.baseline_flake.read_text() if args.baseline_flake else None
        flake = (ROOT.parent.parent / 'flake.nix').read_text()
        calls = re.findall(r'(?:installPhase = old\.installPhase \+ |postInstall = )import \./scripts/nix/package-windows\.nix (\{.*?\n\s*\});', flake, re.S)
        assert len(calls) == 2, 'Expected exactly the two Windows packaging callers'
        results = []
        for kind in ['cli', 'library']:
            entry = 'simplex-chat.exe' if kind == 'cli' else 'libsimplex.dll'
            expected = {entry, 'fixture_first.dll', 'fixture_leaf.dll'}
            if kind == 'library':
                expected.add('libsimplex.dll.a')
            expression = fixture / 'render.nix'
            arguments = calls[0 if kind == 'cli' else 1]
            expression.write_text(f'''let
  data = builtins.fromJSON (builtins.readFile {json.dumps(str(data_path))});
  pkgs = data.pkgs // {{
    lib.getBin = value: value;
    lib.escapeShellArg = builtins.toJSON;
    pkgsCross.mingwW64 = data.windowsPkgs // {{ hostPlatform.system = "fixture-windows"; }};
  }};
in import {ROOT / 'package-windows.nix'} {arguments}
''')
            script = run(['nix', 'eval', '--offline', '--raw', '--file', str(expression)])
            variants = [('fixed', script)]
            if baseline:
                if kind == 'cli':
                    match = re.search(r'installPhase = old\.installPhase \+ \'\'(.*?)\n\s*\'\';', baseline, re.S)
                else:
                    part = baseline[baseline.index('kind = "windows";'):]
                    match = re.search(r'postInstall = \'\'(.*?)\n\s*\'\';', part, re.S)
                if not match:
                    raise RuntimeError('Cannot find exact baseline Windows packaging phase')
                body = match.group(1)
                body = re.sub(r'\$\{pkgs\.tree\}', str(tools), body)
                body = re.sub(r'\$\{pkgs\.zip\}', str(tools), body)
                body = re.sub(r'\$\{pkgs\.binutils\}', str(tools), body)
                names = {'openssl': 'openssl', 'libffi': 'libffi', 'gmp': 'gmp',
                         'stdenv.cc.cc': 'gcc', 'windows.mcfgthreads': 'mcfgthreads'}
                for nix_name, name in names.items():
                    body = body.replace('${pkgs.lib.getBin pkgs.pkgsCross.mingwW64.' + nix_name + '}', providers[name])
                body = body.replace('${pkgs.pkgsCross.mingwW64.hostPlatform.system}', 'fixture-windows')
                assert '${' not in body, 'Unresolved baseline Nix interpolation'
                variants.append(('original', body))
            for variant, shell in variants:
                out = fixture / (kind + '-' + variant)
                (out / 'bin').mkdir(parents=True)
                shutil.copyfile(fixture / 'simplex-chat.exe', out / 'bin' / 'simplex-chat.exe')
                env = dict(os.environ, out=str(out))
                run(['/bin/bash', '-e', '-c', shell], cwd=fixture, env=env)
                archive_path = next(out.glob('*.zip'))
                with zipfile.ZipFile(archive_path) as archive:
                    assert archive.testzip() is None
                    actual = set(archive.namelist())
                if variant == 'fixed':
                    assert actual == expected, (kind, actual, expected)
                else:
                    assert expected - actual, 'Original unexpectedly includes the DLL closure'
                results.append({'kind': kind, 'variant': variant, 'members': sorted(actual),
                                'missingExpectedDLLs': sorted(expected - actual)})
        (args.output / 'result.json').write_text(json.dumps({'checks': results,
                'realPEInputs': True, 'targetExecuted': False}, indent=2) + '\n')
        print(json.dumps(results, indent=2))
    (args.output / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')


if __name__ == '__main__':
    main()
