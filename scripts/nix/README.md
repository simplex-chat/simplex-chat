# Haskell builds with Nix

The compiler and all cross-compilers come from the haskell.nix input pinned in
`flake.lock`. The `ghc914-sh` compiler is the stable-haskell fork, not the stock
GHC 9.14 release from GHCup. Use the same source checkout and lock file for every
platform; Android ARMv7 does not require a separate branch or tag.
Package-specific GHC options retain the common `-j1` default, including Darwin's
`-DIOS` and Android's `-pie`; an explicit later parallelism option still overrides
that default. The pure `check-project-ghc-options.nix` regression evaluates the
pinned haskell.nix plan, package, and component modules without building a project.
Run it with:
`nix eval --impure --json --file scripts/nix/check-project-ghc-options.nix --apply 'test: let f = builtins.getFlake (toString ./.); in test { lib = f.inputs.nixpkgs.lib; haskellNix = f.inputs.haskellNix; }'`.
The pinned haskell.nix includes the merged ARM32 personality, v2 Cabal metadata,
Windows patch-selector, and i386 COMDAT fixes. The raw GHC source pin is unchanged;
changes to v2 build scripts can still change compiler and component derivations.

`compiler-selection.nix` and `compiler-cache-overlay.nix` match Hydra's compiler
build context, including auxiliary iserv projects, so the pinned compiler can use
its existing cache. Application dependencies keep the project's current packages.

On Linux x86_64, `native-rts-project.nix` patches the final compiler's RTS ELF
loader to honor section alignment even when its file offset is unaligned.
The stage2 RTS package patch preserves the configured source, stage1 compiler,
plan, and package IDs, but rebuilds the final compiler and its dependent slices.
Native, auxiliary, and cross compiler selection share this patched compiler.
The ELF fix does not patch target RTS sources or target iserv runtimes,
including musl.

The focused regression runs before the native x86_64 Linux CI build. To run it
from this checkout, use `nix build .#checks.x86_64-linux.ghc-elf-alignment -L`.
It loads the same 8192-byte C object with aligned and deliberately packed section
file offsets, requiring both to retain 64-byte memory alignment and their data.
It is a flake check, separate from the 21 existing Hydra artifact outputs.

On Intel Darwin, `darwin-info-table-project.nix` patches the GHC x86 backend in
both compiler stages. It marks info-table entry symbols as Mach-O alternate
entries, keeping each table and its code in the same linker subsection. The
observed Setup GC abort reads closure type zero because LLD inserts eight bytes
between a valid `FUN_STATIC` table and its entry. Relinking the actual cached
`directory` object reproduces that layout error; changing only alternate-entry
symbol flags preserves the table's original 24-byte distance and closure type 14.
The patched compiler rebuild and full Intel outputs still require CI validation.

Run `nix build .#checks.x86_64-darwin.ghc-macho-info-tables -L` to compile a Haskell
fixture with the selected compiler and check its table placement after a real
LLD link. CI runs this check after the Intel artifact builds. It checks object
metadata, table distances, and closure types without executing the fixture.

Intel Darwin projects also declare bitvec's Clang runtime dependency before
planning. GHC's object linker needs the selected C compiler's
`libclang_rt.osx.a` for `__builtin_cpu_supports`; its CPU model is otherwise
unresolved during Template Haskell. The SIMD implementation remains enabled.
The patch declares both ordinary and static C libraries for TH and rolled archives.
The patched local dependency keeps its tests and benchmarks disabled.
Its package-local Nix dependency map treats `clang_rt.osx` as provided by that
selected compiler resource directory; all other library mappings stay intact.
The resolver regression exercises the pinned HN plan/package/component modules:

```sh
nix eval --impure --json --file scripts/nix/check-darwin-bitvec-resolver.nix \
  --apply 'test: let f = builtins.getFlake (toString ./.); in test { lib = f.inputs.haskellNix.inputs.nixpkgs.lib; haskellNix = f.inputs.haskellNix; }'
```

CI checks this platform scope before the Intel builds with
`nix eval --json --file scripts/nix/check-darwin-bitvec-runtime.nix`.

Cross projects add `ghc-internal` to the Cabal dependencies of the pinned
`composition-1.0.2.2` source. This preserves its deliberate avoidance of `base`
while making GHC 9.14's wired-in types available in isolated target package
databases. The patch runs before planning and source packaging. Native builds
and the Mac-to-iOS conversion projects keep the original package metadata.

Musl projects declare OpenSSL in Cabal's `extra-libraries-static` field for
`direct-sqlcipher` and `simplexmq`. GHC's fully static link path reads that field,
so an ordinary `extra-libraries` entry alone does not supply `libcrypto`. These
source metadata patches run before planning and require Cabal specification 3.8.

Linux-built musl `ghc914-sh` iserv projects use a package-specific
`executable-static: False` policy before Cabal v2 planning. The build-time
interpreter needs musl's dynamic C loader for Template Haskell dependencies;
a fully static musl executable has a `dlopen` stub. Its Haskell library ways
and profiling selection stay unchanged, and the final CLI remains fully static.
An i386-only preplanning source patch omits the private `__aio_close` symbol
registration: shared musl exposes `close`, which calls its hidden AIO hook
internally, rather than exporting that implementation detail.
The scope check uses the real Nix module fixed point in the Linux static CI
group, including `pkgs` supplied through `_module.args`. Repeat it with
`nix eval --impure --json --file scripts/nix/check-musl-iserv-scope.nix --apply 'f: f { lib = (builtins.getFlake (toString ./.)).inputs.nixpkgs.lib; }'`.

The i386 musl target RTS also handles duplicate hidden global function
signatures at the start of a sole executable COMDAT member. A known function
size must cover the whole member. GCC's `__x86.get_pc_thunk.*` PIC helpers use
zero symbol size (unknown extent); that case permits only the signature plus
local untyped labels and section metadata in the member. Additional typed or
nonlocal definitions remain errors. Coalescing is checked against the current
symbol owner during discovery and loading; it
does not persist weak-symbol status or suppress ordinary strong conflicts.
Multi-member groups remain outside this patch's scope. The source patch applies
to application and interpreter projects, including profiling. Native compilers
and other cross targets keep their existing RTS sources.
This production patch now comes from haskell.nix's project module. The scope and
native runtime regressions consume that same pinned upstream module and patch.

Run the native x86_64 Linux regression with
`nix build .#checks.x86_64-linux.ghc-elf-comdat-functions --no-link -L --max-jobs 1 --timeout 7200`.
It links the same configured RTS sources with and without the patch, verifies
the replacement objects in both linker maps, checks relocations in both load
orders, and rejects strong conflicts and stale archive-owner decisions. Each
probe has a 15-second bound. Module scope is checked separately by
`scripts/nix/check-musl-rts-scope.nix` using the same invocation as the iserv
scope check above.

Windows patches Basement's `INVALID_HANDLE_VALUE` expression to `INT_PTR`, so
hsc2hs receives the signed pointer-sized integer expected by the Haskell API.
The generated constant converts through Haskell `Int` with `Prelude.fromIntegral`;
Basement's `RebindableSyntax` has no `HasNegation IntPtr` instance.
Windows also plans the already selected `crypton-x509-system-1.6.8` as a local
patched source, so its Cabal metadata names `crypt32` before source packaging.
This keeps planning and packaged Cabal metadata aligned, including the metadata
used for native package unit IDs.
The source producer applies the existing haskell.nix patch once; its later
package patch list is empty to prevent a second application.
For this Windows GHC 9.14 package, the Nix dependency resolver also recognizes
lowercase `crypt32` as an implicit MinGW import library, matching the existing
`Crypt32` mapping. Cabal keeps the lowercase library declaration and linker flag;
all other package/library mappings stay intact. The external platform/compiler
guard omits the package definition entirely from excluded projects, which may
not contain x509 in their keyed package schema. The focused resolver check uses
the real haskell.nix keyed plan and package modules without building a compiler:

```sh
nix eval --impure --json --file scripts/nix/check-windows-crypt32.nix \
  --apply 'test: let f = builtins.getFlake (toString ./.); in test { lib = f.inputs.haskellNix.inputs.nixpkgs.lib; haskellNix = f.inputs.haskellNix; }'
```

The Windows MQ source patch makes BLST's COFF assembly reference the platform
capability storage already defined by its C server. Redundant COMMON declarations
otherwise abort the GHC runtime linker when loading MQ for chat Template Haskell.
`nix build .#checks.x86_64-linux.windows-blst-linkage -L` compiles the actual C
server and original/patched assembly with the selected MinGW toolchain. It checks
storage ownership, unchanged assembly sections/relocations, and the final PE link.
This check executes neither Wine nor GHC; the full Windows build qualifies TH.

Linux-built Win64 `ghc914-sh` v2 projects patch only the RTS DLL search-path
caller, including normal and profiling interpreters. It resolves relative paths
once, retains successful namespace conversion for long paths, and uses the
resolved absolute path if conversion returns NULL. Invalid paths fail without
passing NULL to `AddDllDirectory`; its opaque cookie and Win32 errors survive
cleanup. Other filesystem helpers and native compilers keep their sources.
The native caller regression uses scripted APIs, not Wine or a target ABI test:
`python3 scripts/nix/test-windows-dll-search-path.py --rts-source /path/to/configured-ghc/rts`.
`scripts/nix/check-windows-rts-scope.nix` checks the real module fixed point using
the same pinned-lib invocation as the musl scope check above. Production Win64
RTS compilation and the canonical Wine TH gate remain runtime qualification.

Run these commands from the repository root:

```sh
make help
make nix-list                  # list this host's existing Hydra outputs
make nix-native                # native library and CLI
make nix-android               # ARM64 + ARMv7 libraries, on x86_64-linux
make nix-windows               # Windows library and CLI, on x86_64-linux
make nix-linux-static          # x86_64, i686, and ARM64 static Linux CLIs
make nix-build                 # all existing outputs for this build host
```

Each command builds this checkout, without cloning another branch. Output links
are placed under `result-nix/<build-system>/<package>`. ZIP filenames inside the
outputs retain the existing Hydra names. For example, the ARMv7 client library is
`result-nix/x86_64-linux/armv7a-android:lib:simplex-chat/pkg-armv7a-android-libsimplex.zip`.
Nix includes tracked working-tree changes; add new source files to Git before
building so they are included.

For a remote builder, select its build system explicitly. Nix must already have
that builder configured:

```sh
make nix-ios NIX_SYSTEM=aarch64-darwin
make nix-darwin-libs NIX_SYSTEM=x86_64-darwin
make nix-build NIX_SYSTEM=aarch64-linux
```

`nix-ios` produces the Haskell static-library ZIP. It does not run Xcode, sign an
iOS application, or upload to TestFlight. ARM64 device libraries and Intel
simulator libraries retain their separate outputs. The existing iOS preparation
scripts retag ARM64 archives with `mac2ios -s` when an Apple Silicon simulator is
needed.

The Cabal v2 builder stores ordinary package archives in its staged store.
`link-mobile-library.sh` relinks the selected package and its dependencies before
that store is pruned. iOS ZIPs contain both the plain `libHSsimplex-chat-*.a` and
the rolled `libHSsimplex-chat-*-ghc*.a` used by `update-pbxproj.sh`. Android and
Windows clients link the threaded RTS into `libsimplex.so` or `libsimplex.dll`;
Android's separate `libsupport.so` resolves RTS symbols from the client library.
The GHC 9.14 Windows DLL link explicitly roots `init_ghc_hs_iface` from the real
static `ghc-internal` archive, as GHC already does for executable links. Run
`python3 scripts/nix/test-link-mobile-library.py` for scoped argv/status checks.
Its optional `--cc`, `--ar`, `--ghc-source` and `--link-output` arguments also run
a MinGW archive-extraction control using the pinned initializer body; extra C
linker inputs can be passed with `--link-option`. This does not execute a PE or
qualify the complete GHC runtime ABI.
Android client export flags use the ordered `chat-exports.txt` manifest. The
desktop library scripts check that manifest and `libsimplex.dll.def` against the
Haskell exports before building; update the manifest when adding an export.
Run `scripts/desktop/test-exports.sh` to check all three guards without compiling.
ARM64 Android libraries retain the 16 KiB linker alignment. These steps run in
the derivation's build/install phases because v2 component overrides do not run
the older `postBuild` and `postInstall` hooks.

Mobile builds keep the RTS's small-address-space setting. On Darwin, the final
link uses ABI-checked replacement RTS registrations, plus a replacement `text`
on ARM64. These boot libraries and all ARM64 mobile app C/C++ code target the
Apple A7 instruction baseline before `mac2ios` retags the archives. These mobile replacements use the cached ARM64
Darwin compiler; the Intel compiler receives the info-table patch described above. Android applies the RTS source patch
when building its boot libraries. The Android overlay also supplies GNU iconv
headers and a static PIC archive to `ghc-internal`, including the separate
iserv projects. The `ghc-internal` system dependency propagates GNU iconv to
interpreter consumers as well. Both Android interpreters register the GNU iconv
function addresses for runtime object loading. The ARM64 table uses the selected
`iconv.h` to distinguish GNU and Bionic, preserving plain lookup names alongside
GNU names when GNU supplies the definitions. Actual API26/API35 C controls check
these names against the target archive and the `ghc-internal` consumer; the
Bionic API35 object stays byte-identical. ARM32 source is patched before Cabal
planning, removing false missing-function stubs that shadow real static Bionic
startup routines. Android links libffi through the RTS package dependency.
Android preplanning patches remove the separate `pthread` library declarations
from crypton and direct-sqlcipher. Bionic supplies these functions in `libc`;
the NDK has no `libpthread`. Other targets retain their original declarations,
and compiler `-pthread` flags and the threaded RTS remain enabled. The local
crypton override retains its dependency-only scope; it does not add the package's
tests or benchmarks as planning goals. Repeat the
scope regression with `nix eval --json --file scripts/nix/check-android-pthread-scope.nix`.
The minimum Android API stays at 26.
For Android below API28, SplitMix's initializer uses Bionic's existing
`arc4random_buf` rather than the newer `getentropy` API. The dependency patch
preserves the OS entropy source; API28 and later keep the original initializer.
The ARM32 interpreter registers the real `arc4random_buf` address for loaded
objects on that route; the ARM64 interpreter already contains that registration.
The ARM64 interpreter registers `getentropy` and `getrandom` only from API28,
matching the NDK headers and versioned shared-library exports. API26 builds omit
those entries; API35 retains them.
The interpreter source patch applies to both Android ABIs; only ARM32 replaces
the emulator. The Android CI group runs the pure overlay-scope regression with
`nix eval --json --file scripts/nix/check-android-iserv-scope.nix`.

Android ARM32's QEMU wrapper preserves the guest's `PER_LINUX32` execution
domain when an ARM64 host kernel rejects that domain. Upper personality flags
still go through the host, and queries and setters retain the previous guest
value. This QEMU patch applies only to the Android ARM32 emulator;
the other emulator targets stay unchanged. Android's libc initializes
this state again at startup after an exec.
The shared helper passes the actual Android guest platform to the pinned
haskell.nix selector, which applies the personality patch once. The explicit
local Linux scheduling opt-out remains restricted to supported ARM32 emulators.

A standalone C regression checks startup, full personality values, the host's
`UNAME26` effect, and independent thread/fork state. Run it from this checkout
with configured Linux builders; Nix supplies the pinned Android API26 NDK and
patched QEMU. The NDK compiles the tiny payload on x86_64 Linux; QEMU then runs
on the selected Linux architecture. For example, on an Apple Silicon Mac whose
Linux builder supports both x86_64 and ARM64:

```sh
nix build --impure --file scripts/nix/check-android-personality.nix \
  --argstr system aarch64-linux --max-jobs 1 --cores 1 --print-build-logs
```

Use `--argstr system x86_64-linux` for an x86_64 Linux builder. This optional
check does not add a Hydra output. It checks C startup and personality handling;
it does not qualify GHC/base under Rosetta, cross-TH, or Android device behavior.
The static C fixture supplies the ARM
[TLS alignment input](https://android.googlesource.com/platform/bionic/+/cc9b100%5E!/)
added to newer Android CRTs; its minimum API stays at 26. On 2026-10-15 this
regression passed on native ARM64 Linux, native x86_64 Linux, and x86_64 Linux
through Rosetta on an Apple Silicon Mac.

`NIX_ARGS` passes additional options to `nix build`, and `NIX_OUT_DIR` changes the
output-link directory. Each derivation has a two-hour timeout by default; set
`NIX_BUILD_TIMEOUT` in seconds to change it. For a wall-clock limit on a local
batch, use `timeout` from GNU coreutils to include environment setup in the limit:

```sh
timeout 6h nix develop -c make nix-build
make nix-native NIX_ARGS=--dry-run
```

`.github/workflows/nix.yml` runs these same outputs on Linux ARM64/x86_64 and
Darwin ARM64/x86_64, with a six-hour limit per build job. Its seven build jobs cover all 21
outputs once: x86_64 Linux runs `native`, `android`, `windows`, and `linux-static`
separately; ARM64 Linux runs `native`; both Darwin hosts run `all`. A failed
native Linux build therefore leaves the cross-build jobs running. PRs, pushes,
and default manual runs retain that complete matrix. A manual dispatch can
select the existing three `linux-static` outputs with
`gh workflow run nix.yml --ref BRANCH -f group=linux-static`, or the four Android
outputs with `gh workflow run nix.yml --ref BRANCH -f group=android`, or the two
Windows outputs with `gh workflow run nix.yml --ref BRANCH -f group=windows`, or
all four Intel Darwin outputs with
`gh workflow run nix.yml --ref BRANCH -f group=x86_64-darwin`. The registered
workflow can run on a branch containing this workflow file; it does not require
that file on the default branch. Runs for the same workflow/ref queue without
canceling an active run; manual groups have separate queues. Legacy PR build jobs also preserve running
jobs; their cancellation policy for other events and release tags is unchanged.
A two-minute planning job selects these
existing groups; it does not evaluate or build Nix outputs. Each build job
uploads its existing library ZIPs and CLIs under an artifact named for the build
host, target group, and source SHA. Each native CLI also runs
the encrypted-store and profile-password smoke test, which can be repeated with
`python3 scripts/nix/smoke-cli.py result-nix/BUILD_SYSTEM/exe:simplex-chat/bin/simplex-chat`.

The Windows job first prepares `checks.x86_64-linux.windows-rts-fs.inputs`
under the normal 7,200-second per-derivation budget, then runs
`checks.x86_64-linux.windows-rts-fs` with the short 300-second limit.
Both share the existing preparation deadline. The inputs contain only the C
payload and Wine, allowing a cold patched C toolchain to build before the
runtime trial. This real-WinAPI check compiles the unchanged locked GHC
`rts-fs` source with the selected MinGW C toolchain and runs it with the same
Wine package as the interpreter.
The Windows CRT overlay backports upstream MinGW commit `e7e21db45eaa` to the
verified 13.0.0/14.0.0 sources. It fixes ISO `swprintf` rejecting strings that
exactly fit with their terminator; the regression also requires a negative
return for a too-small buffer and intact outside-buffer sentinels.
The overlay preserves haskell.nix's existing UCRT pthread flags when rebuilding
the Windows scope; the current SimpleX Windows targets use MSVCRT.
It requires real file opens, descriptor/stat information and fixture bytes for
short, long and archive-shaped paths, including absolute and extended variants.
Helper results and a separate direct Win32/CRT replay remain diagnostic output.
The owned Wine prefix has a 20-second execution window and bounded cleanup;
missing environment/file prerequisites are distinct from an FS regression.
Run it alone with `nix build .#checks.x86_64-linux.windows-rts-fs -L`.
Check overlay scope with
`nix eval --json --file scripts/nix/check-mingw-stdio-scope.nix`.
It adds no Haskell compiler-project build or dependency to the 21 product outputs.

After this check passes, the Windows job prepares only the named direct input outputs of the
canonical `wai-app-static` component from the same Windows CLI project, using
the normal 7,200-second per-derivation limit. It then builds that exact component
with a 300-second limit; success is reused by the full Windows build. A failure
keeps the job red and skips the full outputs. CI replays only that failed
component in a separate diagnostic derivation, with `ISERV_ARGS=-v` and
`strace -f -tt -T -s 80` around compiler calls. It copies the exact selected Wine
scripts and appends `+seh,+loaddll` to their pinned `WINEDEBUG` value, with an
assertion that no other script bytes change. Only compile calls receive the
copied `-pgmi` path. Compiler queries, source/plan, original arguments, DLL
collection, Wine prefix and normal/profile selection stay unchanged.
The trace has a 15-minute step bound and a 300-second per-derivation limit;
strace is fetched from caches only. Logs and the selected derivation/input
metadata are uploaded even on failure. Repeat the canonical gate on Linux with
`python3 scripts/nix/windows-th-gate.py prepare` followed by
`python3 scripts/nix/windows-th-gate.py build`; use `trace` only after a failed
canonical attempt. These commands use the committed checkout and preserve the
canonical Nix core setting. A fresh Wine prefix and full syscall tracing can
consume most of the diagnostic budget; a timeout does not identify a deadlock.
The diagnostic changes its derivation and process
environment; it does not qualify a Windows artifact or change the failed result.

After a failed native x86_64 Linux library build, CI tries a separate derivation
with `-v2 -ddump-timings` and GDB on compile calls and uploads
`native-linux-ghc-phase-trace.log`, including when that trace fails. Compiler
queries and the original source, plan, dependency units and GHC arguments stay
unchanged. GDB follows the compiler's shell wrapper into the actual ELF program,
preserving its injected package DB and `-B` arguments. On a stopped fault it
records up to 80 frames per thread, registers, the PC symbol and disassembly,
shared libraries and memory mappings. It does not dump the environment or a core
file. Normal compiler exit codes propagate; faults and debugger errors fail the
diagnostic. The canonical build remains failed. The trace runs one derivation at
a time and inherits the canonical Nix core setting, preserving Setup's job count
and the compiler job arguments. It has a two-hour wall-clock/per-derivation
bound, including cache-only debugger setup; a debugger cache miss fails visibly
without compiling GDB locally or remotely. The job's six-hour limit stays in
place. A local `--cores` override also
changes those job arguments and must be reported when comparing diagnostics.
`trace-native-ghc.nix` accepts the original component via `--arg original` and an
optional pinned GDB derivation via `--arg debugger`. Omitting `debugger` retains
the phase-only trace. GDB leaves ASLR enabled; its host/runtime effects must be
reported when comparing diagnostics.

After a failed Intel Darwin GitHub CI build, `build.sh` also prints technical
fields from recent Nix GHC `.ips` crash reports, if macOS publishes readable
reports. The collector reads the user and system `DiagnosticReports` directories
without a debugger or process replay. It selects reports modified since the
failed target's build started, then reports the exception, faulting thread's
registers and up to 80 frames with their original binary-image indexes. These
are recent compiler reports; their PID and executable path must be correlated
with the failed component before attributing a cause. At most three reports
are printed, each limited to 2 MiB input and 64 KiB technical output, with a
15-second overall deadline. Missing, inaccessible or incomplete reports do not
change the original build's exit status. Local builds and successful CI builds
do not invoke this collector. Run its focused regressions with
`python3 -B scripts/nix/test-darwin-ghc-crash.py`.

If that failed target is the Intel Swift iOS library and its log identifies one
SimpleX library derivation with compiler exit code `-10` (SIGBUS), the driver
also attempts one separate compiler replay. `trace-intel-ghc.py` pins the current
checkout and checks the original component's exact derivation path before
building a diagnostic copy. Its added Python input must already be available
locally or from substitutes; a cache miss skips the replay. Existing Apple
Clang builds a small signal recorder for the Intel runner. The normal Nix
sandbox stays enabled. The replay has a 300-second wall-clock/per-derivation
limit and a 360-second overall preparation-and-replay budget, plus a five-second
termination grace. It adds no compiler flags and preserves the actual compiler
arguments and response-file bytes. Compiler queries pass through unchanged.
Each launcher command runs in its own process group. Python enforces the
deadline and kills only that group on expiry, with at most five seconds to reap
the command; GNU `timeout` and Homebrew aliases are not required. Missing Apple
or Nix tools are named explicitly.

For the default Apple toolchain, the launcher reads `DEVELOPER_DIR` or makes
one bounded `xcode-select --print-path` query, then validates the existing
Clang executable and macOS SDK in that selected Xcode or Command Line Tools
installation. This avoids `xcrun --find clang`, which reached its ten-second
limit on hosted Intel before the recorder could start. An invalid selection
does not fall back to another installation. Explicit custom `TOOLCHAINS`
selection retains the original bounded `xcrun` queries. The Intel policy tests
also check the real selected Apple paths and Clang version on hosted Darwin
before the production build starts; this metadata check has a twenty-second
overall limit and does not compile or run GHC.

The recorder loads only into the selected normal SimpleX iOS compiler process.
It reports its main executable's UUID, image base and slide, then records SIGBUS
fault address, signal code and registers, including PC and SP, if the process
faults. It does not unwind the signal handler or read memory at the fault
address. It restores the loader environment before compiler children start and
disables core output. This diagnostic adds a loaded image and a signal handler;
its results require comparison with the canonical failure. SIGBUS still
terminates the instrumented compiler; its enclosing shell reports status 138.
The build driver always returns the original canonical build status. A skipped,
failed or successful replay does not qualify a production output. Run the
selector and launcher controls with
`python3 -B scripts/nix/test-select-ghc-sigbus.py` and
`python3 -B scripts/nix/test-trace-intel-ghc.py`. The recorder's owned-process
controls are in `python3 -B scripts/nix/test-trace-ghc-sigbus.py`.

When the pinned Intel Darwin `ghc-boot` component fails, CI captures a separate
Setup diagnostic. It selects one exact failed `ghc-boot` derivation from the
canonical log and checks it against the selected compiler project; a rebuilt
compiler therefore does not leave a stale diagnostic path. Multiple matching
failed components cause the probe to skip. It preserves the newly linked Setup
executable's bytes and
runs its actual configure arguments with an explicit capture mode. LLDB mode
records bounded thread backtraces, registers, instructions and loaded images;
compiler queries and other Setup commands pass through. Child failures remain
failures, and the canonical job stays failed. The diagnostic has a two-hour
step/per-derivation limit and one five-minute deadline for LLDB plus any fallback.
Its log is
uploaded as `intel-setup-lldb-trace.log`, including after failure. Only this probe
disables the Nix sandbox to reach host tools; it records that runtime
difference and the debugger version. Hosted Intel uses `direct-abort` mode:
LLDB hosts the Python command without attempting debugger attachment, and the
probe invokes Setup once with a diagnostic abort interposer built by the
runner's existing Apple Clang. LLDB mode also retains a fallback for an explicit
permission denial before a child PID is assigned.
It reports up to 80 current-thread unwind frames, available callee-saved
registers and image addresses. It symbolicates mapped register addresses without
dereferencing them, then forwards to real `abort`. Its constructor
restores the original loader environment before Setup starts compiler children
and disables core output for the traced process tree. This adds a loaded image
to Setup; it changes no debugger permissions or security settings. Nonzero
exits and terminating signals remain failures. The hosted Intel replay captured
14 frames through `evacuate1` and `scavenge_loop1`. A local Setup built with the
same cached compiler and target dependency IDs reproduces the panic in 1.55
seconds, and its traced replay matches nine cloud frame offsets. Native ARM64
and Rosetta/x86 C controls also cover the helper.
A successful probe is diagnostic evidence.

Hydra continues to consume
the same `hydraJobs` attributes. The older application release workflow still
uses its existing GHCup/Docker compiler settings; migrating that packaging and
release path is separate from this Nix build lane. The legacy GHC 8.10 Linux job
now reports container memory counters and kernel OOM records after a failed CLI
build, with a one-minute diagnostic limit. Its observed SIGKILL alone does not
establish an out-of-memory cause.

Hydra needs builders advertising `aarch64-darwin` and `x86_64-darwin` to run the
corresponding Mac/iOS jobs. Check its [builder pool](https://ci.zw3rk.com/machines)
when jobs remain queued. The pinned haskell.nix now defaults
`emulatorSystemFeatures` to `[]`; its own CI opts back into `recursive-nix` as a
native-builder scheduling marker. This changes scheduling defaults for cross
targets independently of this checkout's explicit `ghc914-sh` Android ARM32
opt-out on Linux with patched QEMU 9.1–11, where normal target TH passed under
emulation. The marker also covers cross-compiler stalls without QEMU. If a job
requires it, advertise it on a native x86_64 Linux builder and keep it off
Rosetta slots. Nix also requires the
[`recursive-nix` experimental feature](https://nix.dev/manual/nix/2.35/development/experimental-features.html#xp-feature-recursive-nix)
when building derivations with this reserved marker. Enable both
`extra-experimental-features = recursive-nix` and
`extra-system-features = recursive-nix` on that builder. The GitHub workflow
sets the complete `experimental-features` list, including `nix-command` and
`flakes`, because its installer suppresses those defaults when this setting is
present. It adds `recursive-nix` to both lists only for its native x86_64 Linux
runner. The upstream fixes and regression jobs
are in [haskell.nix PR #2585](https://github.com/input-output-hk/haskell.nix/pull/2585).

To build both Android APK ABIs from the current checkout, preserving the same
compiler and source, use the existing builder's source/skip-checkout flags:

```sh
scripts/android/build-android.sh -sg VERSION_CODE
```

This command also needs the Android SDK, Gradle, and its other existing tools.
The APK reproduction script now checks out the supplied tag for both ABIs.
Historical releases that used a separate `-armv7a` tag must be reproduced with
their historical build scripts.

## Migration validation

On 2026-10-16, all 21 existing Hydra derivations evaluated successfully at
`f26dbbc0d93a5c359427307b4c57f7f5e2123d69`: 11 on x86_64 Linux, two on ARM64 Linux,
and four on each Darwin architecture. This includes both Android ABIs, Windows,
and all three static Linux targets. This establishes derivation evaluation;
the completed artifact builds are listed below.

GitHub run `38939062012` tested head `c5b7a04fe` as integration merge
`582e88ccf07d5da06cfb0e9d7e99d9133f2f56af`. Both native Linux library/CLI groups
and all four ARM64 Darwin outputs passed, including encrypted-store and
profile-password CLI smoke checks. The production x86_64 Linux compiler also
passed the ELF alignment regression. The x86_64 musl CLI linked with
`-fully-static`, installed, and completed fixup. These qualify nine built outputs;
the static group subsequently failed, and its artifact upload did not run.

The ARM64 Android normal and profiling interpreters compiled and linked with the
API-aware symbol table. The next Android failure is SplitMix 0.1.3.2 calling
`getentropy` at API26. The local fallback described above passes exact-source
compilation and shared linking for both ABIs with native Clang21/LLD21 and the
actual NDK headers/libraries. API35 objects remain byte-identical. The ARM32
interpreter table also compiles with its real `arc4random_buf` registration and
remains byte-identical at API35. An earlier source variant passed actual NDK
Clang18; the final Clang18 fixture and production package-tar evaluation were
blocked before compilation by the local Linux VM/frontier. The final Android
interpreter runtime and application builds still need CI.

The new dynamically C-linked i686 interpreter reaches its link step but fails
because `symbols.i386-musl.o` references private musl `__aio_close`. Its real GHC
arguments retain static Haskell libraries and omit `-fully-static`. This proves
the link-policy change reached the component; the helper ELF and TH execution
remain unqualified. Normal and profiling plans retain all 95 record identities:
only three iserv configure arguments disable executable-static; the other 92
records are byte-identical. The changed project skeleton rebuilds auxiliary boot
derivations. An i686/QEMU C control reproduces the old static-loader error and
loads the same `libz.so` with dynamic C linkage.

The newer focused static run `38974180411` at `43262a5c0` built and installed
both normal and profiling i686 interpreters after the private-symbol fix. Its
x86_64 static CLI also built. The i686 `wai-app-static` Template Haskell splice
then failed: the runtime linker rejected duplicate `__x86.get_pc_thunk.bx` and
`.ax` definitions before unresolved lookups, a QEMU target SIGSEGV, and proxy
EOF. The scoped COMDAT candidate cross-compiles both changed C files against
real GHC 9.14 headers with the original warning messages unchanged. Its physical
source producer changes exactly `Linker.c`, `linker/Elf.c`, and `linker/Elf.h`.
The paired native x86_64 Linux runtime script passes with the exact existing
GHC 9.14 compiler on the Hydra host: the original RTS rejects the COMDAT fixture,
and the patched RTS preserves relocations in both load orders while rejecting
ordinary strong and stale archive-owner conflicts. Both explicit RTS object
pairs come from the same configured source; their link maps exclude the original
archive objects. This isolated direct run completes in 4.3 seconds without a Nix
build or configuration change. A real GCC15 `-m32` PIC fixture then exposed
zero-sized thunk signatures, which the initial sized-only candidate rejected.
The corrected paired test passes in 4.8 seconds, including zero-size and mixed
load orders, local-label preservation, and rejection of extra functions, data,
global/weak definitions and partial extents. Extra definitions use distinct names
in each object so unrelated duplicate symbols cannot satisfy negative controls.
The Nix check and final i686 QEMU TH/application builds remain CI checks,
including profiling execution.

Intel Darwin reproduced its Setup GC abort and produced the direct-abort trace
used for the info-table diagnosis above. The local renderer regression passes,
while the patched compiler bootstrap and full Intel outputs await CI. Windows
is still building; its Basement follow-up passes the focused reproducer.

The native ARM64 Darwin library and CLI built with Nix at `f26dbbc0d` and
passed `python3 scripts/nix/smoke-cli.py /path/to/simplex-chat`, covering encrypted
stores and profile passwords. Their actual outputs contain ARM64 macOS objects
and the expected library registration or CLI executable. Earlier native BBS
checks also passed; the relevant native source, compiler, flags, and dependencies
remain unchanged.

A native build with the iOS Swift/CommonCrypto flags passed mobile C API checks
using the rolled archive produced by `link-mobile-library.sh`: Swift JSON,
encrypted stores, UTF-8, reopen behavior, and wrong-key errors with copying and
nonmoving GC. This ran on macOS before archive retagging. The CommonCrypto
SQLCipher component also built with Nix and registered both Apple frameworks.
The replacement RTS and `text` built with Nix and passed ABI,
instruction-baseline, UTF-8, and GC checks.

Both full Nix ARM64 iOS library ZIPs, Swift JSON and tagged JSON, built in that
GitHub run. The downloaded artifact matched its published SHA256, outer/inner
ZIP CRCs, and all ten archive hashes recorded in CI. Each ZIP contained its five
expected archive entries and 3,632 ARM64 Mach-O members with iOS platform markers.
The earlier `f26dbbc0d` ZIPs contained 3,630 members and valid Hydra product markers.
The production FFI archive previously passed an instruction-baseline check with
unchanged inputs. The package uses the full Cabal v2 unit ID in its archive
filenames; the existing Xcode project updater accepts those names. This verifies
the packaged library, not execution after retagging.

The Android API26 ARMv7 `ghc-internal` built with all 292 Haskell modules and
C sources. Its unit ID and ABI match the earlier build, and GNU iconv is retained
in installed paths and consumer dependencies. Canonical `base` and the complete
normal interpreter dependency closure also built under Rosetta.

On 2026-10-16, the normal ARM32 TH trial passed with the production compiler,
interpreter, and all 40 target-library outputs. A target-side splice asserted
`System.Info.arch == "arm"` and a 32-bit `Int`, then emitted `42`. The resulting
shared ELF32 ARM library exported the test function; a static C caller initialized
the Haskell RTS and received `42` from the same compiled object. This inspects the
shared library and executes a static caller, rather than loading the shared
library on an Android device. The byte-identical upstream annotation test also
compiled through the target interpreter.

The identical production interpreter reached Haskell `main` with both x86_64
QEMU under Rosetta and native ARM64 Linux QEMU. It has no `PT_TLS` segment and
needed no TLS adjustment. The separate C caller retains the API26 CRT alignment
fixture used by the C regression. The final QEMU version guard preserves the
exact compiler, core, base, interpreter, and wrapper identities from this trial.
These functional changes are integrated at `748becda3eac5173bdbf34ef4eec476a9215326f`.

The trial used a normal-only diagnostic wrapper around the real production
vanilla interpreter. The full mobile wrapper also references a profiling
interpreter, so normal application builds can still compile profiling dependencies.
Both normal and profiling ARMv7 `ghc-internal` archives subsequently built in
the earlier `b4a1cdd54` compiler context: each contains 283 ELF32 ARM objects,
with profiling objects in the profiling archive. Their installed registration
retains GNU iconv paths. This does not qualify the later native RTS compiler
change or the profiling interpreter's final link and execution.
Profiling interpreter execution, corrected Android/Windows builds, the complete
static Linux group, Intel Darwin outputs, Android devices/APKs, and an Xcode
application build remain unverified. Derivation evaluation and the normal trial
do not establish that those builds and runtime checks pass.

## Updating package hashes

1. Install `nix`, `gawk` and `jq`.

2. Start nix-shell from repo root:

```sh
nix-shell -p nix-prefetch-git
```

3. Run in nix shell:

```sh
gawk -f ./scripts/nix/update-sha256.awk cabal.project > ./scripts/nix/sha256map.nix
```
