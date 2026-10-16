# Haskell builds with Nix

The compiler and all cross-compilers come from the haskell.nix input pinned in
`flake.lock`. The `ghc914-sh` compiler is the stable-haskell fork, not the stock
GHC 9.14 release from GHCup. Use the same source checkout and lock file for every
platform; Android ARMv7 does not require a separate branch or tag.

`compiler-selection.nix` and `compiler-cache-overlay.nix` match Hydra's compiler
build context, including auxiliary iserv projects, so the pinned compiler can use
its existing cache. Application dependencies keep the project's current packages.

Cross projects add `ghc-internal` to the Cabal dependencies of the pinned
`composition-1.0.2.2` source. This preserves its deliberate avoidance of `base`
while making GHC 9.14's wired-in types available in isolated target package
databases. The patch runs before planning and source packaging. Native builds
and the Mac-to-iOS conversion projects keep the original package metadata.

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
ARM64 Android libraries retain the 16 KiB linker alignment. These steps run in
the derivation's build/install phases because v2 component overrides do not run
the older `postBuild` and `postInstall` hooks.

Mobile builds keep the RTS's small-address-space setting. On Darwin, the final
link uses ABI-checked replacement RTS registrations, plus a replacement `text`
on ARM64. These boot libraries and all ARM64 mobile app C/C++ code target the
Apple A7 instruction baseline before `mac2ios` retags the archives. The cached
native compiler itself stays unchanged. Android applies the RTS source patch
when building its boot libraries. The Android overlay also supplies GNU iconv
headers and a static PIC archive to `ghc-internal`, including the separate
iserv projects. The `ghc-internal` system dependency propagates GNU iconv to
interpreter consumers as well. The ARM32 interpreter registers the GNU iconv
function addresses for runtime object loading. Its source is patched before Cabal
planning, removing false missing-function stubs that shadow real static Bionic
startup routines. Android links libffi through the RTS package dependency.
The minimum Android API stays at 26.

Android ARM32's QEMU wrapper preserves the guest's `PER_LINUX32` execution
domain when an ARM64 host kernel rejects that domain. Upper personality flags
still go through the host, and queries and setters retain the previous guest
value. This applies only to the Android ARM32 emulator; native compiler cache
paths and the other emulator targets stay unchanged. Android's libc initializes
this state again at startup after an exec.

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
Darwin ARM64/x86_64, with a six-hour limit per job. Its seven jobs cover all 21
outputs once: x86_64 Linux runs `native`, `android`, `windows`, and `linux-static`
separately; ARM64 Linux runs `native`; both Darwin hosts run `all`. A failed
native Linux build therefore leaves the cross-build jobs running. Each job
uploads its existing library ZIPs and CLIs under an artifact named for the build
host, target group, and source SHA. Each native CLI also runs
the encrypted-store and profile-password smoke test, which can be repeated with
`python3 scripts/nix/smoke-cli.py result-nix/BUILD_SYSTEM/exe:simplex-chat/bin/simplex-chat`.

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

When the pinned Intel Darwin `ghc-boot` component fails, CI captures a separate
Setup diagnostic. It preserves the newly linked Setup executable's bytes and
runs its actual configure arguments under the runner's Apple LLDB. The probe
records bounded thread backtraces, registers, instructions and loaded images;
compiler queries and other Setup commands pass through. Child failures remain
failures, and the canonical job stays failed. The diagnostic has a two-hour
step/per-derivation limit and a five-minute limit for LLDB itself. Its log is
uploaded as `intel-setup-lldb-trace.log`, including after failure. Only this probe
disables the Nix sandbox to reach the host debugger; it records that runtime
difference and the debugger version. A successful probe is diagnostic evidence.

Hydra continues to consume
the same `hydraJobs` attributes. The older application release workflow still
uses its existing GHCup/Docker compiler settings; migrating that packaging and
release path is separate from this Nix build lane.

Hydra needs builders advertising `aarch64-darwin` and `x86_64-darwin` to run the
corresponding Mac/iOS jobs. Check its [builder pool](https://ci.zw3rk.com/machines)
when jobs remain queued. For cross builds, the pinned haskell.nix uses
`recursive-nix` as a scheduling marker; these builds do not need recursive Nix
operations. This checkout opts `ghc914-sh` Android ARM32 out of that restriction
on Linux with patched QEMU 9.1–11, after passing normal target TH under emulation.
Other target/compiler package sets retain the upstream guard, which also covers
cross-compiler stalls without QEMU. Advertise the marker on a native x86_64 Linux
builder for those jobs; keep it off Rosetta slots. Nix also requires the
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

Both full Nix ARM64 iOS library ZIPs, Swift JSON and tagged JSON, built at
`f26dbbc0d93a5c359427307b4c57f7f5e2123d69`. Each ZIP contained its five expected
archive entries with valid CRCs and 3,630 Mach-O members tagged ARM64/iOS. Their
Hydra product markers pointed to the actual ZIPs. The production FFI archive
previously passed an instruction-baseline check with unchanged inputs. The
package uses the full Cabal v2 unit ID in its archive filenames; the existing
Xcode project updater accepts those names. This verifies the packaged library,
not execution after retagging.

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
Profiling execution, final Linux/Windows and Intel Darwin outputs, complete
Android libraries/applications, device tests, and an Xcode application build
remain unverified. Derivation evaluation and the normal trial do not establish
that those builds and runtime checks pass.

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
