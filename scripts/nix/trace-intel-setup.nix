# Failure-only diagnostic for the pinned Intel Darwin compiler's ghc-boot Setup.
# Resolve lldbPath with xcrun on the runner before evaluation. Only this
# diagnostic build may use `--option sandbox false` to reach Apple's host tools.
{ original, lldbPath, abortTracerPath, captureMode ? "lldb"
, expectedDrv ? "/nix/store/vghjgkr64qjigddh3gnf3c103wbvpxmz-ghc-boot-lib-ghc-boot-9.14.drv"
}:
let
  # Track the runner-compiled dylib's bytes, not a mutable host-path reference.
  abortTracer = builtins.path { path = abortTracerPath; name = "intel-setup-abort.dylib"; };
  shellQuote = value: "'" + builtins.replaceStrings [ "'" ] [ "'\\''" ] value + "'";
  needle = "    printf '#!/bin/sh\\nexec %s \"$@\"\\n' \"$g\" > \"$wrapBin/$gName\"";
  replacement = ''
        printf '#!/bin/sh\nrealGhc="%s"\n' "$g" > "$wrapBin/$gName"
        cat >> "$wrapBin/$gName" <<'SIMPLEX_SETUP_COMPILER'
    setupOutput=
    previous=
    setupSource=
    setupMake=
    for arg do
      [ "$previous" != -o ] || setupOutput=$arg
      case "$arg" in
        --make) setupMake=1 ;;
        */ghc-boot-9.14/setup/setup.hs) setupSource=1 ;;
      esac
      previous=$arg
    done
    "$realGhc" "$@"
    status=$?
    [ "$status" -eq 0 ] || exit "$status"
    case "$setupOutput" in
      */ghc-boot-9.14/setup/setup)
        if [ -n "$setupMake" ] && [ -n "$setupSource" ]; then
          realSetup=$setupOutput.lldb-real
          cp -p "$setupOutput" "$realSetup" || exit 1
          cmp "$setupOutput" "$realSetup" || exit 1
          echo 'Intel Setup diagnostic: preserving compiler-produced executable'
          sha256sum "$realSetup"
          printf '#!/bin/sh\nset -eu\nrealSetup="%s"\noriginalSetup="%s"\n' \
            "$realSetup" "$setupOutput" > "$setupOutput"
          cat >> "$setupOutput" <<'SIMPLEX_SETUP_EXECUTION'
    configure=
    for arg do
      [ "$arg" != configure ] || configure=1
    done
    if [ -z "$configure" ]; then
      exec "$realSetup" "$@"
    fi
    argvFile=$originalSetup.lldb-argv
    printf '%s\0' "$originalSetup" "$@" > "$argvFile"
    env -0 > "$argvFile.env"
    : "''${NIX_BUILD_TOP:?Intel Setup diagnostic requires NIX_BUILD_TOP}"
    : > "$NIX_BUILD_TOP/intel-setup-lldb-attempted"
    # LLDB rejects a Python module whose basename contains Nix's hash prefix.
    driverDir=$originalSetup.lldb-driver
    mkdir -p "$driverDir"
    cp -f ${./trace_intel_setup_lldb.py} "$driverDir/trace_intel_setup_lldb.py"
    echo 'Intel Setup diagnostic: actual configure, ${captureMode} mode (diagnostic permits sandbox=false)'
    ${shellQuote lldbPath} --version
    exec timeout --kill-after=10s 300s ${shellQuote lldbPath} --batch --no-lldbinit \
      -o "command script import \"$driverDir/trace_intel_setup_lldb.py\"" \
      -o "intel-setup-run \"$argvFile\" ${shellQuote (toString abortTracer)} ${shellQuote captureMode}" -- "$realSetup"
    SIMPLEX_SETUP_EXECUTION
          chmod +x "$setupOutput"
        fi
        ;;
    esac
    exit 0
    SIMPLEX_SETUP_COMPILER
  '';
  occurrences = phase: builtins.div
    (builtins.stringLength phase - builtins.stringLength
      (builtins.replaceStrings [ needle ] [ "" ] phase))
    (builtins.stringLength needle);
in
assert original.system == "x86_64-darwin";
assert original.identifier.name == "ghc-boot";
assert original.drvPath == expectedDrv;
assert builtins.elem captureMode [ "lldb" "direct-abort" ];
assert builtins.isString lldbPath && builtins.substring 0 1 lldbPath == "/";
assert builtins.isString abortTracerPath && builtins.substring 0 1 abortTracerPath == "/";
original.overrideAttrs (old:
  assert occurrences old.buildPhase == 1; {
    buildPhase = builtins.replaceStrings [ needle ] [ replacement ] old.buildPhase + ''
      if [ ! -f "$NIX_BUILD_TOP/intel-setup-lldb-attempted" ]; then
        echo 'ERROR: ghc-boot Setup configure was not intercepted by the diagnostic' >&2
        exit 1
      fi
    '';
  })
