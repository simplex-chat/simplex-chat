# Failure-only replay of one exact Intel Darwin SimpleX library slice.
# The compiler's own wrapper, arguments and dependencies remain intact. The
# injected library records only its own process; no debugger or host tool is
# needed inside the normal Nix sandbox.
{ original, expectedDrv, tracerPath, python }:
let
  tracer = builtins.path { path = tracerPath; name = "ghc-sigbus-recorder.dylib"; };
  quote = value: "'" + builtins.replaceStrings [ "'" ] [ "'\\''" ] value + "'";
  needle = "    printf '#!/bin/sh\\nexec %s \"$@\"\\n' \"$g\" > \"$wrapBin/$gName\"";
  replacement = ''
        printf '#!/bin/sh\nrealGhc="%s"\n' "$g" > "$wrapBin/$gName"
        cat >> "$wrapBin/$gName" <<'SIMPLEX_GHC_SIGBUS_WRAPPER'
    # Cabal passes the actual compilation as @/absolute/file.rsp. Inspect its
    # bounded contents for selection, then pass the original argv unchanged.
    if ${python}/bin/python3 ${./select-ghc-sigbus.py} \
      --package-version ${quote original.identifier.version} -- "$@"; then
      :
    else
      selectionStatus=$?
      [ "$selectionStatus" -ne 1 ] || exec "$realGhc" "$@"
      exit 125
    fi
    : "''${NIX_BUILD_TOP:?Intel GHC diagnostic requires NIX_BUILD_TOP}"
    diagnosticDir=$(mktemp -d "$NIX_BUILD_TOP/intel-ghc-sigbus.XXXXXX") || exit 1
    record=$diagnosticDir/fault.bin
    # Cabal uses ghc-9.14, which is an immutable symlink to the regular script.
    # Queries above still execute its original name; only the private-copy
    # preparation resolves the alias, without changing any compiler argument.
    resolvedGhc=$(readlink -f -- "$realGhc") || exit 125
    # The strict Python parser checks the complete immutable wrapper and the
    # final executable's Mach-O type/CPU. This extracts its literal pathname;
    # it never evaluates shell text from the wrapper.
    expectedBinary=$(sed -n 's|^exec \(/nix/store/[a-z0-9]*-ghc-bin-exe-ghc-[a-zA-Z0-9.+-]*/bin/ghc\) .*|\1|p' "$resolvedGhc")
    if [ -z "$expectedBinary" ]; then
      printf '%s\n' 'Intel GHC diagnostic: compiler wrapper shape was not recognized.' >&2
      exit 1
    fi
    ${python}/bin/python3 ${./trace-ghc-sigbus.py} prepare \
      --source "$resolvedGhc" --output "$diagnosticDir/ghc" \
      --tracer ${quote (toString tracer)} --record "$record" \
      --expected-binary "$expectedBinary" --cpu x86_64 || exit 1
    : > "$NIX_BUILD_TOP/intel-ghc-sigbus-attempted"
    printf '%s\n' 'Intel GHC diagnostic: one scoped compiler invocation with a signal recorder.'
    set +e
    "$diagnosticDir/ghc" "$@"
    status=$?
    printf 'Intel GHC diagnostic: compiler wrapper exit %s\n' "$status"
    if [ -s "$record" ]; then
      printf '%s\n' 'Intel GHC diagnostic: binary record follows.'
      ${python}/bin/python3 ${./trace-ghc-sigbus.py} decode "$record" || \
        printf '%s\n' 'Intel GHC diagnostic: record decoding failed.' >&2
    else
      printf '%s\n' 'Intel GHC diagnostic: no readable signal-recorder startup record.' >&2
    fi
    exit "$status"
    SIMPLEX_GHC_SIGBUS_WRAPPER
  '';
  occurrences = text: builtins.div
    (builtins.stringLength text - builtins.stringLength (builtins.replaceStrings [ needle ] [ "" ] text))
    (builtins.stringLength needle);
in
assert original.system == "x86_64-darwin";
assert original.identifier.name == "simplex-chat";
assert original.identifier.component-type == "lib";
assert original.drvPath == expectedDrv;
assert python.system == "x86_64-darwin";
assert builtins.isString tracerPath && builtins.substring 0 1 tracerPath == "/";
original.overrideAttrs (old: assert occurrences old.buildPhase == 1; {
  nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ python ];
  timeout = 300;
  buildPhase = builtins.replaceStrings [ needle ] [ replacement ] old.buildPhase + ''
    if [ ! -f "$NIX_BUILD_TOP/intel-ghc-sigbus-attempted" ]; then
      echo 'ERROR: the diagnostic did not reach the scoped compiler invocation' >&2
      exit 1
    fi
  '';
})
