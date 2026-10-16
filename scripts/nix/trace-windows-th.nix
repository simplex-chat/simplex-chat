# Failure-only replay of the exact canonical Windows TH component. Source,
# plan, compiler arguments, Wine wrapper and normal/profile selection stay put.
{ original, strace, expectedDrv }:
let
  needle = "    printf '#!/bin/sh\\nexec %s \"$@\"\\n' \"$g\" > \"$wrapBin/$gName\"";
  replacement = ''
        printf '#!/bin/sh\nrealGhc="%s"\n' "$g" > "$wrapBin/$gName"
        cat >> "$wrapBin/$gName" <<'SIMPLEX_WINDOWS_TH_TRACE'
    ghcTraceCompile=
    for arg do
      case "$arg" in
        --numeric-version|--version|-V|--info|--print-*|--show-*|--supported-*|--abi-hash|-M)
          exec "$realGhc" "$@"
          ;;
        @*|--make|-c) ghcTraceCompile=1 ;;
      esac
    done
    if [ -n "$ghcTraceCompile" ]; then
      # The pinned Wine wrapper appends this to the child command, where both
      # the native proxy and target interpreter recognize -v. No -opti or
      # PROXY_ARGS change: their positions differ from this protocol's --pipe.
      export ISERV_ARGS=-v
      : "''${NIX_BUILD_TOP:?Windows TH diagnostic requires NIX_BUILD_TOP}"
      touch "$NIX_BUILD_TOP/windows-th-trace-attempted"
      exec timeout --kill-after=30s 300s ${strace}/bin/strace -f -tt -T -s 80 "$realGhc" "$@"
    fi
    exec "$realGhc" "$@"
    SIMPLEX_WINDOWS_TH_TRACE
  '';
  occurrences = phase: builtins.div
    (builtins.stringLength phase - builtins.stringLength
      (builtins.replaceStrings [ needle ] [ "" ] phase))
    (builtins.stringLength needle);
in
assert original.system == "x86_64-linux";
assert original.identifier.name == "wai-app-static";
assert original.name == "wai-app-static-lib-wai-app-static-x86_64-w64-mingw32-3.1.9.1";
assert original.drvPath == expectedDrv;
original.overrideAttrs (old:
  assert occurrences old.buildPhase == 1; {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ strace ];
    buildPhase = builtins.replaceStrings [ needle ] [ replacement ] old.buildPhase + ''
      if [ ! -f "$NIX_BUILD_TOP/windows-th-trace-attempted" ]; then
        echo 'ERROR: Windows TH diagnostic did not intercept a compile call' >&2
        exit 1
      fi
    '';
  })
