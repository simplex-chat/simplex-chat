# Compile-phase diagnostics for an existing, already-planned native component.
# This produces a different derivation; it does not change source, plan, compiler,
# dependency units or the original GHC arguments. Query output stays unchanged.
{ original }:
let
  needle = "    printf '#!/bin/sh\\nexec %s \"$@\"\\n' \"$g\" > \"$wrapBin/$gName\"";
  replacement = "    printf '#!/bin/sh\\n"
    + "ghcTraceCompile=\\n"
    + "for arg do\\n"
    + "  case \"$arg\" in\\n"
    + "    --numeric-version|--version|-V|--info|--print-*|--show-*|--supported-*|--abi-hash|-M)\\n"
    + "      exec \"%s\" \"$@\"\\n"
    + "      ;;\\n"
    + "    @*|--make|-c) ghcTraceCompile=1 ;;\\n"
    + "  esac\\n"
    + "done\\n"
    + "if [ -n \"$ghcTraceCompile\" ]; then\\n"
    + "  exec \"%s\" \"$@\" -v2 -ddump-timings\\n"
    + "fi\\n"
    + "exec \"%s\" \"$@\"\\n"
    + "' \"$g\" \"$g\" \"$g\" > \"$wrapBin/$gName\"";
  occurrences = phase: builtins.div
    (builtins.stringLength phase - builtins.stringLength
      (builtins.replaceStrings [ needle ] [ "" ] phase))
    (builtins.stringLength needle);
in original.overrideAttrs (old:
  # Fail visibly if the pinned v2 builder changes its wrapper-generation seam.
  assert occurrences old.buildPhase == 1; {
    buildPhase = builtins.replaceStrings [ needle ] [ replacement ] old.buildPhase;
  })
