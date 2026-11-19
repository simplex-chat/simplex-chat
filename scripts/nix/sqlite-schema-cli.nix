# Schema dump shells out to sqlite3 and compares .schema --indent with
# chat_schema.sql. SQLite 3.53 (nixpkgs sqlite.bin on this pin) drops
# IF NOT EXISTS and swallows a following ") STRICT" into the comment on
# the previous line, so the dump no longer matches and will not reload.
# 3.54 rewrote the formatter again. 3.51.0 reprints the checked-in schema.
# This is only the CLI used by the test. The linked library stays SQLCipher.
{ stdenv, fetchurl }:
stdenv.mkDerivation {
  pname = "sqlite-schema-cli";
  version = "3.51.0";

  src = fetchurl {
    url = "https://sqlite.org/2025/sqlite-autoconf-3510000.tar.gz";
    sha256 = "19bc2inw7f9fn0y6j3b57w4mk6bzi2q8hp5yn6qyd8kav7ynvqj2";
  };

  # The autoconf amalgamation has no --disable-tcl. The shell is statically
  # linked so the test does not pick up another libsqlite.
  configureFlags = [
    "--disable-readline"
    "--enable-static-shell"
  ];

  enableParallelBuilding = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin"
    install -m 0755 sqlite3 "$out/bin/sqlite3"
    runHook postInstall
  '';

  meta.description = "SQLite 3.51.0 CLI for the SimpleX schema dump check";
}
