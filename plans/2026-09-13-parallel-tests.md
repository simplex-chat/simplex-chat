# Parallel test execution

Every test gets its own ports and its own directory, so hspec runs the tree
concurrently in one process with `parallel` and `--jobs`.

## Decisions

- `TestParams` gains `portBase :: Int`. A pool of bases `[7000, 7010 .. 8990]`
  is created in `main`; `testBracket` takes a base for the test and returns it
  after. Ports of a test: SMP `base + 1`, XFTP `base + 2`, second SMP `base + 3`,
  remote host `base + 4`.
- `TestCC` carries `testParams`. `class HasTestParams` with instances for
  `TestParams` and `TestCC` lets helpers take either, so closure tests reach
  the environment through a client: `tmpFile bob "test.jpg"`, `smpPort alice`,
  `withXFTPServer alice`.
- Client configs keep the literal ports 7001, 7002 and 7003 as the logical
  layout. `startTestChat_` maps them to the test's ports in `smpServers`,
  `xftpServers` and `shortLinkPresetServers`. Expected output uses the real
  ports through `smpServerStr`, `xftpServerStr`, `smpPort`, `xftpPort`,
  `smpPort2`.
- Servers take the test: `smpServerCfg ps`, `withSmpServer ps`,
  `withSmpServerAndNames ps`, `xftpServerConfig ps`, `withXFTPServer ps`.
  The XFTP server stays under test control, since tests exercise its restarts.
  Its files and store log are under the test directory.
- `tests/tmp` is created once around the whole run (`withTmpFiles` in `main`)
  and each test works in `withTempDirectory "tests/tmp"`. Paths in tests are
  `tmpDir` and `tmpFile` of the client or params.
- `xftpCLI` runs the test binary as a subprocess with the first argument
  `xftp-cli`, because `withArgs` and `capture_` are process-global.
- Test suite `ghc-options` gain `-rtsopts "-with-rtsopts=-N"`.
- `parallel` wraps the tree. Sequential remain: schema dump, API docs,
  "Save query plans" (last item, reads the maps after every test).
- Postgres: the database is created once (`beforeAll_`/`afterAll_`); the
  client schema prefix includes the test directory name.

## Status

Implemented; the suite builds. Runs so far: file tests, 39 examples in 111 s
with `-j 8` on 4 cores at -O0, one timing failure per run, a different test
each time. "error receiving file" (`testXFTPRcvError`) fails intermittently
even alone with `chat db error: SERcvFileInvalidDescrPart`: `appendRcvFD`
receives a description part for a description that is already complete, so
the part arrives twice. The first block stops bob right after alice's upload
completes, without waiting for bob to process the description, which is the
likely source of the duplicate after his restart. The race predates this
change; `-N` and load make it more likely.

Direct, schema dump, protocol and names groups: 178 examples in 331 s with
`-j 6`, 11 failures. Two are environmental on this machine: the schema dump
cannot overwrite root-owned `chat_schema.sql`, and `encrypt/decrypt database`
needs `direct-sqlcipher +openssl`, which CI sets and the local build lacks.
The rest pass alone and fail under load: expiration and timed-message tests
with second-scale TTLs, the client-timeout retry test, and cases where a
line already consumed reappears at the end of the test, which points at the
window diff in `readTerminalOutput` under batched terminal updates.

Profile tests at the default job count (4 cores): 105 examples in 439 s,
2 failures, both timed-message tests with second-scale TTLs.

## Mechanics

- `tests/ChatTests/DBUtils/*.hs`: `portBase` field.
- `tests/ChatClient.hs`: `HasTestParams`, port and path helpers, server
  configs and brackets as functions of the test, port mapping in
  `startTestChat_`, `withTmpFiles` without removal per test.
- `tests/Test.hs`: port pool, `xftp-cli` subcommand, `parallel`, `withTmpFiles`
  around `hspec`.
- `tests/SchemaDump.hs`: `withTmpFiles` wrappers removed.
- `tests/ChatTests/Utils.hs`: `xftpCLI` subprocess.
- Test modules: literal `tests/tmp` paths, server ports and XFTP brackets
  rewritten through the helpers.
