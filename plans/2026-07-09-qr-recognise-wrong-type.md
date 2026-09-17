# Recognise the scanned QR code type in every scanner — via a core classifier

Revision 2. The first version of this PR classified scans in the UI with ad-hoc
parsers (markdown re-parse, `xrcp:/` prefix check, digit-shape heuristic). Review
rejected that: most links can only be truly recognised by the core (Haskell), and
"looks similar to a valid link" (UI heuristics) is not "is a valid link" (core
decoder accepted it). There must be exactly one way to determine a link's type —
the same decoders that later process the link — exposed to the UI as a C function,
like the existing `chat_parse_server` / `chat_parse_markdown`.

## Problem

Each QR scanner is built for exactly one kind of code:

- New chat → connection link
- Network & servers → SMP/XFTP server address
- Migrate device → migration file link
- Use from desktop → desktop session address (`xrcp:`)
- Verify security code → a contact's security code

The first three reject everything else with their own "invalid" message; the
other two are permissive — Use from desktop forwards any scan to the core and
surfaces only the core's error, and Verify security code runs the scan through
`verifyCode` and reports "Incorrect security code!". (That difference is why
section 3 keeps those two on a two-way branch instead of adding a client-side
gate.) In none of the five cases does the user learn that the code is *valid,
just scanned in the wrong place*.

## Design

One core enum + one exported recognition function. Every scanner always calls it
first:

- result is the scanner's expected type → proceed with the existing success path;
- result is another known type → alert "Wrong QR code" / "This is a X. To use
  it, <where>." (single message string per type);
- result is unknown → that scanner's own contextual error, exactly as before the
  feature (one deliberate exception: iOS ScanProtocolServer — see section 3).

### 1. Haskell: enum + `chat_check_link`

Extract the existing connection-link classification from `Markdown.hs` so it has
a single definition:

```haskell
-- Simplex.Chat.Markdown — extracted from simplexUriFormat (Markdown.hs:349-370),
-- which keeps using it; this is the exact logic that labels links in chat today,
-- including group-links-as-contact-URIs via CRDataGroup client data.
simplexLinkType :: AConnectionLink -> SimplexLinkType  -- XLContact|XLInvitation|XLGroup|XLChannel|XLRelay
```

New result type (next to `ParsedServerAddress`, `src/Simplex/Chat/Controller.hs`):

```haskell
data ScannedLinkType -- Controller.hs needs `import Simplex.Chat.Markdown (SimplexLinkType (..))` — new import, no cycle
  = SLTConnection {linkType :: SimplexLinkType} -- reuses markdown's enum, already deserialised by all UIs; payload field name is exactly `linkType` on the wire
  | SLTServer            -- SMP/XFTP server address
  | SLTFileDescription   -- migration / standalone file link
  | SLTDesktopCtrl       -- desktop session address (xrcp)
  | SLTVerificationCode  -- contact's security code
-- sumTypeJSON, dropPrefix "SLT"

newtype CheckedLink = CheckedLink {linkType :: Maybe ScannedLinkType}
```

Both types derive `(Eq, Show)` (the tests compare decoded values). TH splice
placement in Controller.hs: types after `ParsedServerAddress` (~L1373, before
the file's first splice at ~L1769), `deriveJSON` splices next to
`''ParsedServerAddress`'s (~L1827) with `''ScannedLinkType`'s splice **before**
`''CheckedLink`'s (the latter's generated instance needs the former's).

JSON encoding must follow an existing discriminated-sum convention that the
Kotlin and Swift sides already know how to decode (e.g. the one used for
`ConnectionPlan`) — do not invent a new shape. `dropPrefix` lowercases the
first char after stripping, so the five wire tags are exactly:
`connection`, `server`, `fileDescription`, `desktopCtrl`, `verificationCode`.
These must appear verbatim as Kotlin `@SerialName` values (default `"type"`
discriminator) and as the literal Swift enum case names (synthesized
decoding, `ConnectionPlan`-style — the swift-build `_owsf` wrapper is
invisible to it). `ConnectionPlan` models the JSON shape **and** the
payload-label rule (labels are module-independent); it does NOT model
API.swift's *visibility* rules, since its Swift twin lives in the app target —
section 5 mandates visibility separately. Non-Darwin wire shapes:
`{"type":"connection","linkType":"contact"}`, nullary `{"type":"server"}`.

`chatCheckLink :: ByteString -> JSONByteString` (`src/Simplex/Chat/Mobile.hs`,
same pattern as `chatParseServer`) trims the input (ASCII-only, defensive —
the UI owns the real trim, see section 3) and tries, in order, the
decoders the core itself uses to process each kind:

1. `strDecode @RCSignedInvitation` (Simplex.RemoteControl.Invitation — what
   `/connect remote ctrl` parses, Commands.hs:5750) → `SLTDesktopCtrl`.
   Unambiguous `xrcp:` scheme, safe first.
2. `strDecode @FileDescriptionURI` (Simplex.FileTransfer.Description — what
   `/_download` / `/_download info` parse, Commands.hs:5758-5759) →
   `SLTFileDescription`. Unambiguous `…/file#/?desc=` shape, both `simplex:` and
   `https://` schemes.
3. `strDecode @AConnectionLink` (Simplex.Messaging.Agent.Protocol) →
   `SLTConnection (simplexLinkType l)`. Full URIs and short links, invitation /
   contact / group / channel / relay. This is exactly the link branch of
   `AConnectTarget`'s `strP` (Types.hs:1828) — what `/_connect plan`
   (Commands.hs:5645) parses, i.e. what `planAndConnect` accepts. (Note:
   `connLinkP` / plain `/_connect` requires a *full* URI and rejects bare short
   links — the classifier must NOT be anchored on it.)
4. `strDecode @AProtoServerWithAuth` (what `chat_parse_server` already runs) →
   `SLTServer`. After connection links: both use URI shapes, only the connection
   parser accepts `/invitation|/contact|…#` structure, and `smp://`/`xftp://`
   never parse as connection links.
5. Verification-code shape → `SLTVerificationCode`. There is no parser because a
   code is not encoded — it is generated: `verificationCode = T.pack . unwords .
   chunks 5 . show . os2ip` (Types.hs:1901). The recogniser is the inverse of
   that generator — `isVerificationCode :: ByteString -> Bool`, defined
   immediately after `verificationCode` in Types.hs so shape and generator stay
   together: groups of exactly 5 digits separated by exactly one space (last
   group 1–5 digits, matching `unwords`'s single-space output), ≥ 32 digits
   total. Last, since it is shape-based.
6. Nothing matched → `{}` — with the codebase's `defaultJSON`
   (`omitNothingFields = True`) a `Nothing` field is **omitted**, not `null`.
   Both UIs must decode a *missing* `linkType` key as null (Kotlin's shared
   `json` already has `explicitNulls = false`; Swift optional `Decodable`
   tolerates a missing key). `linkType` as a field name in both `CheckedLink`
   and `SLTConnection` is fine — Controller.hs enables `DuplicateRecordFields`.

Export + manifests (checklist verified against the existing `chat_parse_server`):

- `Mobile.hs`: `foreign export ccall "chat_check_link" cChatCheckLink :: CString
  -> IO CJSONString` (~L132) + wrapper (~L227) + pure `chatCheckLink`.
  Two existing **selective** imports must be widened: `Simplex.Chat.Markdown`
  (:40) gains `simplexLinkType`, and `Simplex.Messaging.Agent.Protocol` (:53)
  gains `AConnectionLink`. `RCSignedInvitation` / `FileDescriptionURI` are new
  imports. `Simplex.Chat.Types` (:50) is imported wholesale, so
  `isVerificationCode` needs nothing; `strDecode` (:57) and
  `AProtoServerWithAuth` (:59) are already in scope.
- `libsimplex.dll.def`: add `chat_check_link` once.
- `flake.nix`: add it in **both** `-optl-Wl,-u` symbol lists (L397, L518) —
  `scripts/desktop/build-lib-*.sh` assert exactly this (once in .def, twice in
  flake) and fail the build otherwise. (Not asserted, optional parity:
  `packages/simplex-chat-nodejs/cpp/simplex.h` declares the C API for the Node
  addon — only add there if the addon should expose it.)
- `apps/multiplatform/spec/architecture.md:125-148` — a maintained, numbered
  table of every `external fun` in `Core.kt` (heading :125, rows 1-18 at
  :129-146), each row carrying that function's **line number** and a
  `…/Core.kt#L<n>` anchor, closed by "Total: 18" at :148. It is currently
  accurate, so adding `chatCheckLink` breaks it. Append the `external fun` at
  the end of the **`external fun` block** (immediately after `chatDecryptFile`
  at :40, i.e. as the new :41 — not at the end of the file) — that keeps the 18
  existing external-fun anchors valid (putting it next to `chatParseServer`
  :32 would instead renumber rows 11-18); then add row 19 and bump the total.
  Appending still shifts everything below `Core.kt:41` by one, so **also** fix
  the second, separate anchor table in the same file, "Key Kotlin Functions in
  Core.kt" (:154-162): `initChatControllerOnStart` L51→52 and
  `initChatController` L62→63. (Its other three rows, :160-162, are already
  stale by 4 — actual 186/198/218 vs cited 190/202/222 — leave them; not ours.)
  One more file carries a currently-exact `Core.kt` anchor below the insertion
  point: `apps/multiplatform/spec/database.md:146` (`Core.kt#L62` → `#L63`).
  Every other `Core.kt#L` reference in the repo points above :41 and is safe.
- `apps/ios/spec/architecture.md:90-93` — the C-function list declared in
  `SimpleX.h`; add `chat_check_link`. (That list is already abridged, omitting
  `chat_parse_uri`/`chat_valid_name` — do not expand its scope, just add ours.)
  The header goes 49→50 lines, so the four `SimpleX.h#L1-L49` whole-file
  anchors in the same doc (:61, :67, :69, :330) become short; bump them to
  `#L1-L50` while editing. (Every other whole-file range in that file is
  already decayed — e.g. `API.swift#L1-L388` vs 397 actual — so don't chase
  those; ours is exact today and cheap to keep exact.)
- Tests: `tests/MobileTests.hs` — one positive case per kind (invitation,
  contact address, group, channel and relay short links, smp server, file link,
  xrcp address, security code) + negatives (arbitrary text/URL, bare number,
  empty). Relay matters: it is the one connection subtype with special
  downstream handling (description-only string) and exercises
  `CCTRelay → XLRelay` in the extracted `simplexLinkType`.
  Assertion style: `sumTypeJSON` is CPP-conditional (`_owsf` single-field
  objects under `darwin_HOST_OS && swiftJSON`, `"type"`-tagged otherwise), so
  do NOT compare the C output to literal JSON strings — decode it back through
  the Haskell `FromJSON` (round-trips under either flag) or provide dual CPP
  fixtures like the existing `noActiveUser`/`noActiveUserSwift`.
  Fixtures: connection links/short links can be reused from
  MarkdownTests.hs:256-273 (`/c#`/`/r#` variants derivable); no `xrcp:` or
  `…/file#/?desc=` fixtures exist in tests/ — the file link is easy to author;
  a syntactically valid `RCSignedInvitation` must be generated or captured
  (signatures are parsed, not verified, so a captured string works).
  Three cases are **mandatory** — they guard the behaviour deltas this plan
  introduces:
  1. A corrupt file link (`simplex:/file#/?desc=<garbage>`) → must classify as
     unknown. Guards the *narrowing* half of the `strHasSimplexFileLink`
     deletion (section 3): the prefix check accepted it and failed late.
  2. A file link on a **non-`simplex.chat` https host**
     (`https://<other-host>/file#/?desc=…`) → must classify as
     `FileDescription`. Guards the *widening* half: the deleted prefix check
     rejected it, `FileDescriptionURI` accepts it and the core downloads it
     identically (the URI host is decorative).
  3. An unsigned `xrcp:` invitation (no `ssig`/`idsig`) → must classify as
     unknown; it fails `RCSignedInvitation`.

  Do NOT write a regression test asserting that a *trailing*-whitespace
  verification code failed pre-PR — it did not; see section 3's right-strip
  note. Only leading whitespace was ever broken.

  Verified separately, no test needed: a corrupt file link cannot be mistaken
  for a connection short link, because the short-link type is a **single** char
  — `i`, or one of `a/c/g/r` (case-insensitive; `contactTypeP` upper-cases
  before matching) — followed by an optional `/` and then `#`, whereas `file`
  is four chars. The two are structurally disjoint, so classifier steps 2/3 are
  order-independent.

### 2. Kotlin binding (shared by Android + desktop)

- `common/.../platform/Core.kt`: `external fun chatCheckLink(link: String): String`
  — inserted as the **last line of the `external fun` block, immediately after
  `chatDecryptFile` (:40), i.e. the new :41**. Do NOT put it next to
  `chatParseServer` (:32), thematically tempting though that is: every
  `external fun` below :32 is hard-anchored by line number in
  `spec/architecture.md` (rows 11-18 at architecture.md:139-146), and section
  1's prescribed doc edits assume the after-:40 placement.
- JNI shims, both copies: `common/src/commonMain/cpp/android/simplex-api.c` and
  `common/src/commonMain/cpp/desktop/simplex-api.c` — `extern` decl +
  `Java_chat_simplex_common_platform_CoreKt_chatCheckLink`, mirroring
  `chatParseServer`.
- `model/SimpleXAPI.kt`, **top-level** — same file as
  `ServerAddress.Companion.parseServerAddress` but NOT inside that companion
  (mirroring iOS, where `parseServerAddress` is a top-level `public func`;
  a companion member would force `import ...ServerAddress.Companion.checkLink`
  in all five view files):
  `fun checkLink(link: String): ScannedLinkType?` decoding `CheckedLink`;
  `@Serializable sealed class ScannedLinkType` mirroring the JSON, with
  `Connection(val linkType: SimplexLinkType)` reusing the existing
  `SimplexLinkType` enum. Always reference it **qualified** as
  `ScannedLinkType.Connection`: the bare name is already taken in this package
  by `model.Connection` (ChatModel.kt:1991), which every consumer has in scope
  via `model.*`. **Imports:** three of the five Kotlin consumers have
  `chat.simplex.common.model.*` already (NewChatView.kt:35, MigrateToDevice.kt:16,
  ConnectDesktopView.kt:27); two do not and need `checkLink`/`ScannedLinkType`
  imported explicitly — `ScanCodeView.kt` (no `model` import at all) and
  `ScanProtocolServer.kt` (whose `model` imports are single-symbol, not a
  wildcard: `…parseServerAddress` at :5, deleted here, and `…UserServer` at :6,
  which must be **kept** — the success path still constructs `UserServer`).
  Each subclass **must** carry `@Serializable` *and* `@SerialName` with
  section 1's wire tag verbatim — `connection`, `server`, `fileDescription`,
  `desktopCtrl`, `verificationCode`. Without `@SerialName` kotlinx defaults to
  the fully-qualified class name, so **all five** tags fail to decode at
  **runtime** (not compile time), presenting as "the classifier always returns
  unknown". Template to copy: the `Format` hierarchy at ChatModel.kt:4857-4883,
  which annotates every subclass even where the simple name would match.

### 3. Kotlin UI — uniform scanner flow

`views/newchat/WrongQRCode.kt` shrinks to the alert only — no parsing:

```kotlin
// One message string per scanned type ("This is a X. To use it, <where>.";
// exception: Connection(relay) shows its own relay message — see
// Strings section). Title defaults to "Wrong QR code"; MigrateToDevice passes
// the neutral "Wrong link" title (its path is shared with paste).
fun showWrongQRCodeAlert(type: ScannedLinkType, title: String = generalGetString(MR.strings.wrong_qr_code))
```

Platform-neutral invariant for EVERY success branch below (both platforms): the
success path must consume the **same trimmed string** that was classified — not
the raw scan/paste text. The classifier's trimmed string never crosses the FFI
back, so **the UI owns the trim**: each site computes `trimmed` once (Kotlin
`text.trim()`, Swift `.trimmingCharacters(in: .whitespacesAndNewlines)`) and
passes that same value to `checkLink` AND onward on success; the Haskell-side
trim in `chatCheckLink` is defensive only, and must be ASCII-only
(`" \t\r\n"` — a `Char8.dropWhile isSpace` would half-strip a UTF-8 NBSP,
`0xC2 0xA0`, leaving a dangling byte; Kotlin/Swift trims strip NBSP fully, so
ASCII-only keeps the defensive trim strictly weaker than the UI trim).
Pre-PR the strict UI gates rejected whitespace-padded input, so passing raw
text onward would newly accept e.g. a padded server QR and **store** the
untrimmed string as the server address (`chat_parse_server` uses `parseAll`, so
it rejects padding on both ends — this one is a persistence bug, not a
command-parse one), or send a **leading**-whitespace link raw into
`/_connect plan`. Note the asymmetry: *trailing* whitespace is harmless on the
command paths where the scanned string is the **last token on the line**, since
`parseChatCommand` right-strips the whole command line (`Commands.hs:411`,
`B.dropWhileEnd isSpace`); there only leading whitespace breaks the `strP` after
the command prefix. That covers `/_connect plan`, `/_download info`,
`/connect remote ctrl` and `/_verify code` — but **not** `/_download`, which is
also in scope: Migrate's success path sends the stored link there
(MigrateToDevice.kt:630 → `"/_download $userId $url ${file.filePath}"`,
SimpleXAPI.kt:4161; iOS AppAPITypes.swift:411), and the URL sits *before* the
file path via `strP_`, which consumes exactly one space (`Commands.hs:5759`) —
so a trailing space there corrupts the next field. Trailing trim is therefore
required too, not optional; iOS is live-affected today because `.whitespaces`
does not strip newlines. (`/_connect plan` also stops being last-token when its
optional `resolve=`/`sig=` suffixes are non-default, SimpleXAPI.kt:4090-4093 —
not on the scanner path, but do not generalise the rule.)

**Sites that must change** (this is the complete list, both platforms; each
passes at least one untrimmed string today): ScanProtocolServer builds
`UserServer` from raw `text`; NewChatView passes raw text to connect;
ConnectDesktopView sets raw `sessionAddress`; ScanCodeView calls
`verifyCode(text)` raw; and `checkUserLink` must compute `val trimmed =
link.trim()` **once** at the top and pass it to `checkLink` and down the
FileDescription success path. This applies to the two-way pass-throughs too, on
both platforms: `connectDesktopAddress` and `verifyCode` receive the trimmed
string. The rationale is this feature's own invariant — the classifier and the
consumer must see the same string — not any downstream bug fix (see the
pre-existing-bug note below, which is out of scope here).

For `verifyCode` that is a deliberate small improvement over pre-PR: with a
**leading** newline the command line fails to parse entirely — `verifyCodeP`
halts at the newline (`Commands.hs:5895`) and the trailing `A.endOfInput` in
`chatCommandP`'s `choice` (`Commands.hs:5775`) then rejects the remainder, so
the core returns a bad-command error, surfaced as the same "Incorrect security
code!"; trimmed it succeeds. Trailing whitespace already worked, per the
right-strip above — do not write a regression test asserting otherwise.

**Pre-existing bug — NOT this PR; track in a separate PR.** Independently of
this feature, Kotlin `checkUserLink` already mishandles a padded migration
link: it trims the gate (MigrateToDevice.kt:528) and the state payloads
(:534/:535/:538) but passes the **raw** link to `readFromLink` at :529, which
sends it into `/_download info`. So a **leading**-whitespace migration link
silently loses `networkConfig` on `stable` today, skipping the Onion screen and
migrating with the device's own host modes (`getNetCfg()`) instead of the
link's. This is a one-line fix (`readFromLink(link.trim())`) plus a regression
test, on its own, and does not belong in this feature PR — file it separately.
(This PR's rewrite of `checkUserLink` happens to route `trimmed` down that path
too, so it would incidentally mask the bug; keep the standalone fix + test
anyway so the networkConfig regression is covered on its own merits and can land
independently of / ahead of this feature.) iOS is not affected: its
`readFromLink` is a `nil` stub (AppAPITypes.swift:2153-2156, `standaloneFileInfo`
has no live caller), so it never reads `networkConfig` at all.

Second invariant, **Kotlin only** (iOS scanner completions return `Void`), for
every Kotlin scanner branch below: each wrong-type and unknown alert branch
returns `false` from the `QRCodeScanner` callback — the pre-PR reject
convention. `false` means "not handled", so the code is not memoised
(`QRCodeScanner.android.kt:101-110` — `contactLink.value` is assigned only when
`onBarcode` returns true) and the same scan re-fires the alert after the
existing ~1s delay; it does NOT end the scan session, which only success side
effects do: `verifyCode()` succeeding, which dismisses the scanner modal from
VerifyCodeView.kt:45 (ScanCodeView's own `close` is `{}` at its only call site,
VerifyCodeView.kt:113), `onNext(...)` (ScanProtocolServer),
`connect(..., close)` (NewChatView), or — for ConnectDesktopView and
MigrateToDevice, which have neither — a state transition that re-composes the
view away from the scanner (`UIRemoteCtrlSessionState` written at
ConnectDesktopView.kt:502-506, read at :79 with the branches at :89/:104/:141;
`MigrationToState` written at MigrateToDevice.kt:527-538, with the scanner
composed only under `PasteOrScanLink` at :188). Do NOT add a `close()` to those
two: Migrate's `close` dismisses the whole migration modal, not the scanner.
Success branches keep their existing returns.

Scanners whose success path needs a *valid* value use a three-way branch;
scanners whose existing path already handles arbitrary input use a two-way
branch (wrong-known-type → alert; everything else → the pre-existing path
unchanged, so pre-PR behaviour is preserved exactly). `expected` comparison is
on the top-level kind (any `Connection` subtype matches the New-chat scanner):

- `NewChatView` (three-way) — `Connection` → call `connect()` directly
  (`planAndConnect`; the core re-parses authoritatively). NOT `verifyAndConnect`:
  its internal `strIsSimplexLink` markdown gate is a second, different decider —
  if it disagreed with the classifier the scan would be silently swallowed with
  no alert. Other known type → wrong-type alert; unknown → original "Invalid QR
  code / not a SimpleX link" alert.
- `ScanProtocolServer` (three-way) — `Server` → build `UserServer` directly from
  the scanned text. Do NOT keep the `parseServerAddress(text)` call: both
  platforms only null-check its result and construct the server from the raw
  string anyway (Kotlin ScanProtocolServer.kt:21-23, iOS
  ScanProtocolServer.swift:40-43), so it would be a redundant second decider of
  the type `chat_check_link` already determined. Unknown → "Invalid server
  address! / Check server address…" alert — on Kotlin this **restores** the
  pre-v1 alert that v1 regressed to the generic message; on iOS this is a
  small **deliberate behaviour change**: pre-v1 iOS passed the string into
  `addServer`, which validated it via `parseServerAddress` and on garbage
  dismissed the scanner then showed a **global** "Invalid server address! /
  Check server address…" alert (NewServerView.swift:118-151) — same wording,
  different flow. The v1 in-scanner gate is kept so the alert shows without
  dismissing the scanner, matching the other scanners. Other known type →
  wrong-type alert.
- `MigrateToDevice.checkUserLink` (three-way) — `FileDescription` → existing
  download path; unknown → the pre-existing "Invalid link" alert (unchanged
  from pre-PR and v1); other known type →
  wrong-type alert. `strHasSimplexFileLink` is deleted (both platforms):
  `FileDescriptionURI` is the authoritative decoder and **supersedes** the
  prefix check — it does not merely duplicate it. Declare this behaviour delta,
  which cuts both ways: the prefix check was too **loose** (a `desc`-less or
  corrupt `simplex:/file…` passed and failed late during download; now it is
  unknown → immediate "Invalid link", which is what §1's corrupt-link test
  asserts) and too **strict** (`ServiceScheme` accepts *any* https host, so
  `https://<other-host>/file#/?desc=…` is rejected today but decodes and
  downloads identically — the URI host is decorative). Both halves of this
  delta have a mandatory test in section 1.
- `ConnectDesktopView` (two-way) — other known type → wrong-type alert;
  `DesktopCtrl` **or unknown** → `connectDesktopAddress` (pre-PR behaviour: the
  core parses and reports its own error — no invented client-side message). The
  v1 `DESKTOP_ADDRESS_SCHEME` constant is deleted.
- `ScanCodeView` (verify contact, two-way) — other known type → wrong-type
  alert; `VerificationCode` **or unknown** → existing `verifyCode()` path (its
  `incorrect_code` "Incorrect security code!" alert on failure — `verifyCode`
  keeps running for unknown input rather than being short-circuited; same flow
  as pre-PR, except it now receives the trimmed string per the invariant
  above).

Explicit deletion list (verified: no other callers remain after the above):
`isSecurityCode`, `identifyQRCode`, the `ScannedQRCode` sealed class, the
`detectSecurityCode` flag, `showNotSimplexQRCodeAlert` (NewChatView's unknown
branch uses the pre-existing `invalid_qr_code` +
`code_you_scanned_is_not_simplex_link_qr_code` strings inline, as pre-v1), and
— now orphaned by the direct-`connect()` scanner path — `verifyOnly`,
`verifyAndConnect` and `strIsSimplexLink` (Kotlin NewChatView.kt:778-787,822;
iOS NewChatView.swift:849). The core owns all recognition; expected-type
exclusion makes the `detectSecurityCode` flag unnecessary.

### 4. Strings

Replace v1's 11 strings with 8 — titles + one complete message per type (review:
description + "where to scan" were split into two places for no reason). The
texts below are shown unescaped; in `strings.xml` they need Android escaping —
`&amp;` for `&` (single escape, as strings.xml:813 does — NOT `&amp;amp;`) and
`\'` for each apostrophe (`wrong_qr_security_code` has two: `contact\'s` and
`member\'s` — escape both; an unescaped `'` is an aapt2 build error). iOS
`NSLocalizedString` literals need neither — but iOS **must** use `%@`, not
`%s`, as the substitution token: `%s` is a C `char *` in `String(format:)` and
passing a Swift `String` yields garbage or a crash. Write
`String.localizedStringWithFormat(NSLocalizedString("This is a %@.", …),
linkType.description)`, as v1's WrongQRCode.swift:29 already does. The `%s` in
the texts below is the Android form:

- `wrong_qr_code` — "Wrong QR code" (title, kept)
- `wrong_link` — "Wrong link" (neutral title; new resource, see below)
- `wrong_qr_connection_link` — "This is a %s. To use it, open New chat, then
  scan or paste it there." (`%s` = `SimplexLinkType.description`, whose values
  already begin with "SimpleX" — "SimpleX contact address", "SimpleX group
  link", … — so the template must NOT add its own "SimpleX")
- `wrong_qr_server_address` — "This is a SimpleX server address. To use it, open
  Network & servers, Your servers, Add server, then Scan server QR code."
  (The "Add server" hop is required: "Scan server QR code" is a button *inside*
  that dialog — ProtocolServersView.kt:299, iOS :206 — and is the sibling of
  the "Chat relay" button the relay string names. v1's text omitted the hop;
  this string is authored fresh here, so fix it rather than inherit it.)
- `wrong_qr_migration_link` — "This is a link to migrate to another device. To
  use it, when setting up a new device, choose to migrate from another device."
- `wrong_qr_desktop_address` — "This is an address to connect to a desktop app.
  To use it, open Use from desktop and scan the QR code shown in the desktop
  app." (Deliberately names **no** control label — no single label is right on
  both screens the user might land on. Both platforms gate identically on
  `connectRemoteViaMulticast && remoteCtrls.isNotEmpty()`
  (ConnectDesktopView.kt:544-545, iOS :30) and differ only in the default
  (Android `false`, SimpleXAPI.kt:255; iOS `true`, :19), so think by screen,
  not platform: the **connect** screen (Android default, iOS first-time
  linkers) shows the scanner under a non-tappable section header "Scan QR code
  from desktop" (ConnectDesktopView.kt:355, iOS :332); the **searching** screen
  (iOS returning linkers, and Android users who enable "Discover via local
  network" — the toggle at ConnectDesktopView.kt:436, iOS :379, whose resource
  id is the misleading `discover_on_network`, strings.xml:2830) offers a button
  labelled "Scan QR code" (Kotlin :204, iOS :206).
  Naming the goal covers both, and additionally tells the user *which* QR to
  scan. It does not fix the leading "Use from desktop" clause on desktop, where
  that screen doesn't exist at all — that mismatch is the accepted case in the
  desktop note below.)
- `wrong_qr_security_code` — "This is a security code. To use it, open the chat,
  then the contact's or member's name, then Verify security code." (Not
  "contact's": the identical `VerifyCodeButton` also exists per group member
  (GroupMemberInfoView.kt:556, iOS GroupMemberInfoView.swift:603), reached via
  the member list rather than a contact name, and the classifier is shape-based
  so it cannot tell the two apart. Tapping a member in the chat opens member
  info directly — ChatView.kt:515, iOS ChatView.swift:198/:263 — so no
  group-info hop is needed; `GroupChatInfoView` has no such button. This string
  carries **two** apostrophes to escape.)

- `wrong_qr_relay_address` — "This is a %s. To use it, open Network & servers,
  Your servers, Add server, then Chat relay, and paste the address there."
  (`%s` = `SimplexLinkType.description` as above, so no hardcoded literal — the
  "SimpleX relay address" text stays solely in `simplex_link_relay`,
  strings.xml:109, and cannot drift on retranslation.)
  (Names the real labels on both platforms: `smp_servers_add` "Add server"
  opens the add dialog (ProtocolServersView.kt:180-186), whose `chat_relay`
  "Chat relay" button (strings.xml:3056, ProtocolServersView.kt:308; iOS
  :204-208) opens `NewChatRelayView`. The trailing "paste the address there"
  is load-bearing: unlike every other destination this feature names, the relay
  flow has **no scanner** — `NewChatRelayView` takes the address through a
  `TextEditor` only (ChatRelayView.kt:236, iOS ChatRelayView.swift:309) — so a
  user holding a QR must transcribe or paste it. Do NOT say "under Chat
  relays": that section lists only *existing* relays, is not an add path, and
  is not rendered at all when there are none, ProtocolServersView.kt:89-91.)
  Relay gets its **own**
  destination rather than the generic connection-link one: New chat *rejects*
  relay links (`planAndConnect` aborts with its own
  `relay_address_alert_title`/`_message` — ConnectPlan.kt:33-42, iOS
  NewChatView.swift:1322-1331), but a real relay add flow exists and accepts
  exactly the strings the classifier labels relay — `ChatRelayView.validRelayAddress`
  gates on `SimplexLinkType.relay` (ChatRelayView.kt:58-64, iOS
  ChatRelayView.swift:43), reached via Your servers → Add server → Chat relay
  (ProtocolServersView.kt:180-186 → :305). It is not Android-gated, so the
  destination exists on desktop too. Sending relay to "open New chat…" would be
  a dead end; sending it nowhere would make it the one known type the Design
  section's "This is a X. To use it, <where>." promise doesn't cover.
  (Scanning a relay QR in the New-chat scanner itself still classifies as
  `Connection` → `connect()` → the existing relay alert from `planAndConnect`.)
  **Also extend `relay_address_alert_message`** (strings.xml:3175, iOS literal
  NewChatView.swift:1324-1327) with the same destination clause: today it reads
  "This is a chat relay address, it cannot be used to connect." and names
  nowhere. Without this the app answers the identical relay code two ways —
  four scanners point at Your servers, while New chat (the scanner a user with
  a relay QR reaches for first, and on desktop the *only* New-chat entry, via
  paste) still dead-ends. One string, and it covers scan and paste on both
  platforms because both funnel through `planAndConnect`. Do NOT instead
  special-case relay in NewChatView's `Connection` arm: that duplicates the
  decider and misses paste.

MigrateToDevice's check path is shared with its *paste* button, so its
wrong-type alert uses a neutral title `wrong_link` — "Wrong link" — instead of
"Wrong QR code" (which would be nonsense for pasted text). The four pure
scanners keep the `wrong_qr_code` title. That makes 8 **new** strings: 2 titles
+ 5 type messages + the relay-specific message. One **existing** string is also
edited, not added: `relay_address_alert_message` gains a destination clause (see
the relay bullet above).

Wording is device-neutral where cheap ("open", not "tap"). Two contexts where
the "To use it…" destinations don't exist yet, both accepted (the type
identification is still the alert's value):

1. **Onboarding.** MigrateToDevice is reachable *before a profile exists* — the
   entry point is `WelcomeView.kt`'s `MigrateButton` (:201-224, mounted on both
   `CreateFirstProfileMobile` :242 and `CreateFirstProfileDesktop` :320), which
   sets `migrationState` and shows the view; SimpleXInfo.kt:97-98/:168-169 only
   *re-open* an already-started migration. Its scan and paste both route through
   `checkUserLink`, and all five cross-type messages name post-onboarding
   surfaces. Applies on every platform, not just desktop.
2. **Desktop**, below.

Desktop note: the
only desktop-reachable path is Migrate's paste — `QRCodeScanner`'s desktop
actual is an empty stub so no scanner callback ever fires on desktop, and every
scanner is additionally `appPlatform.isAndroid`-gated before it is composed —
externally for ScanProtocolServer (ProtocolServersView.kt:281),
ConnectDesktopView (chatlist/UserPicker.kt:354) and Verify security code
(VerifyCodeView.kt:110, gating the button that opens the ScanCodeView modal at
:111-113); inline in the view itself for New chat (NewChatView.kt:652) and
Migrate (MigrateToDevice.kt:207, which gates only the scanner — the paste field
below it is unconditional). That is all five. So the alert shown for a desktop
paste in Migrate can name screens that sit elsewhere on desktop, or don't exist
there at all ("Use from desktop"). Accepted: the type
identification is the alert's value; per-platform navigation strings are not
worth 5 extra resources for one obscure path.

### 5. iOS mirror

- `SimpleXChat/SimpleX.h`: `extern char *chat_check_link(char *str);`
- `SimpleXChat/API.swift`: `checkLink(_:) -> ScannedLinkType?` +
  `Decodable` enum, mirroring `parseServerAddress` (L170-188); connection
  subtype reuses the existing `SimplexLinkType`. Four mandates, all iOS-only
  failure modes on the platform we cannot compile locally:
  - **Import:** `Views/Chat/ScanCodeView.swift` imports only `SwiftUI` and
    `CodeScanner` (:9-10) — add `import SimpleXChat`. It compiles today only
    because v1's helpers live in the app target; `checkLink` does not. The
    other five consumers already import it.
  - **Visibility:** `checkLink` and `ScannedLinkType` must be `public` — they
    cross the SimpleXChat→Shared module boundary, like
    `ServerAddress` (APITypes.swift:218). Do NOT copy the visibility of
    `ParsedServerAddress` (API.swift:185): that wrapper is `internal` because it
    never leaves the framework. The `CheckedLink` wrapper likewise stays
    internal. Getting this wrong is a compile error in all six consumer files.
    (Its cases inherit `public` automatically — do not write `public case`,
    which Swift rejects.)
  - **Case names** must be section 1's wire tags verbatim — `connection`,
    `server`, `fileDescription`, `desktopCtrl`, `verificationCode` — since
    synthesized `Decodable` keys off the case name.
  - **Associated-value label** must be `linkType`:
    `case connection(linkType: SimplexLinkType)`. Synthesized `Decodable` keys
    off the label too (precedent: `AppAPITypes.swift:1410`
    `case invitationLink(invitationLinkPlan: …)`). An *unlabeled* payload — as
    v1 writes it at WrongQRCode.swift:19, `case connectionLink(SimplexLinkType)`
    — decodes against `_0`, compiles fine, and fails at **runtime**: connection
    links classify as unknown and every scanner silently loses its wrong-type
    alert.
- `Views/NewChat/WrongQRCode.swift` shrinks to the message mapping +
  `wrongQRCodeAlert`, which keeps its **single-argument** shape — its four
  *surviving* callers (ScanCodeView.swift:42, ScanProtocolServer.swift:46,
  NewChatView.swift:692, ConnectDesktopView.swift:147) all want the default
  "Wrong QR code" title. (v1's fifth call, NewChatView.swift:697, is the
  unknown branch replaced in the next bullet.)
  The surviving message mapping **changes contract**:
  `func wrongQRCodeMessage(_ type: ScannedLinkType) -> String` — it takes the
  classified type, not text, and is **non-optional**. v1's `-> String?`
  (WrongQRCode.swift:92) signalled "unknown", which each scanner's own branch
  now handles, so all six call sites collapse to a direct call: the four
  `if let msg = wrongQRCodeMessage(…)` sites (NewChatView.swift:690,
  MigrateToDevice.swift:210/:239, ScanCodeView.swift:41) and the two
  `?? notSimplexQRCodeMessage()` sites (ScanProtocolServer.swift:46,
  ConnectDesktopView.swift:416) — whose fallback this plan deletes, so keeping
  `String?` is a compile error. Relay needs no extra parameter: discriminate
  internally on `case .connection(let linkType)` → `linkType == .relay` and
  return §4's `wrong_qr_relay_address` message, mirroring Kotlin. Do NOT add a title parameter: iOS Migrate does not
  call this helper at all — it uses its own
  `MigrateToDeviceViewAlert.error(title:error:)` case (MigrateToDevice.swift:76,
  rendered :179-180), so the neutral-title edit there is simply changing the
  hardcoded `title: "Wrong QR code"` at :211 and :240 to `"Wrong link"`.
  `identifyQRCode`, `isSecurityCode`,
  `desktopAddressScheme`, `strHasSimplexFileLink`, `notSimplexQRCodeMessage`,
  the `ScannedQRCode` enum (WrongQRCode.swift:18-49), the `detectSecurityCode`
  parameter (:92, sole call site ScanCodeView.swift:41 — expected-type
  exclusion makes it unnecessary, and leaving it keeps a second shape-based
  decider alive on the one scanner that must not short-circuit)
  and `strIsSimplexLink` (NewChatView.swift:849) deleted
  (`MigrateToDevice.swift` gets back its own gate via `checkLink`).
- iOS NewChatView's **unknown** branch restores the pre-v1 alert —
  `mkAlert(title: "Invalid QR code", message: "The code you scanned is not a
  SimpleX link QR code.")` (pre-v1 NewChatView.swift:692), wrapped in its
  `SomeAlert` as before — **not** `wrongQRCodeAlert`, which v1 used and which
  retitles it to "Wrong QR code". This is the iOS twin of §3's Kotlin
  `invalid_qr_code` restore; without it the two platforms show different titles
  for the identical scan.
- The same **per-scanner branching as section 3** (three-way or two-way as
  specified there — NOT uniformly three-way; a literal three-way on
  ConnectDesktopView would reintroduce the v1 regression section 3 reverts, and
  on ScanCodeView would **newly** short-circuit `verifyCode` — which must keep
  running on unknown input, since core's `sameVerificationCode` ignores spacing
  and so accepts codes the shape recogniser rejects), presented through each
  view's **own** alert state (a
  global alert may not show over a modal scanner). iOS ScanProtocolServer's
  unknown branch shows the scanner-local alert
  `scanAlert = SomeAlert(alert: mkAlert(title: "Invalid server address!",
  message: "Check server address and try again."), id: "invalidServerAddress")`
  — the same wording
  as pre-PR's global alert (NewServerView.swift:147-150) and the twin of
  Kotlin's `smp_servers_invalid_address`/`smp_servers_check_address`. **Not**
  `wrongQRCodeAlert`, which v1 uses here and which retitles it "Wrong QR code"
  — the same defect the NewChatView bullet above guards against. This is the
  deliberate iOS behaviour change from section 3 (in-scanner alert instead of
  the pre-PR route-into-`addServer` dismiss-then-global-alert flow).
- The trimmed-string invariant from section 3 applies here too; iOS
  additionally has a subtle trap: `.whitespaces` excludes newlines
  (MigrateToDevice.swift:207-212,236-241), so a leading-newline paste would
  classify as `FileDescription` but keep the newline downstream — use
  `.whitespacesAndNewlines` everywhere a string is trimmed.
- iOS Migrate has **no shared `checkUserLink`**: the scan
  (MigrateToDevice.swift:204-219) and paste (:234-250) sites duplicate the gate
  inline. Apply the three-way branch, the neutral "Wrong link" title and the
  full trim at **both** sites (or extract a shared helper) — the scan site too
  uses "Wrong link", matching Kotlin, whose shared path shows it for scans.

## Verification / build order

1. Haskell: build the library and run the new `MobileTests` cases (x86_64 Linux
   toolchain available locally).
2. Kotlin, in this order:
   a. `./gradlew :common:compileKotlinDesktop` — compile check only; it does
      **not** touch native libs (common/build.gradle.kts has no cmake wiring).
   b. `bash ~/build/linux.sh` — the single desktop build command. Do NOT run
      `scripts/desktop/build-lib-linux.sh` by hand first: linux.sh runs it
      itself behind a fingerprint gate over `src/**/*.hs` +
      `libsimplex.dll.def` + `flake.nix` (all changed by this plan, so it will
      rebuild), and a manual run does not update that fingerprint — you would
      re-run the lib build for nothing (a relink at minimum, a full rebuild if
      your `cabal.project.local` didn't already match
      `scripts/cabal.project.local.linux`, which linux.sh overwrites), and the
      manual run's staged libs are wiped again by the in-script run.
      (`LINUX_BUILD_FORCE_REFRESH=1` forces the lib rebuild if ever needed.)
      Note `build-lib-linux.sh` also deletes `libapp-lib.so` along with the
      libs dir; it is regenerated by `cmakeBuildAndCopy` (a
      `:desktop:compileKotlinJvm` dependency, desktop/build.gradle.kts:165-181)
      during the `./gradlew createDistributable` that `make-appimage-linux.sh`
      runs — i.e. it comes back only if you go through the full script. Without
      it the app dies at `System.loadLibrary`, a different failure from the
      stale-lib one.
   c. Verify the symbol reached the **packaged** app — not the dist-newstyle or
      staged copies, which can be fresh while the AppImage ships a stale lib
      (linux.sh:350-360 documents this; it `rm -rf`s `release/main/app` each
      run to force repackaging). Check the AppDir copy — the one
      `appimagetool` packages; do NOT `find` for the name, it matches two
      files (`simplex/lib/…` and `AppDir/usr/lib/…`) and a quoted command
      substitution would hand `nm` one bogus argument and print nothing,
      reading exactly like "symbol missing":
      `nm -D apps/multiplatform/release/main/app/AppDir/usr/lib/app/resources/libsimplex.so | grep chat_check_link`
   d. Exercise the Migrate-to-device **paste** path (the only desktop-reachable
      `checkLink` call site — desktop scanners are stubs). A *stale*
      `libsimplex` fails **lazily at the first `chatCheckLink` call**, not at
      app load (`libapp-lib.so` is not linked `BIND_NOW`), so "the app
      launched" proves nothing.
3. Android APK: **blocked on a fresh arm64 cross-build of `libsimplex.so`** —
   the prebuilt `/home/user/libsimplex.so` lacks `chat_check_link`; there,
   unlike desktop, the app fails at `System.loadLibrary("app-lib")` (bionic has
   no lazy binding), so it won't even start.
4. iOS: not compiled here (no Xcode toolchain) — needs Mac/CI; wording of the
   "where" messages should be checked against live iOS navigation labels.

## Not done / out of scope

- No "use it anyway" action on the alert — informational only.
- ConnectDesktopView's **paste** field (`DesktopAddressView`,
  ConnectDesktopView.kt:369-409; iOS ConnectDesktopView.swift:337-362) keeps its
  pre-PR behaviour — not classified, so a wrong-type address pasted there still
  gets only the core's error while the scanner one section above names the type.
  Justification: the field is behind the **developer-tools** flag
  (ConnectDesktopView.kt:142, iOS :21/:163), so it is not user-reachable in a
  default install, and classifying it would mean duplicating the gate — scan and
  paste share the *action* (`connectDesktopAddress`, ConnectDesktopView.kt:489 /
  .swift:448) but not the gate, which is inline in each callback. Note trimming
  alone *would* be cheap there (one line inside the shared
  `connectDesktopAddress`); deliberately deferred with the classification.
- Commit hygiene: commit ONLY the files this plan names — which now include
  both `spec/architecture.md` files and `apps/multiplatform/spec/database.md`
  (see section 1); they are maintained
  artifacts with back-references from the code, so leaving them stale is not an
  option hygiene should protect. The shared working
  tree carries unrelated uncommitted modifications from other work streams
  (gradle.properties / android/build.gradle.kts local build patches,
  AppUpdater.kt .deb-arch filtering, tests/ChatTests/Groups.hs join-interrupt
  repro) that must NOT be included.
- Scope covers scanners **plus** MigrateToDevice's paste path — `checkUserLink`
  is shared by its scan and paste entry points (MigrateToDevice.kt:209-211,
  233-238; on iOS the two sites are duplicated inline — see section 5), so
  pasting e.g. a contact link into Migrate now gets the classified wrong-type
  alert (titled "Wrong link") instead of the bare "Invalid link" — an intended
  improvement, and the reason for the neutral title.
- **New chat's paste path is NOT covered — flagged for a scope decision, not
  settled.** It keeps `strConnectTarget` (NewChatView.kt:674, iOS
  NewChatView.swift:651) and its existing fallback — Android "Invalid link!" +
  "The text you pasted is not a SimpleX link." (`invalid_contact_link`
  strings.xml:925, `the_text_you_pasted_is_not_a_link` :1003); iOS the same
  without the exclamation mark (NewChatView.swift:660), a pre-existing
  divergence this plan does not touch. Consequences a reviewer should weigh
  before this merges:
  - On Android, New chat answers the *same* server address two different ways
    on one screen: the scanner names the type, the paste field 20px above says
    "not a SimpleX link".
  - On **desktop** the paste field is the only New-chat entry point at all
    (the scanner is Android-gated at NewChatView.kt:652, `PasteLinkView` at
    :648-650 is unconditional), so New chat — the highest-traffic link entry
    point — gets none of this feature there.
  - Extending it is one branch per platform: route the `null` arm of
    `strConnectTarget` through `checkLink` before the existing fallback,
    reusing the same strings and the neutral `wrong_link` title. It must stay
    a *fallback*, not a replacement: `strConnectTarget` also accepts
    `Format.SimplexName` (NewChatView.kt:840-842), which `checkLink` does not
    classify.
  Left out here only to hold the diff to what was authorised; say the word and
  it's a small addition.
