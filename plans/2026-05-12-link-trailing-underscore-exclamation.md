# Links: trailing `_` and `!` dropped from the highlighted link

Design doc for the fix shipped in PR #6973.

## Problem

A bare URL or domain ending in `_` (or `!`) was highlighted as a link only up
to the last non-`_` character — the trailing `_` rendered as plain,
non-clickable text. For example `https://en.wikipedia.org/wiki/The_Lord_of_the_Rings_`
showed `…The_Lord_of_the_Rings` as a blue link followed by a separate, plain
`_`. Reported for `_`; the same defect applies to `!`.

## Background — how bare links are parsed

`parseMarkdown` (`src/Simplex/Chat/Markdown.hs`) splits a message into
fragments; a fragment that isn't a recognized markdown construct falls through
`wordP` → `wordMD`, which decides whether the "word" (the run up to the next
space) is a URI / SimpleX link / domain / email.

To handle the very common case of a link immediately followed by sentence
punctuation — `check out https://simplex.chat.` or `(https://simplex.chat)` —
`wordMD` peels a trailing run of "punctuation" off the word and re-emits it as
`unmarked` text:

```haskell
where
  punct   = T.takeWhileEnd isPunctuation' s
  s'      = T.dropWhileEnd isPunctuation' s
  res md' = if T.null punct then md' else md' :|: unmarked punct
```

`isPunctuation'` is `Data.Char.isPunctuation` with exemptions for characters
that legitimately *end* a URL: `/` (trailing path separator, e.g.
`https://github.com/simplex-chat/`) and `)` (Wikipedia disambiguation, e.g.
`…/wiki/Servo_(software)`).

All link-highlighting surfaces derive from the result of this parser: the
desktop/Android UI (`TextItemView.kt`) and iOS UI (`MsgContentView.swift`)
both call `chatParseMarkdown` (FFI into the bundled Haskell core) and style a
`Uri` / `HyperLink` fragment by its whole `text`; the compose-preview path uses
the same function; `Styled.hs` does the terminal rendering. So whatever the
parser puts inside the `Uri` fragment is exactly what gets highlighted, on
every platform.

## Root cause

`Data.Char.isPunctuation '_' == True` — `_` is Unicode `ConnectorPunctuation`
(`Pc`). `isPunctuation '!' == True` — `!` is `OtherPunctuation` (`Po`). Neither
was in the `isPunctuation'` exemption list, so a trailing `_` or `!` was always
stripped from the URI text.

For `https://simplex.chat/page_name_`:

- `punct = "_"`, `s' = "https://simplex.chat/page_name"`
- output: `Uri "https://simplex.chat/page_name" :|: unmarked "_"`

The UI highlights the `Uri` fragment by its text, so the `_` lands outside the
blue/clickable span — exactly the reported behaviour.

## Fix

Add `_` and `!` to the `isPunctuation'` exemptions, alongside `/` and `)`:

```haskell
isPunctuation' = \case
  '/' -> False
  ')' -> False
  '_' -> False
  '!' -> False
  c -> isPunctuation c
```

`T.takeWhileEnd isPunctuation'` now stops at a trailing `_`/`!`, so the full
token is kept in `s'` and emitted as a single `Uri` fragment. Anything still
trailing it (`.`, `,`, ` …`) is peeled off as before:

- `https://simplex.chat/page_name_` → `Uri "https://simplex.chat/page_name_"`
- `https://simplex.chat/page_name_, hello` → `Uri "…/page_name_" :|: unmarked ", hello"`
- `https://simplex.chat/page!` → `Uri "https://simplex.chat/page!"`

## Why this is the right place

- `wordMD`/`isPunctuation'` is the single point where bare-link text is
  trimmed, and it already encodes "these characters legitimately end a link."
  `_` and `!` belong in that list next to `/` and `)`.
- `_` and `!` are RFC 3986–valid URL characters (`_` is in `unreserved`, `!` is
  a `sub-delim`); `_` is never sentence-ending punctuation.
- Fixing it in the parser fixes every surface at once (desktop, Android, iOS,
  terminal, compose preview), because they all consume the same `FormattedText`.
  A UI-layer patch would have to be repeated per platform and would leave
  `Styled.hs` wrong.

## Why a wider change is not in scope

- The reported bug is fully resolved by the two-line addition to the exemption
  `case`. Nothing more is required.
- `isPunctuation'` is shared by the URI, domain and email branches of `wordMD`.
  Exempting `_`/`!` for all three is the intended behaviour, with one minor
  knock-on: `user@example.com!` now renders as plain text rather than
  `Email "user@example.com" :|: unmarked "!"`, because `user@example.com!`
  isn't a valid email so the whole token isn't recognized. This is acceptable —
  `!` is now treated consistently as part of the token everywhere — and is
  preferable to splitting `isPunctuation'` into a URI predicate and an email
  predicate, which adds structure for a marginal case. Phones are unaffected
  (`phoneP` is a separate parser that doesn't use `isPunctuation'`).
- `good-code-v5.md` — *"Find the minimal change … the smallest structural
  modification that achieves the goal."* The smallest modification that
  resolves the report is two lines in the exemption `case`.

## Backward compatibility

Pure parsing change, no wire-format impact. `FormattedText` keeps the same
shape; only which characters fall inside a `Uri`/`Email` fragment changes.
Messages already stored keep their previously-parsed formatting — re-parsing
happens on compose / receive, not on display of stored items. An old client
receiving a message authored by a fixed client parses the raw text itself and
behaves per its own (older) rule — no incompatibility either way.

## Verification

`tests/MarkdownTests.hs`, `describe "text with Uri"` — four cases added:

- `"https://simplex.chat/page_name_" <==> uri "https://simplex.chat/page_name_"`
  — the trailing `_` is part of the link.
- `"https://simplex.chat/page_name_, hello" <==> uri "https://simplex.chat/page_name_" <> ", hello"`
  — `_` kept, the `, hello` after it still peeled off.
- `"https://simplex.chat/page!" <==> uri "https://simplex.chat/page!"`
- `"https://simplex.chat/page!, hello" <==> uri "https://simplex.chat/page!" <> ", hello"`

`MarkdownTests` suite: 38 examples, 0 failures. The existing exemption / peel
coverage is unchanged — `…/simplex-chat/`, `…/wiki/Servo_(software)`,
`https://simplex.chat.` → link + `.`, `https://simplex.chat, hello` → link +
`, hello`, etc.

Manual sanity (desktop, Linux AppImage build): a message containing
`https://en.wikipedia.org/wiki/The_Lord_of_the_Rings_` highlights the whole URL
including the trailing `_`.

## Alternatives considered and rejected

- **Split `isPunctuation'` into a URI predicate and an email predicate** so `!`
  is kept only inside URLs. Adds a second predicate and a branch solely to
  preserve `Email "x@y.z" :|: unmarked "!"` on `x@y.z!` — a marginal case. The
  shared predicate is simpler; rejected.
- **Strip `_`/`!` only when followed by more URL-looking text.** Requires
  look-ahead the trailing-trim model doesn't have, for no real benefit — `_`
  and `!` aren't sentence punctuation in the first place.
- **Extend the link span over a trailing `_` in the UI layer.** Wrong layer:
  the parser is the single source of truth for `FormattedText`, consumed by
  three platforms plus the terminal renderer; a UI-only patch would diverge per
  platform.
