# "Remove link tracking" strips whitelisted query parameters (`?list=` in YouTube links, github `ref`)

Design doc for the fix in PR #6965 (`nd/fix-list-in-link` → `master`).

## Problem — what prompted this

With **"remove link tracking"** enabled (Settings → Privacy & security), sending a message
with a YouTube link that has a `list` query parameter — `https://www.youtube.com/playlist?list=PL...`
or a video-in-playlist link `https://www.youtube.com/watch?v=...&list=PL...` — sent the URL
with `?list=...` removed, so the recipient got a plain (non-playlist) link instead of the
playlist. Fixing that `?list=` stripping is the immediate purpose of this change.

iOS, Android and desktop are all affected — the URI sanitiser lives in the shared Haskell
core (`src/Simplex/Chat/Markdown.hs`).

## Cause

"Remove link tracking" on send uses *safe mode* of `sanitizeUri`:
`ComposeView.sanitizeMessage` → `parseSanitizeUri(_, safe = true)` → `chatParseUri 1` →
`sanitizeUri True`.

`sanitizeUri` has three branches that pick which query parameters to keep; two of them
already consult `qsWhitelist` (the list of parameter names known *not* to be tracking — `q`,
`search`, `list`, `page`, youtube's `v`/`t`, github's `ref`, …):

```haskell
let sanitizedQS
      | safe       = filter (not . isSafeBlacklisted . fst) originalQS   -- ← whitelist NOT consulted
      | isNamePath = case originalQS of
          p@(n, _) : ps -> (if isWhitelisted n || not (isBlacklisted n) then (p :) else id) $ filter (isWhitelisted . fst) ps
          [] -> []
      | otherwise  = filter (isWhitelisted . fst) originalQS
...
isSafeBlacklisted p = any (`B.isPrefixOf` p) qsSafeBlacklist
qsSafeBlacklist = [ "ad", "af", ..., "li", ..., "ref", ... ]  -- name *prefixes*; "li" → LinkedIn (li_fat_id, lipi, licu)
```

The safe-mode branch is the odd one out: it drops a parameter whenever its name *starts
with* a known tracking prefix, and never looks at `qsWhitelist`. So `list` was dropped
because `"li"` is a tracking prefix, and github's whitelisted `ref` was dropped because
`"ref"` is itself a tracking prefix — even though both are explicitly listed as non-tracking
and are kept by every other branch.

## Fix

Make the safe-mode branch apply the same "whitelisted *or* not blacklisted" rule the other
branches already use:

```haskell
| safe = filter (\(n, _) -> isWhitelisted n || not (isSafeBlacklisted n)) originalQS
```

This *removes* a special case rather than adding one — `list` is no longer handled
differently from any other whitelisted parameter; `qsWhitelist` becomes authoritative in all
three branches. Effects relative to the previous behaviour:

- `list` is kept everywhere (the reported `?list=` bug);
- github's `ref` is kept on `github.com` in safe mode too (it was already kept in eager
  mode — it's in the whitelist for exactly that reason);
- nothing else changes: of all whitelist entries, only `list` (vs the `"li"` prefix) and
  `ref` (vs the `"ref"` prefix) collide with a `qsSafeBlacklist` prefix today;
- every actual tracking parameter is still stripped — `qsWhitelist` does not contain
  `li_fat_id`, `lipi`, `licu`, `utm*`, etc., and `ref` on any non-github host stays stripped.

Regression tests added in `testSanitizeUri` (`tests/MarkdownTests.hs`):

```haskell
it "should keep whitelisted parameters in safe mode even if they match a blacklist prefix" $ do
  "https://example.com/playlist?list=abc" `sanitized` Nothing -- "list" is whitelisted, "li" is blacklisted
  "https://example.com/playlist?list=abc&si=def" `sanitized` Just "https://example.com/playlist?list=abc"
  "https://github.com/owner/repo?ref=main" `sanitized` Nothing -- "ref" is whitelisted for github.com
```

Verified: full library + test rebuild, then `cabal run simplex-chat-test -- --match /sanitizeUri/`
→ 4 examples, 0 failures (the new block plus the three pre-existing `sanitizeUri` cases).

## Alternatives considered

- **Special-case `list`** (`isSafeBlacklisted p = p /= "list" && …`). Smallest possible diff,
  provably zero collateral, but it hard-codes one parameter name into a predicate and leaves
  the structural inconsistency (safe mode ignoring the whitelist) in place — a fix by
  exception rather than by rule. (This was the first version; replaced.)
- **Narrow the `"li"` blacklist entry to `"li_"`.** Fixes `list` but stops matching `lipi`
  and `licu` (real LinkedIn email-link params), i.e. changes more than `list` while still
  not addressing `ref` or the underlying inconsistency.
