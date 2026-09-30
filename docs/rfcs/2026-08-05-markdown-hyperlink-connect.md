# Connecting via a SimpleX link written as a markdown hyperlink

## Problem

Pasting a short SimpleX link written as a markdown hyperlink — `[label](https://smp6.simplex.im/a#...)` — into the chat list search, the new chat sheet search, or "Tap to paste link" fails with "Invalid connection link" instead of connecting.

## Cause

`markdownP` parses such a link into a single fragment whose `format` is `SimplexLink` but whose `text` is the whole markdown source:

```
[{"format":{"type":"simplexLink","showText":"label","linkType":"contact",
  "simplexUri":"simplex:/a#...?h=smp6.simplex.im","smpHosts":["smp6.simplex.im"]},
  "text":"[label](https://smp6.simplex.im/a#...)"}]
```

`strConnectTarget` returns that `text` as the string to connect with. For a bare link `text` is the link, so it works; for a hyperlink it is `[label](link)`, which the core rejects as `InvalidConnReq`.

## Design

Use `simplexUri` — the link the parser already resolved — when the fragment came from the hyperlink parser, and keep using `text` otherwise:

```
text = if showText != null then simplexUri else text
```

`showText` is an exact discriminator, not a heuristic: `simplexUriFormat` is called with `Just t` only from `sowLinkP` (the hyperlink parser) and with `Nothing` from `wordMD` (bare link). Gating on it leaves every bare-link path unchanged.

This also matches how the chat item renderer already resolves the same format — `TextItemView.kt` takes `simplexUri`, never the fragment `text`, when `showText` is set. `strConnectTarget` was the outlier.

## Scope

Short links only. `sowLinkP` rejects a full link inside a hyperlink (`fail "full SimpleX link in hyperlink"`), so `[label](full-link)` yields no formatting at all and never reaches this code — it stays treated as search text, as before. Bare full links are unaffected.
