# Fix `/start remote host` parser when iface name contains a space

## Problem

Picking a non-default interface (e.g. Windows `Ethernet 2`) on the "Link a mobile" screen and refreshing the QR code returns `error chat: error chat commandError Failed reading: empty`. The desktop UI sends `/start remote host new addr=… iface="Ethernet 2" port=…`; the chat backend rejects it as an unparseable command. Without a workaround the user can't pin a specific local interface for the mobile-link controller.

## Cause

`rcCtrlAddressP` parses the iface value with `jsonP <|> text1P` (`src/Simplex/Chat/Library/Commands.hs:5549`). `jsonP` calls `A.takeByteString`, consuming *all* remaining input, then runs `eitherDecodeStrict'`. When `port=…` follows `iface=…` the strict decode fails because the JSON value `"Ethernet 2"` has trailing junk after it, so attoparsec backtracks to `text1P` (`takeTill (== ' ')`). `text1P` stops at the first space — inside the JSON quotes — leaving `2" port=12345` which nothing downstream can consume, `A.endOfInput` fails, the whole `A.choice` exhausts and surfaces attoparsec's `empty` message. With an iface name that has no space (`"lo"`) the bug is invisible: text1P swallows the full quoted token and the rest parses, but the interface name is stored with literal quotes so the iface preference silently never matches a real adapter anyway.

## Fix

Replace `jsonP` with a bounded `quotedP` that consumes only the bytes between `"…"` and leaves trailing fields for the next parser. `text1P` is kept as the unquoted fallback. Two-line change in `Commands.hs` plus a pure regression test in `tests/RemoteTests.hs` that asserts `parseChatCommand` of `/start remote host new addr=192.168.1.5 iface="Ethernet 2" port=12345` produces `RCCtrlAddress _ "Ethernet 2"` with port `12345`.
