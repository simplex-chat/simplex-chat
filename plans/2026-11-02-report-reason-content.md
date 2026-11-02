# Report reason "content"

## Problem

The apps send the "Inappropriate content" report reason as `illegal`. The core parses `illegal` as an unknown reason, so CLI moderators and bots see `report illegal`. A report with reason `content`, sent from the CLI or a bot, is shown in the apps as the raw word `content`.

## Cause

The core names this reason `content` (`RRContent`, `src/Simplex/Chat/Protocol.hs`) since #5469. The Kotlin and iOS apps encode and decode it as `illegal`. On iOS the `/_report` command interpolates the enum case name instead of its encoding.

## Fix

Core: `strP` of `ReportReason` parses `illegal` as `RRContent`. Reports stored, received or forwarded with `illegal` are read and sent as `content`, also to a desktop controlling an older mobile host.

Kotlin (`ReportReasonSerializer`): `ReportReason.Illegal` is encoded and decoded as `content`.

iOS (`ReportReason`): `cmdString` returns the reason's API value (`content` for `.illegal`); the JSON encoding and the `/_report` command use it. `content` is decoded as `.illegal`.

`docs/protocol/simplex-chat.schema.json`: the reason enum lists `spam`, `content`, `community`, `profile`, `other`.

## Scope

- Stored `item_text` of reports received before the fix stays `report illegal`.
- Apps older than this fix show as `content` the reports sent from fixed apps and the reports that a fixed member or relay forwards.
