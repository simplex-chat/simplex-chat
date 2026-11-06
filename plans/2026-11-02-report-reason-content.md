# Report reason "content"

## Problem

The apps send the "Inappropriate content" report reason as `illegal`. The core parses `illegal` as an unknown reason, so CLI moderators see `report illegal` and bots receive the reason `illegal`. A report with reason `content`, sent from the CLI or a bot, is shown in the apps as the raw word `content`.

## Cause

The core has named this reason `content` (`RRContent`, `src/Simplex/Chat/Protocol.hs`) since #5469. The Kotlin and iOS apps encode and decode it as `illegal`. On iOS the `/_report` command is built by interpolation of the enum case name instead of its encoding.

## Fix

Core: `strP` of `ReportReason` parses `illegal` as `RRContent`. Reports stored or received with `illegal` are read as `content`; unsigned reports are forwarded as `content`. A desktop controlling an older mobile host also receives `content`.

Kotlin (`ReportReasonSerializer` and the `@SerialName` of `ReportReason.Illegal`): `ReportReason.Illegal` is encoded and decoded as `content`.

iOS (`ReportReason`): `cmdString` returns the reason's API value (`content` for `.illegal`), which is used in the JSON encoding and in the `/_report` command. `content` is decoded as `.illegal`.

`docs/protocol/simplex-chat.schema.json`: the reason enum contains `spam`, `content`, `community`, `profile`, `other`.

## Scope

- The stored `item_text` of reports received or sent before the fix is left as `report illegal`.
- Apps older than this fix show as `content` the reports sent from fixed apps and the unsigned reports that a fixed member or relay forwards; an older desktop controlling a fixed mobile host shows all reports with this reason as `content`.
