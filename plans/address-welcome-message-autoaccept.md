# Address welcome message after Auto-accept toggle (Android, desktop)

## Problem

In Settings > SimpleX address > Address settings:
- Auto-accept turned off: the welcome message field keeps its text; the stored welcome message is empty.
- Message typed with Auto-accept off, then Auto-accept turned on: the field keeps the text, Save is disabled, the stored welcome message is empty.

## Cause

`AutoAcceptToggle` sets `autoReply = ""` and saves the settings at once.
`AutoReplyEditor` keeps the field text in `rememberSaveable` without a key; the text is copied only from the field to the settings.

## Fix

UserAddressView.kt:
- `AutoReplyEditor`: the field text is keyed on `autoAccept`, so each toggle re-creates it from the settings.
- `AutoAcceptToggle`, Auto-accept on: `autoReply` keeps its value.
- Auto-accept off: `autoReply = ""`, unchanged.

## Scope

Android and desktop (commonMain). iOS binds the field to the settings directly.
