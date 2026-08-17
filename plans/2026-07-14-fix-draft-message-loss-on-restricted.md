# Fix draft loss when switching to a chat where user cannot send messages

**Branch:** `nd/fix-draft-message-loss-on-restricted` · **PR:** [#7239](https://github.com/simplex-chat/simplex-chat/pull/7239) (draft) · **Commit:** `9d4bb2188`

## Problem

A draft typed in the current chat is lost when another chat is opened where the user
cannot send messages (observer, channel subscriber, review by admins). Reproduces on
desktop only: the chat list is always visible, so `chat` and `chatId` change in the
same recomposition (`CenterPartOfScreen` passes `ChatModel.chatId` itself as
`currentChatId`, App.kt:389). Android is unaffected — `AndroidScreen` updates
`currentChatId` via a `snapshotFlow` collector, so the draft-saving effect fires a
frame before the chat swaps.

## Root cause

In `ComposeView.kt`, the effect clearing compose state of a non-sendable chat
(`LaunchedEffect(sendMsgEnabled)`) was composed *before* the draft-saving
`KeyChangeEffect(chatModel.chatId)`. `LaunchedEffect` bodies run in composition
order, so on a same-recomposition switch the clear effect wiped the live compose
state before it could be saved, and `KeyChangeEffect` then fell into its
else-branch, destroying any previously saved draft via `clearPrevDraft` and deleting
attachment files. Present since the effect was introduced (`7b362ff65`, #5922) —
not a recent regression.

## Fix

Move the clearing effect after `KeyChangeEffect` (6 lines moved + 2-line comment).
The draft is saved first; `clearCurrentDraft()` is then a no-op for it because
`draftChatId` no longer matches the opened chat. Preserved behavior: the opened
chat's own draft is still cleared when it cannot send, and the draft is still
cleared when the open chat itself becomes non-sendable (only the `sendMsgEnabled`
key changes). Incidental improvements: an active live message is now sent on switch
instead of dropped, and the `inProgress` branch no longer resurrects cleared text.

## Verification

- Headless Compose-runtime harness (scratchpad `TmpDraftLossEffectOrderTest.kt` /
  `TmpDraftFixVerifyTest.kt`): reproduced the bug (clear-before-save order observed),
  verified the fix across 5 scenarios (switch to non-sendable, in-chat demotion,
  non-sendable chat's own saved draft cleared, Android staggered path, sendable
  control). All pass; `:common:desktopTest` and `:common:compileKotlinDesktop` green.
- Manual desktop repro confirmed the bug and fix.
- Works with `privacySaveLastDraft` off: text discarded per the setting's contract,
  never saved; disabling purges the slot, so no stale-state interactions.

## iOS

Not reproduced on device (desktop repro path is safe on iOS: back-navigation fires
`ComposeView.onDisappear`, which saves the draft). A code-traced fix for in-place
chat switches (member info → "message", notification tap, "forwarded from" — where
`onDisappear` never runs) was removed from the PR and parked on
`nd/ios-draft-save-on-chat-switch` (commit `7b9b8d987`) pending device verification.
Decisive test: type a draft in a group → member info → "message" → back to group.
If the draft survives without the change, delete the side branch.

## Pre-existing issues found (not addressed, both platforms)

- Secondary (member support/reports) chat views share the group's chat id and race
  the primary on the single draft slot (Kotlin: unguarded `KeyChangeEffect` in
  secondary instances can clear the primary's just-saved draft when a modal is open
  during a switch; iOS: secondary `onDisappear` saves under the swapped chat id).
- Kotlin: drafts are not saved when a chat is closed to `chatId = null` on desktop
  (ChatView is disposed before `KeyChangeEffect` runs; only the forwarding case is
  saved via `DisposableEffect`).
- Switching between two non-sendable chats restores the target's draft into the
  disabled composer (clear effect does not re-fire on `false→false`).
