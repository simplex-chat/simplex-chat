# Desktop: text selection copies non-message event items

Branch: `nd/fix-copy-non-msg-items` · code commit `a536452ca` · PR [#6993](https://github.com/simplex-chat/simplex-chat/pull/6993).

## 1. Problem statement

The Desktop "select text in messages" feature (PR [#6725](https://github.com/simplex-chat/simplex-chat/pull/6725)) lets the user drag a selection across several message bubbles and copy it. When the selection spans a chat event/info item — a "connected" event, a member "joined"/"left" event, a call event, an e2ee-info line, a feature-change line — the copied text includes that item's text, even though the item is never shown highlighted as part of the selection.

Expected: only real message text is copied. Observed: event/info text such as "connected" is appended into the clipboard between the selected messages.

### Privacy note

Event/info item text is produced from localized string resources (`generalGetString`, `RcvConnEvent.text`, etc.) — it is rendered in the language the user has chosen for the app, whereas real message text is not. A user who selects and copies a long span of messages carelessly, then pastes it into another chat or app, can therefore leak their chosen interface language through the event lines mixed into the paste. For a privacy-focused messenger this is a metadata leak, not only a cosmetic bug.

## 2. Root cause

`SelectionManager.getSelectedCopiedText` (`TextSelection.kt`) builds the copied string by iterating every merged-item index between the selection bounds and emitting each item's text:

```kotlin
return (lo..hi).mapNotNull { idx ->
    val ci = items.getOrNull(idx)?.newest()?.item ?: return@mapNotNull null
    if (ci.meta.itemDeleted != null && ...) return@mapNotNull null
    val sel = selectedRange(range, idx) ?: return@mapNotNull null
    selectedItemCopiedText(ci, sel, linkMode)
}
```

For an *interior* item in a multi-item selection, `selectedRange` returns `0 until Int.MAX_VALUE` — the whole item is treated as selected — so its text is emitted unconditionally. The only items previously skipped were deleted ones.

Anchor/focus character tracking (`setupItemSelection`) is wired up only for real message views (`FramedItemView`, `EmojiItemView`); event/info items never register offsets and never compute a highlight range. So an event item caught between two selected messages is invisible to the highlight but fully visible to `getSelectedCopiedText`. The copy logic and the on-screen selection disagreed.

The distinguishing property: a real message has `ci.content.msgContent != null`; every event/info `CIContent` variant returns `msgContent == null`.

## 3. Solution summary

`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/TextSelection.kt` — one guard, +3 lines.

In `getSelectedCopiedText`, skip any item whose `content.msgContent` is null, alongside the existing deleted-item filter:

```kotlin
if (ci.content.msgContent == null) return@mapNotNull null
```

Only real messages now contribute copied text — which is exactly the set of items that are selectable and highlighted, so the clipboard matches the visible selection. `content.msgContent` is the existing model property used elsewhere to tell a real message apart from an event/info item.

## 4. Alternatives considered (and rejected)

- **Special-case only "connected" events.** Matches the literal report but leaves the identical bug for every other event/info item (joined/left, calls, e2ee info, feature changes) — same class, same language leak.
- **Make event items non-selectable / consume the drag.** Larger change to the selection gesture; event items are already non-anchorable, and the bug is purely in the copy aggregation, not in the gesture.
- **Filter at the call site (`onCopySelection`).** Duplicates the message/non-message distinction outside the one function that owns copied-text assembly; `getSelectedCopiedText` is the correct single source.
