# Desktop Text Selection — Anchor by Item Id

## 1. The bug

`SelectionRange` stored two **positional** indices into the reversed merged-items list:

```kotlin
data class SelectionRange(
    val startIndex: Int,
    val startOffset: Int,
    val endIndex: Int,
    val endOffset: Int
)
```

`reversedChatItems` grows from the front: a new message is prepended at index 0, every existing item shifts +1. Selection indices were never adjusted, so once the user had a selection on a message and another message arrived (or was sent), the indices kept pointing to the same numerical positions while the items at those positions had changed. The highlight (and the copy result) silently moved onto neighbouring messages.

Same root cause for the deletion case: removing an item from the list left selection indices pointing into a different item.

## 2. Root cause

Selection is **about items**, not positions. Storing positions into a list whose front grows is structurally wrong. The data structure must encode the stable identity (`ChatItem.id`), not the volatile position.

Two ingredients are mandatory for any correct fix:

1. **Remember which items** are anchor and focus (their stable `ChatItem.id`s).
2. **Update the positional indices** when the list mutates, so that everything downstream that reads `range.startIndex` / `range.endIndex` (highlight rendering, copy iteration, snap, copy-button placement, anchor/focus detection in `setupItemSelection` / `setupEmojiSelection`, drag direction in `SelectionCopyButton`) stays correct.

Anything beyond this is structural overreach.

## 3. Approaches considered

| # | Approach | Note |
|---|----------|------|
| A | Replace positional indices with ids in `SelectionRange`; cache items on the manager via `mutableStateOf`; expose indices via `derivedStateOf`; rename every reader from `range?.startIndex` to `manager.startIndex`; move top-level `selectedRange` into the manager as a method. | Structurally clean (single source of truth = ids), but renames every reader and moves a function for no behaviour reason. Ripples through `setupItemSelection`, `setupEmojiSelection`, `SelectionCopyButton`, `getSelectedCopiedText`, `snapSelection`, `copyButtonOffset`. |
| B | Same as A but replace the cached `var items` with `var mergedItemsState: State<MergedItems>?` (mirrors the existing `listState` field; eliminates duplicated state and the items-sync line in `SideEffect`). | Marginal improvement; the cost is still the renames and the function move, neither of which the bug requires. |
| C | **Final** — keep positional indices in `SelectionRange`, **add** `startItemId, endItemId` alongside them; resync the indices to the items they were anchored to on every recomposition via a `SideEffect`. | Every existing reader of `range.startIndex` / `range.endIndex` keeps working unchanged. The fix is a pure addition. |

Approach C accepts one piece of structural duplication that A and B do not have: anchor ids and positional indices coexist in `SelectionRange`, kept consistent by `resyncIndices`. For a bug-fix change, the trade-off favours diff minimality — migrating to a single source of truth (ids only, indices derived) is a separate refactor that should not be bundled with a fix.

## 4. Final implementation

### 4.1 `SelectionRange` — two new fields

```kotlin
data class SelectionRange(
    val startIndex: Int,
    val startItemId: Long,   // NEW — stable anchor for the selection start
    val startOffset: Int,
    val endIndex: Int,
    val endItemId: Long,     // NEW — stable anchor for the selection focus
    val endOffset: Int,
)
```

Existing `r.copy(startOffset = …)`, `r.copy(endOffset = …)`, `r.copy(startOffset = …, endOffset = …)` calls in `setAnchorOffset` / `updateFocusOffset` / `snapSelection` automatically preserve the new fields (data-class `copy` semantics). No change to those methods.

### 4.2 `SelectionManager` — one new field, two body additions, one new method

```kotlin
var mergedItemsState: State<MergedItems>? = null   // mirrors existing listState
```

`startSelection` looks up the id once at click time:

```kotlin
fun startSelection(startIndex: Int, anchorY: Float, anchorX: Float) {
    val id = mergedItemsState?.value?.items?.getOrNull(startIndex)?.newest()?.item?.id ?: return
    range = SelectionRange(startIndex, id, -1, startIndex, id, -1)
    selectionState = SelectionState.Selecting
    anchorWindowY = anchorY
    anchorWindowX = anchorX
}
```

`updateFocusIndex` updates `endItemId` whenever it updates `endIndex` (called both from `updateDragFocus` and from the scroll snapshotFlow — both paths covered by this single method):

```kotlin
fun updateFocusIndex(index: Int) {
    val r = range ?: return
    val id = mergedItemsState?.value?.items?.getOrNull(index)?.newest()?.item?.id ?: return
    range = r.copy(endIndex = index, endItemId = id)
}
```

New method:

```kotlin
fun resyncIndices() {
    val r = range ?: return
    val items = mergedItemsState?.value?.items ?: return
    val newStartIndex = items.indexOfFirst { it.newest().item.id == r.startItemId }
    val newEndIndex = items.indexOfFirst { it.newest().item.id == r.endItemId }
    if (newStartIndex < 0 || newEndIndex < 0) clearSelection()
    else range = r.copy(startIndex = newStartIndex, endIndex = newEndIndex)
}
```

### 4.3 `SelectionHandler` — three new lines

```kotlin
manager.listState = listState
manager.mergedItemsState = mergedItems        // NEW — wires items into the manager
manager.onCopySelection = { … }

// Resync after the items list mutates (new message arrives, item deleted).
SideEffect { manager.resyncIndices() }        // NEW — the trigger
```

### 4.4 What is *not* changed

- `selectedRange(range, index)` — still a top-level function with its existing signature.
- `getSelectedCopiedText(items, revealedItems, linkMode)` — same signature, same body.
- `snapSelection(items, linkMode)` — same signature, same body.
- `copyButtonOffset(...)` — uses `r.endIndex` directly; no change.
- `setupItemSelection`, `setupEmojiSelection`, `SelectionCopyButton` — every `range?.startIndex` / `range?.endIndex` reference is preserved verbatim.
- `startDragSelection`, `updateDragFocus`, `startSelection` (signature), `updateFocusIndex` (signature) — unchanged. `mergedItemsState` is reached via the manager's own field, so callers don't thread items.

This is the structural property that compresses the diff: callers see no API change, and the file's structure (top-level `selectedRange`, top-level `selectedItemCopiedText`, top-level `snapOffset`, top-level extension helpers) is untouched.

## 5. Why this works in Compose

`SideEffect { manager.resyncIndices() }` runs after every successful composition of `SelectionHandler`. `SelectionHandler` returns a `Modifier` (non-Unit return → non-skippable), so it re-runs whenever its caller (`ChatView`) re-runs, which `ChatView` does whenever `mergedItems.value` changes (it iterates the items list directly). Within the same Compose frame, the `SideEffect` mutation of `range` invalidates the children that read `range`, and Compose re-runs them to convergence before commit. Net visible result: the selection highlight stays on the originally selected items on the same frame the new message arrives — same fidelity as a `derivedStateOf`-based approach, no observable lag.

`mergedItemsState` is a plain `var` (not `mutableStateOf`) — this is fine because (a) it is reassigned on every recomposition of `SelectionHandler` to the same `State<MergedItems>` reference, and (b) the values inside it are read through `State.value`, which Compose tracks. The pattern is identical to the existing `var listState: State<LazyListState>? = null` field on the manager.

## 6. Behaviour changes — full inventory

1. **Selection follows the original messages when the items list mutates.** This is the bug fix.
2. **Selection clears if either anchor item is removed from the list** (e.g. message deleted from another session). Previously, indices silently slid onto neighbouring messages. The new behaviour is `clearSelection()` when `indexOfFirst` returns -1. This is a side-effect of anchoring by id — once the anchor is gone, "the selection" is no longer well-defined. It is the same class of bug as #6.1 and is fixed by the same mechanism.
3. **Defensive `?: return` in `startSelection` and `updateFocusIndex`** when the id lookup fails. In practice this branch is unreachable: `mergedItemsState` is wired before any user input; the index passed in always comes from `resolveIndexAtY` (which only returns visible-item indices); `newest().item` is non-null for any merged item. No observable change, but worth flagging for completeness.

Nothing else changes. Verified by reading the diff against master line-by-line.

## 7. Verification

1. **Linux desktop build** succeeded end-to-end, producing `SimpleX_Chat-x86_64.AppImage`. No compilation errors, no Compose runtime issues from the new field on the manager or the new fields on `SelectionRange`.
2. **Manual flow against the test plan**: selection persists across `new-message-arrives`, `new-message-sent`, multi-item span; deletion clears (see §6.2); drag-select & copy button behaviour preserved.

## 8. Trade-offs and follow-ups

The two pieces of structural debt this change knowingly leaves in place:

1. **Anchor ids and positional indices coexist in `SelectionRange`.** Single source of truth would store only ids and derive indices on read. The cost of unifying is the rename and function-move churn, which is independent of this bug. A follow-up could collapse these into ids-only without behaviour change, scoped to its own commit.
2. **`resyncIndices` runs on every recomposition of `SelectionHandler`.** The two `indexOfFirst` calls are O(n) on the items list. If profiling ever shows this on a hot path, the cheap fix is to gate on the pointer identity of the items list (`if (lastResyncedItems !== items) { … }`) — one extra field, one branch. Not worth doing speculatively.
