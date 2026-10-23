# Fix copying selected text in reports

PR: [#6863](https://github.com/simplex-chat/simplex-chat/pull/6863) · branch `nd/fix-select-in-reports` · final commit `96d6f3222`

## 1. Problem statement

Report items in desktop render as a red italic *reason prefix* followed by the user's comment, e.g. `Spam: hi @alice`. The user reported that selecting `Spam: test` and pressing Ctrl-C / clicking the copy button placed only `test` on the clipboard — the `Spam: ` prefix was silently dropped. Selecting *only* the prefix produced an empty clipboard.

A second symptom existed for any report whose comment contained a transformed segment (mention with `localAlias`, link with `showText`): dragging a selection boundary inside that segment snapped to the wrong character on release, then copy emitted the wrong text.

Both symptoms have a single cause and the bug is desktop-only because the touch UI does not use this selection path.

## 2. Solution summary

`MarkdownText` builds the on-screen `AnnotatedString` as `[prefix][body]` (one composable, one layout). Compose's `layout.getOffsetForPosition(...)` therefore returns selection offsets in **display-text space**, which includes the prefix. Pre-PR, `selectedItemCopiedText` and `snapOffset` walked `ci.formattedText` from `displayOffset = 0` — i.e. they treated those offsets as **prefix-excluded body offsets**. Every offset for a report was off by `prefix.length`.

The fix is one structural realisation: the prefix is the **leading display-space segment**, so the loop that walks `ci.formattedText` must start at `displayOffset = prefix.length`, and any portion of the selection that falls in `[0, prefix.length)` must be emitted by appending a slice of the prefix string before the loop runs.

To prevent the same silent decoupling from re-emerging, the prefix string itself is extracted into a single source of truth — `itemPrefixText(ci)` — used by every call site that either renders or measures the prefix.

## 3. Detailed tech design

### 3.1 Where the offsets come from

```
Compose layout
  └─> SelectableText / ClickableText (TextItemView.kt)
       └─> getOffsetForPosition(localPos)   // returns display-space offset
              └─> SelectionManager.setAnchorOffset / updateFocusOffset
                     └─> selectedRange(...) → IntRange in display space
                            └─> selectedItemCopiedText(ci, sel, linkMode)   // FIX site #1
                            └─> snapOffset(ci, off, linkMode, expandRight)  // FIX site #2
```

`MarkdownText` (TextItemView.kt) builds the `AnnotatedString` in this order:

```
inlineContent                — never present for report items
appendSender(...)            — null for the CIMarkdownText path
prefix (AnnotatedString)     — "${reason}: " for reports, null otherwise
text / formatted segments    — the body
typingIndicator (live only)  — past selectableEnd
reserve (timestamp space)    — past selectableEnd
```

For non-report items the prefix is null and the existing identity `displayOffset = 0` holds. For reports, the body's first character lives at display offset `prefix.length`.

### 3.2 The minimal structural change

Pre-PR loop:

```kotlin
var displayOffset = 0
for (ft in formattedText) {
    val segDisplay = itemSegmentDisplayText(ft, ci, linkMode)
    val displayEnd = displayOffset + segDisplay.length
    val overlapStart = maxOf(displayOffset, sel.first)
    val overlapEnd = minOf(displayEnd, sel.last + 1)
    if (overlapStart < overlapEnd) { /* emit */ }
    displayOffset = displayEnd
}
```

Two changes only:

1. **Seed with prefix length.** `var displayOffset = prefix.length` (or `itemPrefixText(ci).length` for `snapOffset`). Loop body is otherwise byte-for-byte identical to pre-PR. For non-reports `prefix.length == 0`, so the non-report path is unchanged.

2. **Emit the prefix slice.** Before the loop, append the portion of `prefix` covered by the selection:
   ```kotlin
   if (sel.first < prefix.length) {
       sb.append(prefix, sel.first, minOf(prefix.length, sel.last + 1))
   }
   ```
   `selectedRange()` guarantees `sel.first ≥ 0`, so no clamping is needed at this site.

3. **Handle the `formattedText == null` branch.** Reports with empty body have null `formattedText`, but the prefix selection still has to be returned. The early-return in the pre-PR null branch is replaced by the same `StringBuilder` path so prefix-only selections work:
   ```kotlin
   val formattedText = ci.formattedText ?: run {
       val start = (sel.first - prefix.length).coerceAtLeast(0).coerceAtMost(ci.text.length)
       val end = (sel.last + 1 - prefix.length).coerceAtMost(ci.text.length)
       if (start < end) sb.append(ci.text, start, end)
       return sb.toString()
   }
   ```
   `coerceAtLeast(0)` on `start` is required here because `sel.first - prefix.length` is negative when the selection lies entirely inside the prefix.

### 3.3 Single source of truth

Pre-PR, the prefix expression `if (mc.text.isEmpty()) mc.reason.text else "${mc.reason.text}: "` lived inline at two render sites:

- `FramedItemView.kt:368` — the actual report row
- `ChatPreviewView.kt:262` — the chat list preview

Re-introducing it inline in `TextSelection.kt` would have re-created exactly the silent coupling that produced the bug — a future change to the separator (e.g. localised colon) at the renderer would silently break copy/snap. The fix factors the expression into:

```kotlin
// TextItemView.kt
fun itemPrefixText(ci: ChatItem): String = when (val mc = ci.content.msgContent) {
  is MsgContent.MCReport -> if (mc.text.isEmpty()) mc.reason.text else "${mc.reason.text}: "
  else -> ""
}
```

Both renderers and both selection-side functions now derive the string from this one definition.

### 3.4 Edge cases verified

| Case | Pre-PR | Post-PR |
|---|---|---|
| Non-report, fmt non-null (markdown) | works | byte-identical loop, works |
| Non-report, fmt null (plain text) | substring fast path | StringBuilder path, value-equivalent |
| Non-report, sel out of bounds | clamped to `[L, L]` → `""` | same |
| Report, full sel `Spam: test` | returns `test` (BUG) | returns `Spam: test` |
| Report, prefix-only sel | returns `""` (BUG) | returns prefix slice |
| Report, body-only sel | returns body (offset shift was hidden by `Int.MAX_VALUE` clamp at `sel.last+1`) | returns body |
| Report, sel.first == prefix.length | works coincidentally | works |
| Report, empty body, prefix-only sel | returns `""` | returns prefix |
| Report with mention having `localAlias` (transformed) | snap snapped to wrong char (BUG) | snaps correctly |
| Multi-item interior sel (`sel.last = MAX-1`) | works | no overflow on `+1 - prefix.length` |

### 3.5 What was deliberately not done

- **Performance restoration of the non-report null-fmt path.** Pre-PR returned `ci.text.substring(...)` directly (1 allocation). Post-PR uses `StringBuilder` (3 allocations). `selectedItemCopiedText` runs once per selected item per copy action — never on a hot path. Restoring the pre-PR fast path with an `if (prefix.isEmpty() && formattedText == null)` early return adds 4 lines of branching for negligible gain. Not worth it.

- **Migrating `ChatPreviewView.kt` was kept** because it crossed the 3-site extraction threshold (FramedItemView + ChatPreviewView + 2× TextSelection) and the bug we are fixing is exactly the failure mode of duplicating this expression. ChatPreviewView is not a selection site, so no behaviour change — it shifts to the same single source of truth.

## 4. Detailed implementation plan

### 4.1 Files touched (final state)

| File | Δ | Purpose |
|---|---|---|
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TextItemView.kt` | +7 / 0 | new `itemPrefixText(ci)` helper next to `itemDisplayText` |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt` | +1 / −1 | report branch delegates to `itemPrefixText(ci)` |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatPreviewView.kt` | +2 / −2 | preview row delegates; drops unused `val mc =` |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/TextSelection.kt` | +16 / −8 | the actual fix in `selectedItemCopiedText` and `snapOffset`, plus import |

Total: 4 files, +26 / −11.

### 4.2 Step-by-step (final commit `96d6f3222`)

1. **Add `itemPrefixText(ci)`** in `TextItemView.kt` next to `itemDisplayText` / `itemSegmentDisplayText`. Returns `""` for non-reports.

2. **`FramedItemView.kt:365-372`** (`MCReport` branch): replace inline expression with `append(itemPrefixText(ci))`. The surrounding `withStyle(SpanStyle(color = Red, italic))` is preserved — visual rendering unchanged.

3. **`ChatPreviewView.kt:258-264`**: replace inline expression with `append(itemPrefixText(ci))`. Drop the now-unused `val mc =` from `when (val mc = ci.content.msgContent)` (the discriminator becomes `when (ci.content.msgContent)`).

4. **`TextSelection.kt`**:
   - Add `import chat.simplex.common.views.chat.item.itemPrefixText`.
   - In `selectedItemCopiedText`:
     - Compute `val prefix = itemPrefixText(ci)` and `val sb = StringBuilder()` first.
     - Emit prefix slice if `sel.first < prefix.length`.
     - Modify the `formattedText ?: ...` early-return to a `?: run { … }` block that adds the body slice (offsets shifted by `-prefix.length`, clamped) to `sb` and returns `sb.toString()`.
     - Seed the formattedText loop with `var displayOffset = prefix.length`. Loop body unchanged.
   - In `snapOffset`: change `var displayOffset = 0` to `var displayOffset = itemPrefixText(ci).length`. Loop body unchanged.
   - Update the docstring on `selectedItemCopiedText` to note that display-text space includes any leading `itemPrefixText`.

### 4.3 Verification

- `./gradlew :common:compileKotlinDesktop` — passes (warnings are pre-existing).
- `bash /home/user/build/linux.sh` — full Linux x86_64 AppImage produced (`SimpleX_Chat-x86_64-fix-select-in-reports.AppImage`).
- Manual test plan, all in desktop:
  1. Open a chat with a report whose rendered form is `Spam: test`. Select across the whole line + Ctrl-C → clipboard reads `Spam: test`.
  2. Select only the red prefix → clipboard reads the prefix.
  3. Select only the comment → clipboard reads the comment.
  4. Report comment containing `@alice (Bob)` (mention with localAlias). Drag a selection boundary into the mention → on release, highlight snaps to mention boundaries.
  5. Plain (non-report) messages: full-line, partial, mention, link selections — clipboard contents unchanged from pre-PR.
  6. Multi-item selection across non-report and report rows — prefixes appear inline at the correct positions.

### 4.4 Risk and rollback

- **Blast radius** is the desktop selection-copy code path. iOS / Android use separate selection mechanisms and are unaffected.
- The non-report selection path's inner loop body is byte-for-byte identical to pre-PR (the `displayOffset = 0` initialisation is unchanged when `prefix.length == 0`), so regressions on non-reports would require the prefix expression itself to fail — which is impossible because `itemPrefixText` returns `""` for any `msgContent` other than `MCReport`.
- Rollback is `git revert 96d6f3222 e97dd7bf4 6aacfa4d2` (three commits) and a force-push, restoring the pre-PR copy behaviour with the original bug.

## 5. Why this specific shape

- Recognising the prefix as the *first* display-space segment turns the bug into a one-line seed change. No special-cased report branch in copy/snap; the existing loop handles both.
- The inner loop of `selectedItemCopiedText` and `snapOffset` is byte-for-byte identical to pre-PR. Only the seed value of `displayOffset` and the pre-/post-amble change.
- Four sites need the prefix string (FramedItemView, ChatPreviewView, and two in TextSelection). `itemPrefixText` becomes their single point of change, closing the silent-coupling gap that produced the bug.
- `selectedRange()` guarantees `sel.first ≥ 0`, so no `coerceAtLeast(0)` is added at the prefix-slice append. The one `coerceAtLeast(0)` that survives (on the `formattedText == null` body branch) is reachable when the selection lies entirely inside the prefix and is needed.
- Final PR is 4 files, +26 / −11. The inner loop body changes by zero lines.
