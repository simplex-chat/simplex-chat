# iOS: Native Markdown Formatting Edit Menu in Compose Field

## Problem

When composing a message on iOS, formatting text with Markdown (bold, italic, strikethrough, monospace, secret/spoiler) requires typing Markdown syntax markers manually (`**`, `_`, `~`, `` ` ``, `#`). Competitor messaging apps like Signal and iMessage allow users to select text and format it directly via the native edit menu (popup menu with Copy, Paste, etc.).

## Solution

Extend the native edit menu (`UIEditMenuInteraction` via `UITextViewDelegate.textView(_:editMenuForTextIn:suggestedActions:)`) in `CustomUITextField` (`NativeTextEditor.swift`) on iOS 16+:
1. When text is selected (`range.length > 0`), insert a "Format" (`Formatuj`) menu item with a submenu containing:
   - **Bold** (`Pogrubienie`) -> wraps core text in `**`
   - **Italic** (`Kursywa`) -> wraps core text in `_` (SimpleX Markdown uses `_` for italic and `*` for single-star bold)
   - **Strikethrough** (`Przekreślenie`) -> wraps core text in `~`
   - **Monospace** (`Kod`) -> wraps core text in `` ` ``
   - **Spoiler** (`Ukryty tekst`) -> wraps core text in `#`
2. Smart whitespace handling: iOS selection often captures trailing or leading whitespace (e.g. double-tapping a word captures trailing spaces like `"word "`). Because SimpleX's Markdown parser explicitly rejects formatting tags that contain leading or trailing spaces (`isSpace (T.head s) || isSpace (T.last s)`), the formatter trims leading/trailing whitespace and places Markdown delimiters around the core text only (e.g. `**word** `).
3. Cursor positioning: After formatting is applied, the cursor is positioned directly after the formatted text (`selectedRange` set to length 0 after the replacement), and synced to the SwiftUI state (`parent?.selectedRange`).
4. Undo support & state synchronization: Text replacement uses `UITextInput.replace(_:withText:)` to integrate with UIKit's native undo manager, followed by `textViewDidChange` to update SwiftUI's message text binding, recalculate view height, and trigger link preview / draft updates.
5. Localization: Menu titles are localized using `NSLocalizedString` with base keys added to `en.lproj/Localizable.strings`.

## Affected docs

- `spec/client/compose.md` — document edit menu formatting actions in `NativeTextEditor` / `CustomUITextField`.
- `product/views/chat.md` — Compose Bar table: text formatting edit menu.
- `spec/impact.md` — affected components.
