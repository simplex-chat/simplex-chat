# Fix attachment options hidden under the keyboard

## Problem

On Android, when the keyboard is already open and the user taps the attachment
(paperclip) button in a chat, the sheet with the Camera / Image / Video / File
options opens partly or fully under the keyboard. With the keyboard closed it opens
correctly.

## Cause

The sheet is a Material `ModalBottomSheetLayout` (`ChatView.kt`, `ChatLayout`) whose
state is created in `ChatView` as
`rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)`, leaving
`skipHalfExpanded` at its default `false`.

The sheet content, `ChooseAttachmentView`, pads itself with
`.navigationBarsPadding().imePadding()`, so while the keyboard is open the sheet's
height is the buttons row plus the keyboard height, and the buttons are at the top of
the sheet.

In Material 1.8.2 (`androidx.compose.material:material`, `ModalBottomSheet.kt`):

- `modalBottomSheetAnchors` adds a `HalfExpanded` anchor at `fullHeight / 2` whenever
  `!isSkipHalfExpanded && sheetSize.height > fullHeight / 2`;
- `show()` animates a hidden sheet to `HalfExpanded` when that anchor exists, and to
  `Expanded` otherwise.

With the keyboard closed the sheet is short, there is no `HalfExpanded` anchor, and
it opens `Expanded`. With the keyboard open the sheet is taller than half the
layout, so it stops at `HalfExpanded`: its top is at the middle of the screen, and
the buttons row below it lands under the keyboard's top edge.

## Fix

Pass `skipHalfExpanded = true` to the attachment sheet's
`rememberModalBottomSheetState`. `show()` then always goes to `Expanded`, so the
sheet's bottom sits at the bottom of the layout and the `imePadding` places the
buttons directly above the keyboard.

The half-expanded state has no use for this sheet: it holds a single row of buttons
and no scrollable content.

## Scope / non-goals

- Only the chat attachment sheet is changed. The other `ModalBottomSheetLayout`s
  (`GroupProfileView`, `UserProfileView`, `AddGroupView`, `AddChannelView`,
  `WelcomeView`) use the same default state but are not part of this report.
- The two preview usages of `rememberModalBottomSheetState` in `ChatView.kt` are
  left as they are.
- Desktop: the attachment sheet is shared code, but desktop has no software
  keyboard inset, so the sheet is never taller than half the window and behaviour
  is unchanged.
- iOS was not checked; it does not use this code.

## Verification

Debug arm64 APK built from this branch; on a device, open the keyboard in a chat
and tap the attachment button: the options are shown directly above the keyboard.
With the keyboard closed the sheet opens as before.
