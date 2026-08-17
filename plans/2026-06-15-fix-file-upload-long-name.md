# Fix: long file name hides the close icon in the compose file preview

Date: 2026-06-15
Branch: `nd/fix-file-upload-with-long-name`
Platforms affected: Android, Desktop, iOS

## Problem

When a file is attached for sending, the compose area shows a preview row with the
file icon, the file name, and a close (X) icon to cancel/remove the file before
sending. If the file name is long, the close icon is not shown, so the user cannot
dismiss the attachment.

## Cause

The bug is the same layout defect on both codebases: the file-name text is
unconstrained, so a long name consumes all horizontal space and squeezes the
trailing close button to zero width.

### Android / Desktop — `ComposeFileView.kt`

The row was laid out as:

```
Icon(fixed) | Text(fileName)            | Spacer(weight 1f) | IconButton(close)
              ^ unweighted, no maxLines
```

In a Compose `Row`, unweighted children are measured first and take the remaining
width before weighted children get anything. The unweighted `Text` therefore grabbed
the whole remaining width on a long name, leaving the weighted `Spacer` — and the
`IconButton` after it — with ~0 width. The flexible element was the `Spacer`, but a
`Spacer` can only distribute the space the rigid `Text` did not already eat.

### iOS — `ComposeFileView.swift`

```
Image(fixed) | Text(fileName) | Spacer() | Button(close)
               ^ no lineLimit
```

A `Text` with no `lineLimit` reports its full single-line ideal width and refuses to
truncate, so a long name collapses the `Spacer` and pushes the `Button` past the
`.frame(maxWidth: .infinity)` edge, off-screen.

## Fix

Make the file name the element that yields space and let it truncate, so the
fixed-size close control's space is always reserved.

- **Kotlin:** give the `Text` the `weight(1f)` (instead of the `Spacer`) and
  `maxLines = 1`, and drop the now-redundant `Spacer`. This matches the existing
  idiom — `ComposeImageView` puts `weight(1f)` on its content, and `CIFileView`
  caps file-name text with `maxLines = 1`.
- **Swift:** add `.lineLimit(1)` to the `Text`, so it truncates instead of
  overflowing, matching how file names are shown elsewhere on iOS.

## Why this is the right fix (not a workaround)

`ComposeFileView` was the only compose preview that gave the weight to a `Spacer`
rather than to its content; every sibling preview (`ComposeImageView`,
`ContextItemView`) reserves space for the trailing close control by weighting the
content. The change brings the file preview in line with the established pattern
rather than adding a special case. It is purely structural — no behavior changes
beyond layout.

## Scope / risk

- One-spot edit per file; no API or behavior change.
- Android and Desktop share the Kotlin file, so both are fixed together; iOS is the
  separate Swift file.
- No string/translation keys touched.

## Verification

- Visual: attach a file with a very long name on Android, Desktop, and iOS; confirm
  the name truncates and the close (X) icon stays visible and tappable.
