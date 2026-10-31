# Desktop: no confirmation after a QR code image is saved

## Problem

On desktop, after a QR code is clicked and its image is saved to the chosen folder, the app shows no confirmation.

## Cause

Desktop `saveTempImageUncompressed` (`Utils.desktop.kt`) writes the image and shows nothing. Master's only "File saved" toast after a QR save came from the second Save dialog (`shareFile`), which `plans/2026-10-18-desktop-save-dialog-mixup.md` removes on desktop.

## Fix

Desktop `saveTempImageUncompressed` shows the `file_saved` toast after the image is written.

## Scope

Desktop QR code image save. Android: unchanged.
