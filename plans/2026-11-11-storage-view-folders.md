# Android, desktop: folder sizes inside the Storage screen

## Problem

The Storage screen in Developer options shows the size of each top-level entry of the app's storage folders. On Android, `files` contains received files (`app_files`), the core's temporary files (`temp_files`) and wallpapers (`assets`). "Delete all files" deletes only the files directly inside `app_files`. When `files` stays large after it, the screen cannot show which folder or file inside `files` holds the space.

## Cause

`StorageView` lists only the entries of `storageRoots()`, and its rows are not clickable.

## Fix

- `StorageView` takes the directories to list, `storageRoots()` by default.
- An entry that is a directory and not a symlink keeps its `Path` from `Files.newDirectoryStream`; its row opens `StorageView` for that directory.
- Each screen keeps its listing in its `ModalData`, so Back shows the parent's listing without walking it again.
- `rootUsage`, `RootUsage` and `EntryUsage` are `internal`, for `StorageViewTest`.

## Scope

The iOS Storage screen (`apps/ios/Shared/Views/UserSettings/StorageView.swift`) is unchanged.
