# Android, desktop: folder sizes inside the Storage screen

## Problem

The Storage screen in Developer options shows each top-level entry of the app's storage folders with its total size. On Android, the `files` entry contains received files (`app_files`), the core's temporary files (`temp_files`) and wallpapers (`assets`). When `files` stays large after "Delete all files" (which empties `app_files` only), the screen cannot show which of them, or which file inside them, takes the space.

## Cause

`StorageView` lists the entries of fixed root folders only (`storageRoots()`), and its rows are not clickable.

## Fix

- `StorageView` takes the folders to list as a parameter, `storageRoots()` by default.
- Each entry that is a directory (not a symlink) records its `Path`; its row opens a new Storage screen for that directory. Files and symlinks are not clickable.
- The opened screen shows the directory's own entries with their recursive sizes and its total, so any depth can be reached one level at a time.
- Directories are passed as `Path`, not `File`, so names that are not valid in the JVM file-name encoding open correctly.

## Scope

Android and desktop (`commonMain`). The iOS Storage screen is not changed.
