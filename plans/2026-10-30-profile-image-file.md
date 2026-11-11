# Profile images from files

`/set profile image file <path>` and `--user-image-file <path>` set the file's bytes as the profile image. A photo exceeds the profile image limit of 12500 characters and is refused; a file of another format with a `.png`, `.jpg` or `.jpeg` name is sent to contacts unchanged.

The file is resized by the core, as the apps resize a picked profile image: cropped to the centre square, then resized to a data URI of at most 12500 characters. A file that cannot be resized gives an error, and the profile is unchanged.

References are to master `d7d180a42`. Haskell paths without a directory are in `src/Simplex/Chat/`.

## Decisions

| ID | Decision |
|---|---|
| D1 | **One function for both paths.** `loadImageData` (`Library/Commands.hs:207-223`) resizes the file; `readProfileImageFile` (`:225-226`, `/set profile image file`) and `loadImageFile` (`Core.hs:227-228`, `--user-image-file`) call it unchanged. |
| D2 | **Centre-square crop in the core.** The apps crop the picked image to its centre square before `resizeImageToStrSize(…, maxDataSize: 12500)`: iOS `cropToSquare` (`apps/ios/SimpleXChat/ImageUtils.swift:88-99`), Android and desktop `cropToSquare` (`Images.android.kt:54-64`, `Images.desktop.kt:65-84`). The core crops the decoded image the same way: side `min w h`, offsets `(w - side) div 2` and `(h - side) div 2`. |
| D3 | **Resize as for previews.** `decodeResizable`, the crop, then `resizeImageDataUri` with `maxProfileImageSize` (12500): JPEG, or PNG for an image with transparency, a Display P3 profile as in `Image.hs`. |
| D4 | **Errors.** The extension, missing-file and empty-file errors are unchanged. A file that cannot be decoded or resized, or whose resizing throws an exception, gives `image file cannot be resized: <path> (<error>)`. |

## Mechanics

- `Image.hs`: `cropToSquare :: DynamicImage -> DynamicImage`, exported, with `dynamicPixelMap` and `generateImage`, as `orientImage`.
- `Library/Commands.hs`, `loadImageData`:
  - extension check: `.png`, `.jpg` or `.jpeg`, any case
  - `doesFileExist`, then `B.readFile`, then the empty check, as before
  - the bytes: `decodeResizable`, `cropToSquare`, `resizeImageDataUri (fromIntegral maxProfileImageSize)`, under `catchAll`, as `imagePreview`
- Test `testSetProfileImageFromFile` (`tests/ChatTests/Profiles.hs:1212-1237`):
  - `tests/fixtures/test.jpg` (1280×720): "profile image updated"; the active user's image is a `data:image/jpg;base64,` URI of at most 12500 characters
  - a `.png` file with text bytes: "bad chat command: image file cannot be resized: …"
  - the unsupported extension, missing file and empty file cases unchanged
