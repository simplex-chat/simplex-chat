# PNG previews from a predicted size

A PNG preview is fitted from the largest size down (`fitSize maxSize Nothing`, `Image.hs:228`). Its first try encodes the image at the display size twice, without and with PNG filtering; the kept preview is 88 to 539 px wide.

References are to `src/Simplex/Chat/Image.hs` at `ea858cccf`.

## Decisions

| ID | Decision |
|---|---|
| D1 | **PNG previews start at a predicted width.** As for JPEG, the first width is predicted from the data size of the limit, at 8 bits per pixel. |
| D2 | **Other paths unchanged.** JPEG previews and images, and PNG images, keep their search and their outputs. |

## Mechanics

- `fitImage` (`:208-228`), the tuple per output:
  - `jpegBytes` renamed `dataBytes`: data bytes of the output size, now used by both formats
  - `bitsPerPixel` renamed `jpegBitsPerPixel`
  - added `pngBitsPerPixel`: `Nothing` for `ImageBytes`, `Just 8` for `ImageDataUri`
- `fitSize` first pixels:
  - JPEG: `8 * dataBytes target / jpegBitsPerPixel`, as before
  - PNG: `8 * dataBytes target / bpp` for `Just bpp`, the largest size for `Nothing`

## Measured result

16 research inputs, preview and image, minimum of 3 runs at -O2. SSIMULACRA2 against the source at the preview display size, the research method.

| PNG preview | time before, ms | time after, ms | SSIMULACRA2 before | after |
|---|---|---|---|---|
| cutout | 3744 | 442 | -37.11 | -37.11 |
| diagram | 698 | 228 | -56.15 | -53.30 |
| emoji | 456 | 89 | 0.95 | 0.95 |
| fixture | 727 | 445 | 66.25 | 66.25 |
| large | 10918 | 4225 | -37.18 | -37.18 |
| logo | 1816 | 689 | 55.51 | 58.06 |
| saturn | 4707 | 150 | 2.33 | 1.03 |
| window | 784 | 284 | 30.96 | 30.57 |

- PNG previews: 23849 ms → 6551 ms in total (-73%).
- Scores: 4 of 8 unchanged; the changes -1.30 to +2.85.
- Score noise of the current path, encoded at 99% of the kept width: median 0.7, up to 3.2.
- JPEG outputs and PNG images: byte-identical.
