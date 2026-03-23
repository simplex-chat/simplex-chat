# Small Text Markdown

Add `!- text!` syntax for small gray text — legal disclaimers, secondary commentary, LLM reasoning, etc.

## Syntax

`!- text!` — renders as small gray text. Uses the `!` style prefix family, `-` for "reduced."

On old clients: `!- fine print!` shows as-is (old `coloredP` fails on `-`, falls to `wordP`). Readable.

## Changes

### Haskell — `src/Simplex/Chat/Markdown.hs`

1. **`Format`**: add `Small` constructor (no fields).

2. **`coloredP` parser**: before trying `colorP`, check for `-` followed by space. If matched, produce `Small`. Otherwise fall through to existing color parsing.

3. **`markdownText`**: add `Small` case, reconstruct as `!- text!`.

4. **JSON serialization**: TH-derived `ToJSON`/`FromJSON` via existing `sumTypeJSON fstToLower`. Produces `{"small": {}}`. Old Haskell `FromJSON Format` falls to `Unknown` via `<|> pure (Unknown v)`.

### Haskell — `src/Simplex/Chat/Styled.hs`

5. **`sgr`**: add `Small` case — map to `FaintIntensity` for terminal rendering.

### Haskell — `tests/MarkdownTests.hs`

6. Tests for:
   - `!- text!` parses as `Small`
   - `!- text!` with leading/trailing spaces in content → no format (same rule as other formats)
   - Existing color syntax unchanged
   - `markdownText` round-trip

### iOS — `apps/ios/SimpleXChat/ChatTypes.swift`

7. **`Format`** enum: add `case small`.

### iOS — `apps/ios/Shared/Views/Chat/ChatItem/MsgContentView.swift`

8. **`messageText`**: render `Small` with smaller `UIFont` point size + gray color.

### Android — `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`

9. **`Format`**: add `@Serializable @SerialName("small") class Small: Format()`.
10. **`Format.style`**: `SpanStyle` with smaller font size + gray color.

### Android — `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TextItemView.kt`

11. **`MarkdownText`**: add `is Format.Small` case — same pattern as `Bold`/`Italic` (apply style, append text).

## Backward Compatibility

### Local (old app receiving message with new syntax)
- Old app's bundled Haskell parses raw message text. Old `coloredP` doesn't know `-`, fails, falls to `wordP`. Text shows as `!- fine print!` — plain text with delimiters.

### Remote desktop (old desktop, new mobile)
- New mobile Haskell parses `!- text!` as `Small`, serializes to JSON `{"small": {}}`.
- Old desktop Haskell re-parses JSON via `J.parseJSON` (`Remote/Protocol.hs:184`). Old `FromJSON Format` doesn't know `"small"` → `<|> pure (Unknown v)`.
- `Unknown` re-serializes to `{"type": "unknown", "json": ...}` → Kotlin `Format.Unknown` (`ignoreUnknownKeys` drops extra fields). Text renders without formatting.

## Order of Implementation

1. Haskell types + parser + tests
2. iOS types + rendering
3. Android types + rendering
