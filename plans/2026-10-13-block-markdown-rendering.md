# Block Markdown: rendering in the apps and on the web

Date: 2026-10-13. Revised: 2026-10-16, for master. Revised: 2026-10-17, drawing from markdownBlocks. Revised: 2026-10-18, drawing by index ranges (S5).

The apps draw `formattedText` with `markdownBlocks` (`2026-10-13-block-markdown.md`, sections 2 and 4) as one text per item. A message without blocks is drawn as now. In a message with blocks, the segment loop of master is called for the index ranges of the blocks, in the font of each block, and the block drawing is put between the calls (section 3). On the web, HTML blocks are drawn by the page script.

References are to master at 382dc7480.

```
K/   apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/
I/   apps/ios/Shared/Views/
S/   apps/ios/SimpleXChat/
W/   website/src/js/
```

## 1. Scope

```
blocks drawn     every list drawn by MarkdownText (K/views/chat/item/TextItemView.kt:125)
                 every list drawn by messageText (I/Chat/ChatItem/MsgContentView.swift:396)
                 desktop selection (K/views/chat/TextSelection.kt:264-305)
                   copied text of a selection: as drawn, by the same block drawing
                   the Copy action of an item: its source text, as now
tokens by kind   notification texts (K/platform/NtfManager.kt:134-148, S/Notifications.swift:200-210)
token form       compose state: parsedMessage, links, mentions, link sanitizing, as now
                   K/views/chat/ComposeView.kt:111, 130, 1024-1070
                   I/Chat/ComposeMessage/ComposeView.swift:560, 1240, 1990
HTML blocks      renderMarkdown (W/simplex-lib.jsc:57-128)
```

- One text per item, as now:
  - taps
  - long presses
  - secrets
  - mentions
  - commands
  - the typing indicator
  - the meta reserve
  - text direction
- Lines are wrapped.
- Code blocks and tables are scrolled with the chat.
- Horizontal scrolling: in the terminal layout (ep/llm-ux); its table views are drawn from the same ranges, one segment loop call per cell.

## 2. Types

Kotlin, `K/model/ChatModel.kt`:

```
Format (:5177)       @Serializable @SerialName("bold") class Bold(val singleStar: Boolean = false): Format()   (:5178)
                     @Serializable @SerialName("escape") class Escape: Format()
                     @Serializable @SerialName("markup") class Markup(val token: BlockToken): Format()
                     style of Escape and Markup: SpanStyle()
BlockToken           @Serializable sealed class, next to Format
                       @SerialName("lineBreak") class LineBreak
                       @SerialName("softBreak") class SoftBreak
                       @SerialName("hardBreak") class HardBreak
                       @SerialName("paragraphBreak") class ParagraphBreak(val lineBreaks: Int)
                       @SerialName("indent") class Indent
                       @SerialName("headingMark") class HeadingMark(val level: Int)
                       @SerialName("headingClose") class HeadingClose
                       @SerialName("headingUnderline") class HeadingUnderline(val level: Int)
                       @SerialName("listMark") class ListMark(val number: String? = null)
                       @SerialName("quoteMark") class QuoteMark
                       @SerialName("fenceMark") class FenceMark(val language: String? = null)
                       @SerialName("rowMark") class RowMark
                       @SerialName("cellMark") class CellMark
                       @SerialName("delimiterRow") class DelimiterRow(val columns: List<TableAlign>)
                       @SerialName("ruleMark") class RuleMark
MarkdownBlock        @Serializable sealed class
                       @SerialName("paragraph") class Paragraph(val content: SegmentRange)
                       @SerialName("heading") class Heading(val level: Int, val content: SegmentRange)
                       @SerialName("codeBlock") class CodeBlock(val language: String? = null, val content: SegmentRange)
                       @SerialName("quote") class Quote(val content: SegmentRange)
                       @SerialName("list") class ItemList(val items: List<ListEntry>)
                       @SerialName("table") class Table(val columns: List<TableAlign>, val header: List<SegmentRange>, val rows: List<List<SegmentRange>>)
                       @SerialName("rule") class Rule(val mark: SegmentRange)
ListEntry            @Serializable class ListEntry(val number: String? = null, val content: SegmentRange, val items: List<ListEntry>)
SegmentRange         @Serializable class SegmentRange(val from: Int, val to: Int)
TableAlign           @Serializable enum class: none, left, center, right
BlockFont            enum class: regular, bold, monospace
BlockPart            sealed class
                       Segments(val range: IntRange, val font: BlockFont)
                       Text(val text: String, val font: BlockFont)
BlockToken.drawnText the text drawn for the token (section 3)
blockParts           (fts: List<FormattedText>, blocks: List<MarkdownBlock>, displayText: (FormattedText) -> String) -> List<BlockPart>
                     section 3
displayWidth         (s: String) -> Int, section 3
segmentDisplayText   (ft: FormattedText, mentions: Map<String, CIMention>?, linkMode: SimplexLinkMode) -> String
                     the body of itemSegmentDisplayText (K/views/chat/item/TextItemView.kt:88-108), with mentions instead of ci
                     Escape and Markup: the text drawn for the token (section 3)
                     replaces itemSegmentDisplayText
itemDisplayParts     (ci: ChatItem, linkMode: SimplexLinkMode) -> List<Pair<String, String>>?
                     per drawn part: the displayed text and the copied text
                       segment         segmentDisplayText, its text (Markup: its drawn text)
                       BlockPart.Text  the text, the text
                     null without formattedText
```

- `blockParts` returns parts instead of calling back: the segment loop reads `@Composable` getters (`Format.style`, `MaterialTheme.colors`), so it stays in the composable scope.

- Kotlin name `ItemList`: `List` is `kotlin.collections.List` in the scope of `MarkdownBlock`; JSON tag "list".

Kotlin records, field `markdownBlocks: List<MarkdownBlock>? = null` after `formattedText`:

```
ChatItem                 K/model/ChatModel.kt:3409
CIQuote                  K/model/ChatModel.kt:4415
ChatItemVersion          K/model/ChatModel.kt:5716
ParsedFormattedText      K/views/helpers/TextEditor.kt:134-136
```

iOS, `S/ChatTypes.swift`:

```
Format (:5681)       case bold(singleStar: Bool)
                     case escape
                     case markup(token: BlockToken)
BlockToken           enum: Decodable, Equatable, Hashable
                     cases as the Kotlin classes, associated values as their fields
MarkdownBlock        enum: Decodable, Equatable, Hashable
                     cases as the Kotlin classes, list as `list`
ListEntry            struct: Decodable, Equatable, Hashable
SegmentRange         struct: Decodable, Equatable, Hashable
TableAlign           enum: String, Decodable, Hashable
BlockFont            enum: regular, bold, monospace
BlockPart            enum: segments(Range<Int>, BlockFont), text(String, BlockFont)
blockParts           public func (_ fts: [FormattedText], _ blocks: [MarkdownBlock], displayText: (FormattedText) -> String) -> [BlockPart]
                     section 3
displayWidth         public func (_ s: String) -> Int, section 3
segmentDisplayText   (_ ft: FormattedText, _ mentions: [String: CIMention]?) -> String, in I/Chat/ChatItem/MsgContentView.swift
                     the displayed text of a segment, as drawn by messageText (I/Chat/ChatItem/MsgContentView.swift:483-541): uri, hyperLink, simplexLink, command, mention
                     escape and markup: the text drawn for the token (section 3)
```

- The `case .bold:` pattern (I/Chat/ChatItem/MsgContentView.swift:450) binds `singleStar`.

iOS records, field `markdownBlocks: [MarkdownBlock]?` after `formattedText`:

```
ChatItem                 S/ChatTypes.swift:3810
CIQuote                  S/ChatTypes.swift:4861
ChatItemVersion          S/ChatTypes.swift:6400
ParsedMarkdown           S/API.swift:179-181
```

- `FormattedText` is unchanged.
- With an unknown block or token tag, the item is invalid, as with an unknown `Format` tag.

## 3. Drawing

Segment loop:

```
segment loop      master's loop over formattedText
                    Kotlin: MarkdownText (K/views/chat/item/TextItemView.kt:229-342)
                    Swift: messageText (I/Chat/ChatItem/MsgContentView.swift:446-570)
range             the range of a Segments part, all indices without blocks
                    Swift: the loop in a nested function appendSegments(_ fts: ArraySlice<FormattedText>, _ blockFont: BlockFont)
font              Kotlin: withStyle(bold or monospace) around the loop
                  Swift: the base font descriptor and font of the block font; regular: those of the text style (:413-414)
                    the fonts of formats and links derived from them
tokens            SoftBreak            " "
                  LineBreak            "\n"
                  HardBreak            "\n"
                  ParagraphBreak n     n × "\n"
                  other Markup         nothing
                  Escape               nothing
                  without markdownBlocks: Markup drawn as its text (a teaser parsed from part of a text)
Bold singleStar   italic, when markdownBlocks is present
HyperLink         linkUri "mailto:" (e-mail autolink): opened as an e-mail link
                    Kotlin: annotation OTHER_URL; Swift: without webLinkAttrKey
                  FormattedText.linkUri: null for it, so no link preview
```

MarkdownText (Kotlin), messageText (Swift):

```
markdownBlocks null   the segment loop over all segments, as now
otherwise             blockParts
                        Segments   the segment loop over the range, in the font
                        Text       the text without format, in the font
```

`blockParts`, with `range(r, font)` for a Segments part and `text(t, font)` for a Text part:

```
blocks       in source order
gaps         the tokens before the first block, between two blocks, between two list entries, after the last block
               LineBreak, ParagraphBreak: text(their drawn text, regular)
               other tokens: nothing
             the tokens of a block outside its ranges: nothing
Paragraph    range(content, regular)
Heading      range(content, bold)
CodeBlock    range(content, monospace)
Quote        the lines of content, divided at HardBreak tokens
             each line: text("│ "), range(line)
             between lines: text("\n")
List         the entries in source order, a parent before its children
             each entry: text(4 spaces per level below the top, "•" or the number, " ")
               the lines of its content, divided at HardBreak tokens: range(line)
               between lines: text("\n", then as many spaces as before its first text)
Table        each row: each cell: range(cell, monospace), text(padding, monospace)
                       between cells: text(" │ ", monospace)
             between rows: text("\n")
             under the header row: text(a run of "─" of the column width per column, "─┼─" between runs, monospace)
             a missing cell at a row end: not drawn
             a cell wider than its column: no padding
             padding by the column alignment
               none         after the text
               left         after the text
               right        before the text
               center       half before, rounded down
                            the rest after
Rule         text("———"), at mark
width        column: the largest width of a cell of the column, at most 40
             cell: displayWidth of displayText of its segments
ranges       limited to the length of fts
```

`displayWidth`, per code point:

```
U+200D                                     0; the next code point 0
U+FE0F                                     1 after a code point of width 1, 0 otherwise
U+FE00-U+FE0E, U+1F3FB-U+1F3FF, U+E0020-U+E007F, marks Mn and Me   0
East Asian Wide and Fullwidth, Unicode 15.0   2
other                                      1
```

- The wide set: a table of 84 ranges, generated from Python's `unicodedata` (Unicode 15.0), assigned code points only, gaps of unassigned code points merged; it includes the emoji of default emoji presentation.

- Per code point, not per grapheme: `java.text.BreakIterator` of JDK 17 (desktop) divides emoji modifier and ZWJ sequences, Android's divides by Unicode rules; the code point rule gives equal widths on both.

- A row wider than the bubble is wrapped; the wrapped part starts at the left edge (B12, decided 2026-10-18).
- Ranges are limited to the length of `fts`: for chat link items, `stripFormattedTextLink` (K/views/chat/item/TextItemView.kt:622-632, I/Chat/ChatItem/MsgContentView.swift:608-616) removes the last segment.

Desktop selection, `selectedItemCopiedText` and `snapOffset` (K/views/chat/TextSelection.kt:264-305):

```
parts      itemDisplayParts
display    the length of each displayed text added to the display offset
copy       the selected part of the copied text when its length equals the displayed length, otherwise all of it
```

Notifications, `hideSecrets` (K/platform/NtfManager.kt:134-148, S/Notifications.swift:200-210):

```
segment text   Secret "...", Markup its drawn text, Escape nothing, other segments their text
blocks null    the segment texts joined
otherwise      blockParts with the segment text as displayText: Segments by their segment texts, Text as is
```

## 4. Call sites

Kotlin:

```
MarkdownText (K/views/chat/item/TextItemView.kt:125)        parameter markdownBlocks: List<MarkdownBlock>? = null; section 3
CIMarkdownText (K/views/chat/item/FramedItemView.kt:445)    ci.markdownBlocks
ciQuotedMsgTextView (K/views/chat/item/FramedItemView.kt:61) qi.markdownBlocks
item preview (K/views/chatlist/ChatPreviewView.kt:261-265, 277)   null where formattedText is null, otherwise ci.markdownBlocks
TextBubble (K/views/chat/ChatItemInfoView.kt:55-57)         the markdownBlocks of the item or the version
ContextItemView (K/views/chat/ContextItemView.kt:59)        contextItem.markdownBlocks
ProfileDescriptionText (K/views/chat/ChatInfoView.kt:794-795, 804-805)   blocks of parseToMarkdownText
showFullProfileDescription (K/views/chat/item/TextItemView.kt:56-58)     blocks of parseToMarkdownText
WelcomeMessageView (K/views/chat/group/WelcomeMessageView.kt:149-151)    blocks of parseToMarkdownText
desktop selection (K/views/chat/TextSelection.kt:264-305)   section 3; itemDisplayParts
notifications (K/platform/NtfManager.kt:134-148)            section 3
parseToMarkdownText (K/views/helpers/TextEditor.kt)         new: (text: String) -> ParsedFormattedText?
parseToMarkdown (K/views/helpers/TextEditor.kt:138-146)     the formattedText of parseToMarkdownText
```

iOS:

```
messageText (I/Chat/ChatItem/MsgContentView.swift:396)      parameter markdownBlocks: [MarkdownBlock]?; section 3
MsgContentView (I/Chat/ChatItem/MsgContentView.swift:34, 100)   var markdownBlocks, passed to messageText
item content (I/Chat/ChatItem/MsgContentView.swift:624)     chatItem.markdownBlocks
quote (I/Chat/ChatItem/FramedItemView.swift:337)            qi.markdownBlocks
FramedItemView (I/Chat/ChatItem/FramedItemView.swift:366)   the markdownBlocks of the item
chatItemPreviewText (I/ChatList/ChatPreviewView.swift:305-316, 302)   nil where formattedText is nil, otherwise ci.markdownBlocks
TextBubble (I/Chat/ChatItemInfoView.swift:298-319)          the markdownBlocks of the item or the version
ContextItemView (I/Chat/ComposeMessage/ContextItemView.swift:74)   contextItem.markdownBlocks
markdownText (I/Chat/ChatItem/MsgContentView.swift:331-352)  blocks of parseSimpleXMarkdownText
notifications (S/Notifications.swift:200-210)               section 3
parseSimpleXMarkdownText (S/API.swift)                      new: (_ s: String) -> ParsedMarkdown?
parseSimpleXMarkdown (S/API.swift:151-162)                  the formattedText of parseSimpleXMarkdownText
```

Without blocks:

```
K/views/chatlist/ChatPreviewView.kt:233                     draft: formattedText null
K/views/chat/ChatInfoView.kt:811-813                        teaser with Format.Modal appended
I/Chat/ChatItem/MsgContentView.swift:380-383                teaser with Format.modal appended
```

Unchanged:

```
K/views/chat/item/FramedItemView.kt:349        short emoji test: formattedText == null
I/Chat/ChatItem/FramedItemView.swift:145       short emoji test: formattedText == nil
K/views/chat/ComposeView.kt:130                edit of a sent item: the token form, for links and mentions
K/views/usersettings/MarkdownHelpView.kt       inline formats
I/UserSettings/MarkdownHelp.swift              inline formats
```

## 5. Web

`renderMarkdown(fts, blocks)` (`W/simplex-lib.jsc:57-128`):

```
blocks absent    as now; escape: nothing
blocks invalid   as now; escape: nothing
                 invalid: not an array
                          an unknown type
                          a range not of integers with 0 <= from <= to <= fts.length
                          a range starting before the end of the previous range, in the order of blockParts
                          a list deeper than 7 levels
blocks           the order of blockParts (section 3), each block one element below
range            each segment drawn as now
                 SoftBreak: " "
                 LineBreak, HardBreak: <br>
                 other markup, escape: nothing
                 Bold, singleStar: <em>
gaps             between blocks and between list entries: LineBreak nothing; ParagraphBreak: one <br> less than the line breaks of its text
Paragraph        <div>: the range
Heading          <div style="font-weight: bold;">: the range
CodeBlock        <pre style="font-family: monospace; white-space: pre-wrap;">: the range
Quote            <div style="border-left: 3px solid; padding-left: 0.5em;">: the range
List             each entry, in source order: <div style="padding-left: Nem;">, "•" or the number, a space, then the range
                 N: 1.5 × the level below the top, at most 6 levels
Table            <div style="overflow-x: auto;"><table style="border-collapse: collapse;">: the header row in <th>, the body rows in <td>
                 each cell: style="text-align: A; border: 1px solid; padding: 0 0.4em;"
                 A: the column alignment, none as left, any other value as left
                 a missing cell at a row end: no element
Rule             <hr>
number           escaped text
```

Call sites:

```
W/channel-preview.jsc:880-887       the welcome message object: markdownBlocks: data.welcomeMessageBlocks
W/channel-preview.jsc:990           renderMarkdown(shortDescr, data.shortDescriptionBlocks)
W/channel-preview.jsc:1522          renderMarkdown(msg.formattedText, msg.markdownBlocks)
W/directory.jsc:232                 shortDescrBlocks, welcomeMessageBlocks from the entry
W/directory.jsc:251-255             renderMarkdown with the blocks of each list
W/directory.jsc:254, 260            <p> kept: set by innerHTML, the block elements become children of the <p>
```

- The meta span after a message with a block at its end (`W/channel-preview.jsc:1522`) is drawn on its own line.
- Differences from the apps: a table is an HTML table, scrolled sideways when wider than the message.

## 6. Docs

```
apps/multiplatform/spec/client/chat-view.md   TextItemView.kt: blocks drawn by blockParts from markdownBlocks
apps/ios/spec/client/chat-view.md             MsgContentView: the same
```

## 7. Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/MarkdownBlocksTest.kt`:

```
decoding           each block token in the JSON of the core
                   each block, a list with nested entries
                   each TableAlign value
                   with and without language
                   with and without number
                   Bold with and without singleStar; Escape
blockParts         the parts, drawn as texts with fonts, for each block kind
                   gaps before the first block, between blocks, after the last block
                   quote and list entry lines divided at HardBreak
                   nested list entries: order and indentation
                   a table with each alignment
                   a table with a missing cell and an empty cell
                   a table with emoji, CJK and a mention in cells: widths in columns of the displayed texts
                   a cell wider than 40 columns
                   ranges beyond the length of the list
displayWidth       ASCII, CJK, an emoji, a modifier sequence, a ZWJ sequence, a flag, U+FE0F, a combining mark
segment texts      each token kind and Escape: segmentDisplayText as in section 3
selection          itemDisplayParts: displayed and copied texts of a quote, Escape without blocks
```

Checks on each platform of the apps:

1. A sent message with each block kind: drawn by section 3.
2. A received message with each block kind: drawn by section 3.
3. A text without blocks: drawn as before.
4. Inline parts in each block with content: actions as in a paragraph.

```
inline parts   a link
               a mention
               a secret
               a command
blocks         a heading
               a list item
               a quote
               a table cell
```

5. A message with blocks in a quote, the chat list preview, the compose context, item info and a notification.
6. Edit of a sent message with blocks: the compose field has the source text.
7. Desktop: selection inside a block, across blocks, across items, and its copy.
8. Live message with an open code fence.
9. Live message with a growing table.
10. Chat link item with text ending with a heading: no empty line at the end of the text.
11. A profile description and a welcome message with a list.
12. iOS: the cases of `MarkdownBlocksTest.kt` drawn equal to Android.

Web:

13. A channel page with a message of each block kind.
14. A directory entry with a welcome message with a list.
15. Relay JSON with hostile values: `number` with markup, `columns` with markup, a range out of bounds, a range not of integers, an unknown block type.

Not automated:

```
iOS          the Swift blockParts: check 12 only
web script   checks 13-15 only
```

## Interfaces

```
app types   Format: Markup, Escape, Bold singleStar
            BlockToken
            MarkdownBlock
            ListEntry
            SegmentRange
            TableAlign
            BlockFont
            BlockPart
records     markdownBlocks: ChatItem, CIQuote, ChatItemVersion, ParsedFormattedText (Kotlin), ParsedMarkdown (Swift)
functions   blockParts
            displayWidth
            segmentDisplayText
            itemDisplayParts (Kotlin)
            parseToMarkdownText (Kotlin), parseSimpleXMarkdownText (Swift)
            the segment loop: over the parts; Swift: nested function appendSegments
files       MarkdownBlocksTest.kt
```

## Depends on

```
block Markdown plan, "Types"                      constructors, fields, JSON tags
block Markdown plan, "Pass 2: blocks"            ranges
block Markdown plan, "Records and parse sites"    fields
release                                           both plans together
```
