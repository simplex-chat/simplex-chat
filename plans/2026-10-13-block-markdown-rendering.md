# Block Markdown: rendering in the apps and on the web

Date: 2026-10-13. Revised: 2026-10-16, for master. Revised: 2026-10-17, block markers in the flat list.

The apps and the web page script draw the marker form of `formattedText` (`2026-10-13-block-markdown.md`, section 3). The apps draw one text per item: the one-text form of section 3, with the present inline formats. The web page script draws HTML blocks.

References are to ep/markdown 382dc7480.

```
K/   apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/
I/   apps/ios/Shared/Views/
S/   apps/ios/SimpleXChat/
W/   website/src/js/
```

## 1. Scope

```
one-text form   every list drawn by MarkdownText (K/views/chat/item/TextItemView.kt:125)
                every list drawn by messageText (I/Chat/ChatItem/MsgContentView.swift:396)
                desktop selection (K/views/chat/TextSelection.kt:264-307)
                  a selection copies the one-text form of the selected range: markers, padding, separators
                  the Copy action of an item copies its source text, as now
                notification texts (K/platform/NtfManager.kt:135, S/Notifications.swift:201)
marker form     compose state: parsedMessage, links, mentions, link sanitizing, as now
                  K/views/chat/ComposeView.kt:111, 130, 1024-1070
                  I/Chat/ComposeMessage/ComposeView.swift:560, 1240, 1990
HTML blocks     renderMarkdown (W/simplex-lib.jsc:57-128)
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
- Horizontal scrolling belongs to the terminal layout (ep/llm-ux).

## 2. Types

Kotlin, `K/model/ChatModel.kt`:

```
Format (:5177)     @Serializable @SerialName("blockStart") class BlockStart(val block: MarkdownBlock): Format()
                   @Serializable @SerialName("blockEnd") class BlockEnd: Format()
                   @Serializable @SerialName("lineBreak") class LineBreak: Format()
                   @Serializable @SerialName("cellBreak") class CellBreak: Format()
                   style of the four: SpanStyle()
MarkdownBlock      @Serializable sealed class, next to Format
                     @SerialName("paragraph") object Paragraph
                     @SerialName("heading") class Heading(val level: Int)
                     @SerialName("codeBlock") class CodeBlock(val language: String? = null)
                     @SerialName("quote") object Quote
                     @SerialName("listItem") class ListItem(val number: String? = null, val indent: Int)
                     @SerialName("table") class Table(val columns: List<TableAlign>)
                     @SerialName("rule") object Rule
TableAlign         @Serializable enum class: none, left, center, right
markdownSegments   (fts: List<FormattedText>?) -> List<FormattedText>?
                   section 3
```

iOS, `S/ChatTypes.swift`:

```
Format (:5681)     case blockStart(block: MarkdownBlock)
                   case blockEnd
                   case lineBreak
                   case cellBreak
MarkdownBlock      enum: Decodable, Equatable, Hashable
                   cases as the Kotlin classes, associated values as their fields
TableAlign         enum: String, Decodable, Hashable
markdownSegments   public func (_ fts: [FormattedText]?) -> [FormattedText]?
                   section 3
```

- `FormattedText` is unchanged.
- `formattedText` fields are unchanged: lists of `FormattedText`.
- An unknown block tag makes the item invalid, as an unknown `Format` tag.
- Each exhaustive dispatch on `Format` draws a marker as its text.

## 3. One-text form

`markdownSegments`:

```
null                 null
no BlockStart        the list, unchanged
blocks               the segments outside blocks, except markers: unchanged
                       the core puts white space there; an app appends other segments after a list (Format.Modal, K/views/chat/ChatInfoView.kt:811, I/Chat/ChatItem/MsgContentView.swift:380)
                     each block: the segments below
                     the text of a marker is not drawn
```

Lines and cells of a block:

```
line         the content segments between two markers of the block
             LineBreak divides lines
             a line without content segments: an empty line
cells        in a Table: CellBreak divides the cells of a line
soft break   a LineBreak between two lines with content: " "
hard break   any other LineBreak: "\n"
```

Blocks:

```
Paragraph    the lines with soft and hard breaks
Heading      the line; its segments without format: Bold
ListItem     a segment without format: 4 spaces per indent, "•" or the number, a space
             then the lines with soft and hard breaks
Quote        a segment without format: "│ "
             then the lines with soft and hard breaks, each hard break "\n│ "
CodeBlock    each line: one Snippet segment
             each LineBreak: "\n", Snippet
Table        each row: the cells, padded with spaces to the column width
             " │ " between cells
             padding by the column alignment
               none         after the text
               left         after the text
               right        before the text
               center       half before, rounded down
                            the rest after
             column width: the largest length of a cell of the column
             under the header row: a run of "─" of the column width per column
             "─┼─" between runs
             a line break between rows
             Snippet
               the separators
               the padding
               the line under the header
               the line breaks between rows
               the segments without format
Rule         "———", without format
length       the number of Unicode code points
               Kotlin: String.codePointCount
               Swift: String.unicodeScalars.count
             of the texts of the segments of a cell
```

- Adjacent segments are not joined.
- Columns are aligned by the code point counts of the segment texts. Not aligned:

```
a cell with a format other than Snippet: proportional font
a cell with a mention, a link or a command: display text differs from its text
a character drawn wider than one column: CJK, emoji
a row wider than the bubble: wrapped
```

## 4. Call sites

```
Kotlin MarkdownText (K/views/chat/item/TextItemView.kt:125)
  formattedText    remember(formattedText) { markdownSegments(formattedText) }, before stripFormattedTextLink
iOS messageText (I/Chat/ChatItem/MsgContentView.swift:396)
  formattedText    markdownSegments(formattedText), before stripFormattedTextLink
desktop selection
  K/views/chat/TextSelection.kt:270, 296    markdownSegments(ci.formattedText)
notifications
  K/platform/NtfManager.kt:135              markdownSegments(cItem.formattedText)
  S/Notifications.swift:201                 markdownSegments of the item's formattedText
```

Unchanged:

```
K/views/chat/item/FramedItemView.kt:349        short emoji test: formattedText == null
I/Chat/ChatItem/FramedItemView.swift:145       short emoji test: formattedText == nil
K/views/chat/ComposeView.kt:130                edit of a sent item: the marker form, for links and mentions
stripFormattedTextLink (K/views/chat/item/TextItemView.kt:622-632, I/Chat/ChatItem/MsgContentView.swift:608-616)
                                               chat link items: applied to the one-text form
K/views/usersettings/MarkdownHelpView.kt       inline formats
I/UserSettings/MarkdownHelp.swift              inline formats
```

## 5. Web

`renderMarkdown` (`W/simplex-lib.jsc:57-128`):

```
no blockStart   as now
blocks          the segments outside blocks without format: a <br> for each line break after the first
                other segments outside blocks, except markers: drawn as now
                each block: one element below
                the text of a marker is not drawn
lines           the lines with soft and hard breaks of section 3, a hard break as <br>
Paragraph       <div>: the lines
Heading         <div style="font-weight: bold;">: the lines, all segments bold
ListItem        <div style="padding-left: Nem;">: "•" or the number, a space, then the lines
                N: 1.5 × indent
Quote           <div style="border-left: 3px solid; padding-left: 0.5em;">: the lines
CodeBlock       <pre style="font-family: monospace; white-space: pre-wrap;">: the lines with "\n"
Table           <div style="overflow-x: auto;"><table style="border-collapse: collapse;">: the header row in <th>, the body rows in <td>
                each cell: style="text-align: A; border: 1px solid; padding: 0 0.4em;"
                A: the column alignment, none as left, any other value as left
Rule            <hr>
other blocks    as Paragraph
content         each segment drawn as now
values          indent: an integer from 0 to 6; number: escaped text
```

Script functions, `W/simplex-lib.jsc`, before `renderMarkdown`:

```
markerType      the marker type of a segment, or null
splitByMarker   lines of a block, cells of a line
renderLines     the lines with soft breaks " " and hard breaks <br>
renderTable     a Table block
renderBlock     one block
renderBlocks    the marker form
```

Call sites, unchanged:

```
W/channel-preview.jsc:990, 1522
W/directory.jsc:251-255
```

- The meta span after a message that ends with a block (`W/channel-preview.jsc:1522`) is drawn on its own line.
- Differences from the apps: a Heading is bold with its formats; a table is an HTML table, scrolled sideways when wider than the message.

## 6. Docs

```
apps/multiplatform/spec/client/chat-view.md   TextItemView.kt: blocks drawn in the one-text form
apps/ios/spec/client/chat-view.md             MsgContentView: the same
```

## 7. Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/MarkdownBlocksTest.kt`:

```
decoding           each marker in the JSON of the core
                   each block value
                   each TableAlign value
                   with and without language
                   with and without number
markdownSegments   no markers: the list
                   each block kind
                   soft and hard breaks in a paragraph, a list item and a quote
                   an empty code line
                   a table with each alignment
                   a table with an empty cell
                   a table with emoji in a cell
                   the segments outside blocks
comparison         the text of each segment
                   the format class of each segment
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
15. Relay JSON with hostile block values: `number` with markup, `columns` with markup, `indent` not a number, an unknown block type.

Not automated:

```
iOS          the Swift markdownSegments: check 12 only
web script   checks 13-15 only
```

## Interfaces

```
app types   Format: BlockStart, BlockEnd, LineBreak, CellBreak
            MarkdownBlock
            TableAlign
functions   markdownSegments
files       MarkdownBlocksTest.kt
```

## Depends on

```
block Markdown plan, "Types"         constructors, fields, JSON tags
block Markdown plan, "Marker form"   the parser output
release                              both plans together
```
