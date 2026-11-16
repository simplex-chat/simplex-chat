# Block Markdown: rendering in the apps and on the web

Date: 2026-10-13. Revised: 2026-10-16, for master. Revised: 2026-10-17, drawing from markdownBlocks. Revised: 2026-10-18, drawing by index ranges (S5). Revised: 2026-10-18, a column of block views (approved mockup https://claude.ai/artifact/7Twe2qJcdULBbcqhSPuh6k, v3).

Where a whole message is shown, the apps draw `formattedText` with `markdownBlocks` (`2026-10-13-block-markdown.md`, sections 2 and 4) as a column of block views; each text view is the segment loop of master over the index range of one block, list entry or table cell (section 3). Previews, quotes, the compose context, the profile teaser and notifications stay one text. A message without blocks is drawn as now. On the web, HTML blocks are drawn by the page script.

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

- Per text view, as in one text now:
  - taps
  - long presses
  - secrets
  - mentions
  - commands
- Per message:
  - the typing indicator and the meta reserve: at the end of the last view when it is text, otherwise in a line below
  - the meta reserve after text: a line break when the text direction differs from the bubble's, as now; spaces in the line below (iOS `reserveSpaceForMeta(mt, newLine: false)`)
  - text direction: the column in the text direction, text aligned to its start
- Code blocks and tables scroll sideways; a drag that starts in them does not swipe the item.

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
BlockToken.drawnText the text of the token in a block view (section 3)
BlockToken.previewText the text of the token in one text (section 3)
BlockTextRange       class BlockTextRange(val range: SegmentRange, val inRow: Boolean)
MarkdownBlock.textRanges   the ranges of the text views of a block, in source order
                       Paragraph, Heading, CodeBlock, Quote: content
                       ItemList: the contents of the entries, a parent before its children
                       Table: the cells of the header, then of each row; inRow for each cell after the first of a row
                       Rule: none
listEntries          (items, level) -> List<Pair<ListEntry, Int>>: the entries with their levels, a parent before its children
blockTextRanges      (blocks) -> the textRanges of all blocks
headingSegments      (size, blocks) -> BooleanArray: the segments in heading contents
segmentDisplayText   (ft: FormattedText, mentions: Map<String, CIMention>?, linkMode: SimplexLinkMode) -> String
                     the body of itemSegmentDisplayText (K/views/chat/item/TextItemView.kt:88-108), with mentions instead of ci
                     Escape and Markup: the text drawn for the token in a block view
                     replaces itemSegmentDisplayText
itemDisplay          (ci: ChatItem, linkMode: SimplexLinkMode) -> ItemDisplay?, null without formattedText
                     parts: per segment the displayed and the copied text (Markup: its drawn text both);
                       between text views a part "\t" (between cells of a row) or "\n"
                     bases: per text view the offset of its first character in the selection offsets of the item; 0 for the first
                     without markdownBlocks: the segments, bases [0]
MarkdownBlocks       composable: the column of block views (section 3)
```

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
drawnText, previewText, BlockTextRange, textRanges, listEntries, blockTextRanges, headingSegments: as Kotlin
MarkdownBlocksView   SwiftUI view, generic over the block text view, in I/Chat/ChatItem/MsgContentView.swift (section 3)
MarkdownView         SwiftUI view: one text, or MarkdownBlocksView when blocks are present
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
                    Kotlin: MarkdownText (K/views/chat/item/TextItemView.kt)
                    Swift: messageText (I/Chat/ChatItem/MsgContentView.swift), the loop in a nested function appendSegments
range             a block view: the range of its block, entry or cell; otherwise all indices
font              Kotlin: the style passed by the column
                  Swift: parameter baseFont; the fonts of formats and links derived from it
Bold singleStar   italic, when markdownBlocks is present
HyperLink         linkUri "mailto:" (e-mail autolink): opened as an e-mail link
                    Kotlin: annotation OTHER_URL; Swift: without webLinkAttrKey
                  FormattedText.linkUri: null for it, so no link preview
```

Modes of the segment loop (Kotlin `MarkdownText`, Swift `messageText`):

```
block view    parameter range (a text view of the column)
                tokens by drawnText: SoftBreak " ", LineBreak and HardBreak "\n", ParagraphBreak n × "\n", other markup nothing
one text      markdownBlocks present, no range (previews, quotes, compose context, profile teaser)
                tokens by previewText: SoftBreak, HardBreak and CellMark " "; LineBreak and ParagraphBreak "\n";
                ListMark the number or "•" and " "; other markup nothing
                segments in heading contents (headingSegments) bold
no blocks     markup drawn as its text; Escape nothing
```

The column (Kotlin `MarkdownBlocks`, `blockLayout = true` of `MarkdownText`; Swift `MarkdownBlocksView`, `blockLayout` of `MsgContentView`, and `MarkdownView`):

```
order         blocks in source order; text view index i = the i-th of blockTextRanges
spacing       a ParagraphBreak between two blocks: half a line; otherwise none
Paragraph     text view, body font
Heading       text view, bold; size × 1.5 (level 1), 1.3 (2), 1.15 (3), 1 (4-6); line height scaled alike
List          per entry (listEntries): a row of the marker ("•" or the number) and the text view; indent 20 per level
Quote         a 4 wide bar (text colour at 16%), 10 gap, the text view at 70-72% opacity
CodeBlock     card in the bubble's quote colour, corners 12 × the bubble roundness setting, the bubble's full width
                the text view: monospace, size × 0.875, no wrapping, scrolls sideways
                copy icon at the top right: on hover (desktop) or after a tap (mobile); copies the drawn text of the range
Table         rounded frame (12 × roundness, 1 line), scrolls sideways
                columns as wide as their widest cell; header row bold on the quote colour; alternate body rows tinted
                lines between rows and columns; a cell aligned by its column
                a missing cell at a row end: an empty cell (GFM 0.29 example 204)
                Kotlin: a Layout measuring cells by max intrinsic width, composing only the present cells
                iOS 16+: Grid, columns × rows at most 1000 (maxGridCells); otherwise, and on iOS 15: the table as typed in a code card
Rule          1 line, 6 above and below
first view    gets the sender and the report prefix
last view     gets the meta reserve and the typing indicator when it is text; otherwise a line with them below the column
ranges        limited to the length of fts
```

- Quote colour: `sentQuote` in a sent bubble, `receivedQuote` otherwise (Kotlin `AppColors`, Swift `theme.appColors`).
- Swipe to reply: Kotlin: the horizontal scroll consumes the drag before `swipeable` of the item. iOS: scrolling blocks report their frames (`SwipeExclusionFrames`) in the item's coordinate space; the item's drag gesture ignores drags that start in them.
- iOS: a horizontal scroll only when the content is wider than the bubble (`ViewThatFits` on iOS 16+).

Secrets (Kotlin, as iOS `hiddenSecretsView`): a hidden secret is drawn transparent, and its text path is painted with the tile `vertical_logo` (8 dp, rotated 45°, saturation 0.65, alpha 0.35), added to `moko-resources/images` from the iOS asset.

Desktop selection (K/views/chat/TextSelection.kt):

```
setupItemSelection   (selectionManager, selectionIndex, isLive, bases)
text views           each registers its bounds and layout with its base (ItemSelection.text(index))
pointer offset       the view under the pointer, or the nearest; base + its local offset
highlight            per view, the selected offsets of the item within the view, shifted by its base
copy, snap           itemDisplay(ci).parts: copied text as before, with "\t" and "\n" between views
```

Notifications, `hideSecrets` (K/platform/NtfManager.kt, S/Notifications.swift): Secret "...", markup by previewText, Escape nothing, other segments their text.

## 4. Call sites

Kotlin:

```
MarkdownText (K/views/chat/item/TextItemView.kt)            parameters markdownBlocks, blockLayout, range, softWrap, cardColor, selection, textIndex, parentDirection (the bubble's direction, for the meta reserve of block views); section 3
CIMarkdownText (K/views/chat/item/FramedItemView.kt:445)    ci.markdownBlocks; column; quote colour by direction; selection with itemDisplay bases
ciQuotedMsgTextView (K/views/chat/item/FramedItemView.kt:61) qi.markdownBlocks; one text
item preview (K/views/chatlist/ChatPreviewView.kt:261-265, 277)   null where formattedText is null, otherwise ci.markdownBlocks; one text
TextBubble (K/views/chat/ChatItemInfoView.kt:55-57)         the markdownBlocks of the item or the version; column
ContextItemView (K/views/chat/ContextItemView.kt:59)        contextItem.markdownBlocks; one text
ProfileDescriptionText (K/views/chat/ChatInfoView.kt:794-795, 804-805)   blocks of parseToMarkdownText; one text
showFullProfileDescription (K/views/chat/item/TextItemView.kt:56-58)     blocks of parseToMarkdownText; column
WelcomeMessageView (K/views/chat/group/WelcomeMessageView.kt:149-151)    blocks of parseToMarkdownText; column
desktop selection (K/views/chat/TextSelection.kt:264-305)   section 3; itemDisplay
notifications (K/platform/NtfManager.kt:134-148)            section 3
parseToMarkdownText (K/views/helpers/TextEditor.kt)         new: (text: String) -> ParsedFormattedText?
parseToMarkdown (K/views/helpers/TextEditor.kt:138-146)     the formattedText of parseToMarkdownText
```

iOS:

```
messageText (I/Chat/ChatItem/MsgContentView.swift)          parameters markdownBlocks, range, baseFont, secretStart; section 3
MsgContentView (I/Chat/ChatItem/MsgContentView.swift)       vars markdownBlocks, blockLayout, cardColor; MarkdownBlocksView rightToLeft from its rightToLeft, MarkdownView from isRightToLeft(text)
item content (I/Chat/ChatItem/MsgContentView.swift:624)     chatItem.markdownBlocks
quote (I/Chat/ChatItem/FramedItemView.swift:337)            qi.markdownBlocks; one text
FramedItemView (I/Chat/ChatItem/FramedItemView.swift:366)   the markdownBlocks of the item; column; quote colour by direction
chatItemPreviewText (I/ChatList/ChatPreviewView.swift:305-316, 302)   nil where formattedText is nil, otherwise ci.markdownBlocks; one text
TextBubble (I/Chat/ChatItemInfoView.swift:298-319)          the markdownBlocks of the item or the version; MarkdownView
ContextItemView (I/Chat/ComposeMessage/ContextItemView.swift:74)   contextItem.markdownBlocks; one text
markdownText (I/Chat/ChatItem/MsgContentView.swift:331-352)  blocks of parseSimpleXMarkdownText; one text
FullProfileDescriptionView, GroupWelcomeView textPreview    MarkdownView with blocks of parseSimpleXMarkdownText
item swipe (I/Chat/ChatView.swift chatItemWithMenu)          ignores drags starting in SwipeExclusionFrames
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
                          a range starting before the end of the previous range, in source order
                          a list deeper than 7 levels
blocks           in source order, each block one element below
range            each segment drawn as now
                 SoftBreak: " "
                 LineBreak, HardBreak: <br>
                 other markup, escape: nothing
                 Bold, singleStar: <em>
gaps             a ParagraphBreak between two blocks: <div class="md-gap"></div>; otherwise nothing
Paragraph        <div>: the range
Heading          <div class="md-hN">: the range; N: the level 1-3, otherwise 4
CodeBlock        <div class="md-code"><pre>: the range, then a copy button (class md-copy, icons md-icon-copy and md-icon-done)
Quote            <div class="md-quote"><span class="md-bar"></span><div class="md-text">: the range
List             each entry, in source order: <div class="md-entry" style="padding-left: Nem;"><span class="md-mark">: "•" or the number; <span class="md-text">: the range
                 N: 1.25 × the level below the top, at most 6 levels
Table            <div class="md-table"><div class="md-frame"><table>: the header row in <th>, the body rows in <td>
                 each cell: style="text-align: A;"; A: the column alignment, none as left, any other value as left
                 rows padded with empty cells to the column count when columns × rows <= 1000 (MAX_TABLE_CELLS)
Rule             <hr class="md-rule">
number           escaped text
copy             handleCodeClick: a click on the button copies the text of the <pre> (innerText); a click elsewhere in the card toggles the class tapped
```

Styles, as the apps (section 3), in each page: `.simplex-preview-text .md-*` in W/channel-preview.jsc (colours from its `--sp-*` variables, card `--sp-quote`, corners 10px for its 18px bubbles), `#directory .entry .md-*` in website/src/directory.html (light and `.dark` values, corners 4px as its entries). The flex items around the text get `min-width: 0` (`.simplex-preview-msg-col`, `#directory .entry .text-container`), so that wide blocks scroll inside the message. `copyToClipboard` moved from W/channel-preview.jsc to W/simplex-lib.jsc.

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
- Click handlers: W/channel-preview.jsc `setupSecretToggles` calls `handleCodeClick`; W/directory.jsc `initDirectory` adds it on `#directory`.

## 6. Docs

```
apps/multiplatform/spec/client/chat-view.md   TextItemView.kt: blocks of markdownBlocks drawn as a column (MarkdownBlocks.kt)
apps/ios/spec/client/chat-view.md             MsgContentView: the same (MarkdownBlocksView)
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
text ranges        blockTextRanges for each block kind: order, inRow of table cells
list entries       listEntries: order and levels
headings           headingSegments, also with a shortened list
token texts        drawnText and previewText of each token kind; Escape; one-text previews of a heading, paragraph and list
selection          itemDisplay: displayed and copied texts and bases of a quote and a table; Escape without blocks
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
iOS          the Swift views: check 12 only
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
            BlockTextRange
records     markdownBlocks: ChatItem, CIQuote, ChatItemVersion, ParsedFormattedText (Kotlin), ParsedMarkdown (Swift)
functions   textRanges, blockTextRanges, listEntries, headingSegments
            segmentDisplayText, itemDisplay (Kotlin)
            parseToMarkdownText (Kotlin), parseSimpleXMarkdownText (Swift)
            the segment loop: range; Swift: baseFont, secretStart, nested function appendSegments
views       MarkdownBlocks (Kotlin), MarkdownBlocksView and MarkdownView (Swift)
selection   ItemSelection per text view (Kotlin)
resources   vertical_logo@1x, @2x, @3x.png (Kotlin, from the iOS asset)
files       MarkdownBlocksTest.kt, MarkdownBlocks.kt
```

## Depends on

```
block Markdown plan, "Types"                      constructors, fields, JSON tags
block Markdown plan, "Pass 2: blocks"            ranges
block Markdown plan, "Records and parse sites"    fields
release                                           both plans together
```
