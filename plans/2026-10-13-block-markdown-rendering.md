# Block markdown: rendering in the apps

Date: 2026-10-13.

The Android, desktop and iOS apps decode the block segments of `formattedText` (`2026-10-13-block-markdown.md`) and draw the text of a chat item as a column of block views.

Paths: `K/` is `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`; `I/` is `apps/ios/Shared/Views/Chat/`.

## 1. Scope

```
block view   text of a chat item in a chat view: main chat view in both layouts, member support chat, reports view
             viewer of the read-more plan (2.7)
one text     every other place where a message text is drawn (section 6)
part 1       release 1, with the flat layout plan and the Strong change of the core parser (strongP, block Markdown plan):
             app types; Format.style; the Strong and marker cases of MarkdownText and messageText (sections 3, 4)
part 2       release 2, with block parsing of the core: every other change of this plan; applied after the flat layout and read-more plans
```

- Block view: used for a segment list with a marker segment (`Format.isMarker`). A list without markers is drawn as now: `MarkdownText` (`K/views/chat/item/TextItemView.kt:125`), `MsgContentView` (`I/ChatItem/MsgContentView.swift:28`).

## 2. Specification

### 2.1. Sketches

```
bubbles
+------------------------------------------+
| Build steps                              |   heading, level 2
| | bash                          [copy] | |   code: language, copy button
| | ./build.sh --target android --releas>| |   one line per source line; scrolled inside the block
| | Target | Time  | Size  |               |   table: grid; row 1 bold, with background
| | arm64  | 4 min | 38 MB |               |   wider than the bubble: scrolled inside the block
|                                    10:42 |   meta on its own line: the last block is a table
+------------------------------------------+

flat
(A) Agent  10:42
Build steps
| bash                                                        [copy] |   code block over the content width
| ./build.sh --target android --release                              |
| Target | Time  | Size  |                                               table at the row start
| arm64  | 4 min | 38 MB |
                                                               [meta]
```

### 2.2. Blocks

```
msgBlocks(segments, prefix):
  cur = paragraph, quoteDepth 0, listDepth 0
  for each segment, with its index:
    Block b q d   -> cur is closed; cur = b, quoteDepth q, listDepth d
    TableRow      -> a new row of cur; skipped when cur is another block
    TableCell     -> a new cell of the current row; skipped when cur is another block
    other         -> part of cur; in a table, of the current cell
  cur is closed
a closed paragraph that is empty or has Syntax segments only is dropped; the first block is kept when the prefix is not empty
```

- A block text and a table cell are ranges of indices of the segment list. `Syntax` segments of a range are skipped when it is drawn.
- `prefix`: the report reason (`itemPrefixText`, `TextItemView.kt:118`; `reason.attrString`, `I/ChatItem/FramedItemView.swift:196`). It is drawn at the start of the first block, a paragraph.
- Chat link item: the link is removed from the list before `msgBlocks`. `stripFormattedTextLink` (`TextItemView.kt:622`, `MsgContentView.swift:608`): after the link is removed, a last marker with white-space text is removed.

```
block       view                    drawing
paragraph   one text                as now
heading     one text                bold; body font size for levels 4 to 6
                                    levels 1, 2, 3: font size x 1.375, 1.25, 1.125
list item   row: marker, one text   marker: "•", or `number` as written, end-aligned in a column of 22, scaled with the font
                                    a wider marker widens the column of its item
code        column: header, text    header: `language` in one line with ellipsis; copy button at the end
                                    text: monospace, font size x 0.875, one line per source line, left-to-right
                                    a line above CODE_MAX_WIDTH (2000) is wrapped
                                    background: quote colour; corners 8 x roundness
table       grid of cell texts      column width: widest cell, within 40 .. 240; cell text is wrapped
                                    column alignment: `columns`; none, or a cell beyond `columns`: start
                                    row 1: bold, quote colour; grid lines 1 wide; a missing cell is drawn empty
rule        line                    1 high, secondary colour, block width

quoteDepth q   start padding q x 12; one bar per level: 3 wide, secondary colour, over the block and the gap before it; q above 3 is drawn as 3
listDepth d    start padding d x 22: the column of the text of a list item with listDepth d - 1; d above 6 is drawn as 6
gap before     list item after a list item: 2; heading: 12; other block: 8; first block: 0; sizes: dp in Kotlin, points in iOS
limits         MSG_MAX_BLOCKS = MSG_FLAT_MAX_LINES (500) block views in one item: the teaser ends before the Block marker number MSG_MAX_BLOCKS + 1 (msgTeaser, argument maxBlocks; read-more plan)
               MSG_MAX_TABLE_CELLS = 600 cell views in one item; MSG_MAX_TABLE_COLUMNS = 32
               a table above a limit is a code block: its text from the first "|" of its marker is one monospace text, drawn as written
               release condition for this value of MSG_MAX_BLOCKS: a measurement of a live item on a mid-range Android phone and an iPhone
```

### 2.3. Layouts

```
                bubbles                                               flat                       viewer
block width     text width of the bubble                              content width of the row   viewer width
bubble width    width of the widest block, within the present limit   -                          -
quote colour    sentQuote or receivedQuote, by direction              receivedQuote              receivedQuote
```

- Quote colour: Kotlin `MaterialTheme.appColors` (`K/views/chat/item/FramedItemView.kt:121-122`), iOS `chatItemFrameContextColor` (`FramedItemView.swift:443`).
- Item limits: flat layout plan (2.7) and read-more plan; the teaser is cut from the segment list before `msgBlocks`. `Format.Bold`: flat layout plan (2.5); the `chatLayout` parameter is passed to every text of a block view. In the viewer its value is passed by `showFullMessage`.

### 2.4. Meta line, typing indicator, direction

```
last block: paragraph, heading, list item   typing indicator and meta reserve at the end of its text, as now (TextItemView.kt:344-351, MsgContentView.swift:115-119)
last block: code, table, rule               one text line after it, with the typing indicator and the meta reserve
meta reserve                                reserveSpaceForMeta (K/views/chat/item/CIMetaView.kt:130), as now
                                            with the partsMissing mark of the message parts plan (CIMetaView.kt:83, :155)
meta view                                   bottom-end corner, as now (FramedItemView.kt:431-439, FramedItemView.swift:114-125)
block direction                             Kotlin: of the block text, its first 50 characters, as now (TextItemView.kt:153-155)
                                            iOS: of the item, isRightToLeft of the item text, as now (FramedItemView.swift:361)
                                            code: left-to-right; the marker row, the paddings and the bars are placed by the block direction
```

### 2.5. Inline formats, taps, copy

- Text of a paragraph, heading, list item and table cell: drawn by the segment code of `MarkdownText` (`TextItemView.kt:229-343`) and `messageText` (`MsgContentView.swift:446-570`). `Strong` is bold. Code is plain text.
- Taps, long presses and hover: handled per text, as now. Kotlin: `ClickableText` (`:356-414`). iOS: `msgTextResultView` (`:139-150`).
- Shown secrets: state per text, as now (`TextItemView.kt:172`, `MsgContentView.swift:43`). Kotlin key: index of the segment in the list of the item. `mentions`, `userMemberId` and the command action of the item are passed to every text of the item.
- Item menu and its Copy action: unchanged (`K/views/chat/item/ChatItemView.kt:323-335, 445-448`, `I/ChatView.swift:2148, 2494-2506`). Copy button of a code block: the text of the block. Desktop selection: section 5.

### 2.6. Horizontal scroll, live items, cost

```
scroll enabled     while the content is wider than the block
Android            CodeBlockView, TableBlockView: Modifier.horizontalScroll, inside SwipeToDismissModifier of the row (K/views/helpers/Modifiers.kt:37)
                   the block is scrolled by a horizontal drag that the user begins inside an enabled scroll area; the user begins the reply swipe outside such areas
                   touch slop: 50 px, the value of the row (K/views/chat/ChatView.kt:1939, :3665)
iOS                chatItemWithMenu (section 4): the reply drag is ignored when the user begins it inside an enabled scroll area
                   EndlessScrollView.gestureRecognizer(_:shouldRecognizeSimultaneouslyWith:) (I/EndlessScrollView.swift:658): false when the view of the other recogniser is a UIScrollView inside a cell
desktop            Modifier.horizontalScroll: Shift + wheel, touchpad
item update        msgBlocks: one pass over the segments; Kotlin: the blocks are composed again, a text is laid out again when its content changed
                   iOS: the cell content is set and measured again (EndlessScrollView.swift:64-81), as now
typing indicator   updated every 250 ms in the last text of the item only (TextItemView.kt:178-183, MsgContentView.swift:84-86)
open code fence    code block to the end of the text; typing indicator on the line after it
kept               by block position and type: scroll offset (rememberScrollState; ScrollView), shown secrets (2.5)
views per row      at most MSG_MAX_BLOCKS blocks and MSG_MAX_TABLE_CELLS cells; one row of the lazy list per item, as now
```

### 2.7. Viewer

```
items        list without markers: msgLines (read-more plan)
             list with markers: segments without format are divided at line breaks, as in msgLines; then msgBlocks
             paragraph, heading, list item, code: one lazy item per line of the block; a line above 15,000 units is cut by the rule of the read-more plan
             table: one lazy item; MSG_MAX_TABLE_CELLS and MSG_MAX_TABLE_COLUMNS are applied per table; rule: one lazy item
drawn by     MsgBlockView with the range of the line and the flags first and last; code lines are wrapped at the viewer width, without horizontal scroll
decoration   first line: gap, list marker, code header; every line: paddings, quote bars, code background
values       2.3; geometry and colours of bubbles
```

## 3. Kotlin

```
K/model/ChatModel.kt, part 1
  Format (:5161), new classes    Strong; Block; TableRow; TableCell; Syntax; JSON tags of "Types" of the block Markdown plan
  Format.Block                   block: BlockFormat
                                 quoteDepth: Int
                                 listDepth: Int
  BlockFormat, sealed class      Paragraph
                                 Heading: level: Int
                                 Code: language: String? = null
                                 ListItem: number: String? = null
                                 Table: columns: List<TableAlign>
                                 Rule
  TableAlign, enum               none; left; center; right
  Format.isMarker                true for Block, TableRow, TableCell, Syntax
  Format.style (:5189)           Strong: as Bold; marker: SpanStyle()
  unknown tag                    the item is an invalid item, as now (K/model/SimpleXAPI.kt:6710-6718)
K/views/chat/item/TextItemView.kt
  MarkdownText (:125), part 1    Strong: style of Bold; marker: its text is appended
  MarkdownText, part 2           range: IntRange? = null: indices of formattedText that are drawn; the loop index (:229) is the index in formattedText
                                 markerText: Boolean = true; false: markers are skipped
                                 direction (:153) of an empty text: LocalLayoutDirection.current
  SelectableText (:499)          made internal: code text
  itemSegmentDisplayText (:88)   marker: ""; used by desktop selection
                                 the cost of a teaser is counted by the displayText argument of msgTeaser (read-more plan)
K/views/chat/item/FramedItemView.kt, CIMarkdownText (:445)
  segments                       ci.formattedText without the chat link; with a teaser (read-more plan): the teaser, then its suffix
                                 in a list with markers the suffix is appended by the rule of "Rendering contract" of the block Markdown plan
  selectable                     number of segments before the suffix
  block view                     for a list with a marker: MsgBlocksView in the Box of :462
                                 blocks: remember(segments, ci.mentions, linkMode, prefix) { msgBlocks(…) }
  otherwise                      MarkdownText as now, with the three values of selection.part
K/views/usersettings/MarkdownHelpView.kt, rows after :37
  **bold**                       MdFormat with Format.Strong
  heading                        syntax text and an example in the style of 2.2; the same for a list item, a quote and a code fence
moko-resources/base/strings.xml
  md_heading                     "heading"
  md_list_item                   "item"
  md_quote                       "quote"
  md_code                        "code"
  reused                         copy_verb; copied; bold_text; icon ic_content_copy

K/views/chat/item/MsgBlocks.kt, new file
class MsgBlock
  format: BlockFormat
  quoteDepth: Int
  listDepth: Int
  range: IntRange                 segments of the block text
  rows: List<List<MsgCell>>       table
  asWritten: Boolean              the range is drawn with marker text (2.2, limits)
  start: Int                      offset of the block text in the display text of the item
  length: Int
class MsgCell                     range, start, length: as in MsgBlock

msgBlocks(segments, prefixLength, displayLength): List<MsgBlock>   2.2; pure
MsgBlocksView    Column of MsgBlockView with gaps; the added meta line: MarkdownText("", meta = meta)
MsgBlockView     one block with paddings and quote bars (Modifier.drawBehind), in its block direction; range, first, last: 2.7
CodeBlockView, TableBlockView
parameters       segments; blocks or block; selectable; selection: ItemSelection; quote colour; chatLayout (flat layout plan); openModal (read-more plan)
                 the parameters of the MarkdownText call of CIMarkdownText (FramedItemView.kt:463-475)

text         last text of the item: MarkdownText(block text, segments, range = block.range, markerText = false, meta = meta), Modifier.fillMaxWidth(); stripLink: null
             every other text: ClickableText or SelectableText (TextItemView.kt:427, :499) with the annotated string and the tap handlers of the formatted-text branch of MarkdownText (:219-421), moved to a function used by both; the typing state (:170-198) is omitted
             passed from the item: mentions, userMemberId, sendCommandMsg, uriHandler, linkMode, onLinkLongClick, chatLayout, openModal; prefix: for the first block only; heading: fontSize and lineHeight x scale, FontWeight.Bold
list item    Row { Text(marker); text }
code         Column(fillMaxWidth, background, clip)
               Row { Text(language); IconButton(ic_content_copy) }
               Box(horizontalScroll(rememberScrollState(), enabled while ScrollState.maxValue is above 0))
                 SelectableText(block text): FontFamily.Monospace, TextDirection.Ltr, Modifier.widthIn(max = CODE_MAX_WIDTH)
             copy: clipboard.setText(block text); showToast(copied)
table        Box(horizontalScroll, as code) { Layout }
               column width: largest maxIntrinsicWidth of its cells, with cell padding, within 40.dp .. 240.dp
               cell: text with range = cell.range; measured once with its column width; placed by TableAlign
               row height: tallest cell; drawBehind: background of row 1, grid lines
keys         key(position in the list, block type) for each MsgBlockView
```

- Block text: the texts of the segments of the range other than markers, joined. Block width in a bubble: `fillMaxWidth()` inside the column of `IntrinsicSize.Max` (`FramedItemView.kt:250`), as `headerModifier` (`:125`).

## 4. iOS

```
apps/ios/SimpleXChat/ChatTypes.swift, part 1
  Format (:5643), new cases     strong; block(block: BlockFormat, quoteDepth: Int, listDepth: Int); tableRow; tableCell; syntax
  BlockFormat, TableAlign       new enums: Decodable, Equatable, Hashable; cases and fields of section 3
  Format.isMarker               as in section 3
  unknown tag                   the item is an invalid item, as now (apps/ios/SimpleXChat/API.swift:280-288)
I/ChatItem/MsgContentView.swift
  messageText (:396), part 1    case .strong: the font of .bold (:451); marker: its text is appended
  messageText, part 2           range: Range<Int>? = nil and markerText: Bool = true, as in Kotlin
                                fontScale: CGFloat = 1 and bold: Bool = false, applied to descr and font (:413-414)
  MsgContentView                properties with the same four names, passed at :100
  handleTextTaps (:154-242)     used per text view, with the attributed string and size of that view; smallFont: true for fontScale above 1 (:170)
I/ChatItem/FramedItemView.swift, ciMsgContentView (:359)
  list with a marker            MsgBlocksView in place of MsgContentView, with the modifiers of :375-378 and :380
                                the chat link is removed first; stripLink is nil for every text; the suffix of a teaser is appended as in section 3
I/ChatView.swift, chatItemWithMenu (:2121)
  coordinate space              .coordinateSpace(name: "chatItem") on the VStack of :2131, after .simultaneousGesture (:2161)
  scrollFrames                  @State [CGRect]; set in .onPreferenceChange(ItemScrollFramesKey.self)
  DragGesture (:2162)           (minimumDistance: 10, coordinateSpace: .named("chatItem"))
                                at the first onChanged of a drag: ignored when value.startLocation is inside a frame of scrollFrames; reset in onEnded
I/EndlessScrollView.swift       gestureRecognizer(_:shouldRecognizeSimultaneouslyWith:) (:658): 2.6
apps/ios/Shared/Views/UserSettings/MarkdownHelp.swift, after :21: the rows of section 3; syntax text by Text(verbatim:); new string keys for the examples

I/ChatItem/MsgBlocksView.swift, new file, added to the SimpleX target in project.pbxproj
struct MsgBlock              the fields of the Kotlin class, less start and length; ranges are Range<Int>
msgBlocks(_ segments: [FormattedText], prefix: Bool) -> [MsgBlock]   2.2
struct MsgBlocksView         VStack(alignment: .leading, spacing: 0) of MsgBlockView, ForEach by block position; the added meta line
struct MsgBlockView          one block with its paddings and quote bars (.overlay), in the item direction; used by MsgBlocksView and by the viewer
struct MsgCodeBlockView, MsgTableView
struct MsgTableCell          messageText and msgTextResultView with its own showSecrets, as TextBubble (I/ChatItemInfoView.swift:308-322)
                             properties: segments, range, mentions, userMemberId, commands, sendCommand, openModal, chatLayout, fontScale, bold
struct ItemScrollFramesKey   PreferenceKey of [CGRect] in the coordinate space "chatItem"

text           the text with the meta: MsgContentView(chat:text:formattedText: segments, range:, markerText: false), parameters of FramedItemView.swift:363-374
               every other text of the item: MsgTableCell; prefix: for the first block only; heading: fontScale and bold
list item      HStack(alignment: .firstTextBaseline, spacing: 0) { Text(marker); text }
code           VStack: HStack { Text(language); Button with the image "doc.on.doc" }
               ScrollView(.horizontal) { Text(code) }, layoutDirection .leftToRight, .multilineTextAlignment(.leading)
               text width: NSAttributedString.boundingRect with the width limit CODE_MAX_WIDTH, rounded up, plus 1; .frame(width:)
               text height: .fixedSize(horizontal: false, vertical: true); copy: UIPasteboard.general.string; the image is "checkmark" for 1 s
table          ScrollView(.horizontal) { VStack(spacing: 0) { HStack(spacing: 0) { MsgTableCell.frame(width:alignment:) } } }
               column width: widest cell text by boundingRect with the width limit 240, rounded up, with cell padding; at least 40
added line     MsgContentView(chat:text: "", meta: meta)
block width    code and table: width of the content, at most the available width; flat: .frame(maxWidth: .infinity, alignment: .leading)
               bubbles: DetermineWidth on each text and on the content of code and tables; then code, table and rule
               are at least msgWidth - 24 wide, as headerFrame (FramedItemView.swift:249-250)
scroll frame   code and table: the frame in "chatItem" is set in ItemScrollFramesKey while the content is wider than the scroll view
```

## 5. Desktop selection

```
K/views/chat/TextSelection.kt; the selection is created on desktop only (ChatView.kt:166)
display text of an item   item prefix, then the display text of each segment; the display text of a marker is empty
part                      one text layout of a block view; its range in the display text: start until start + length
outside the selection     list markers, code headers, rules, added meta line, suffix of a teaser, table drawn as written (2.2)

class SelectionPart
  start: Int
  length: Int?                   null: to the end of the item
  bounds: Rect                   in the window
  layout: TextLayoutResult
class ItemSelection (:428)
  range: IntRange?               selected range of the item, in place of highlightRange
  part(key, start, length)       composable; key: block position and cell index; default: one part from 0 to the end of the item
                                 the part is registered while it is composed; a part of length 0 is skipped
                                 result: highlight range within the part, less start; position modifier; layout callback

setupItemSelection (:437)
  offsetAt(x, y):                               -- window coordinates
    p = part whose bounds contain (x, y)
        otherwise the part nearest to y, then nearest to x
    o = p.layout.getOffsetForPosition(Offset(x - p.bounds.left, y - p.bounds.top))
    return p.start + min(o, p.length)
  anchor (:449-452)   offsetAt(anchorWindowX, anchorWindowY)
  focus (:464-477)    offsetAt(focusWindowX, focusWindowY); the character box of p.layout, moved by p.bounds
```

- Highlight: drawn per text by `selectionHighlight` (`TextItemView.kt:523`), clamped to the selectable end of the text (`:353`). Scrolled code and tables: `bounds` are the window bounds of the text inside the scroll area; the highlight is clipped by the scroll area.
- `selectedItemCopiedText` (`:264-292`): the text of a marker at the display offset p is appended when the characters at p - 1 and at p are selected. For a whole item (`0 until Int.MAX_VALUE`, `:256`) the result is the text of all its segments, markers included. Every other function of `TextSelection.kt` is unchanged by this plan. Live items are outside the selection (`:441`).

## 6. Call sites

```
one text: segment texts appended, markers as written   Kotlin                      iOS
chat list preview                                      ChatPreviewView.kt:277      ChatPreviewView.swift:302
chat list draft                                        plain text (:232-235)       ChatPreviewView.swift:280
quote in an item                                       FramedItemView.kt:61        FramedItemView.swift:337
compose context item                                   ContextItemView.kt:59       ContextItemView.swift:74
item info: versions, quote                             ChatItemInfoView.kt:57      ChatItemInfoView.swift:319
profile and group description, teaser                  ChatInfoView.kt:794-816     MsgContentView.swift:285, GroupChatInfoView.swift:338
profile description, full                              TextItemView.kt:56          MsgContentView.swift:320
welcome message preview                                WelcomeMessageView.kt:149   GroupWelcomeView.swift:62
notification: hideSecrets                              NtfManager.kt:134-142       Notifications.swift:200-210
compose: sanitizeMessage                               ComposeView.kt:1023-1054    ComposeView.swift:1984-2011
compose: selectedMarkdown                              GroupMentions.kt:291-310    GroupMentions.swift:181-193
```

- The last three rows join segment texts in order, markers included; they are unchanged. Block view: `CIMarkdownText` (`FramedItemView.kt:445`), `ciMsgContentView` (`FramedItemView.swift:359`), and the viewer.
- Chat list preview, compose context item and notification text: the list is cut by `msgPrefix` (read-more plan) before it is drawn or joined.

## 7. Docs and compatibility

```
apps/multiplatform/spec/client/chat-view.md:180, :319   MsgBlocks.kt; block view of a text item
apps/multiplatform/product/views/chat.md:57             Text row: blocks
apps/ios/spec/client/chat-view.md:39, :188              MsgBlocksView
apps/ios/product/views/chat.md:52                       Text row: blocks
compatibility                                           a segment list without markers, from a host with an earlier core, is drawn as now
```

## 8. Tests

```
apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/MsgBlocksTest.kt
decoding                   each Format and block tag of "Types" of the block Markdown plan, with and without language and number
msgBlocks                  each block type; a list in a quote; code in a list item; open code fence; row with fewer cells; Syntax only; prefix before a heading
                           teaser with suffix; teaser of a list of MSG_MAX_BLOCKS + 1 blocks: MSG_MAX_BLOCKS blocks; TableRow outside a table; cell beyond columns; table of 601 cells; table of 33 columns
start, length              equal to the offsets in the joined display text, for blocks and cells
selectedItemCopiedText     made internal; the item of 2.1 selected whole: its source text; the last code line alone: without the fence; a range over an empty cell
stripFormattedTextLink     a link after a code block; a link in its own paragraph; a text that ends with the link twice
```

Checks, Android, desktop and iOS:

1. The item of 2.1, sent and received, in both layouts: as the sketches. Three quote levels; six list levels; code in a list item; a list and a quote in right-to-left text in a left-to-right app; right-to-left app. A text of `\# x`: drawn without the backslash. A text without markers: drawn as before.
2. Link, mention, secret and command in a heading, in a wrapped list item and in a table cell, also in a scrolled table: actions as in a paragraph; long press: item menu.
3. Code and a table wider than the block: scrolled inside, and the chat stays in place; the chat is scrolled vertically from the same area; reply swipe from a block that fits; desktop: Shift + wheel. Scroll offset of a code block: kept during live updates; Kotlin: kept after the item is scrolled out of the list and back.
4. Copy button of code; desktop selection inside a paragraph, across blocks, inside scrolled code, across table cells and across items.
5. Last block of each type: place of the meta; live item with an open code fence; "Read more" after an open code block: tap and long press.
6. Texts of 15,602 and of 124,816 bytes of `- a` lines, of code fences, of one table row, final and live, also in a bubbles chat with a search text: the limits of 2.2 are applied; frame times are recorded. A code block of one line of 124,816 bytes is drawn as a teaser.
7. Viewer: a paragraph and a code block of 124,816 bytes as lazy items per line, with code lines wrapped; a table wider than the viewer is scrolled. Code with tabs, emoji and CJK text: the same indentation and full last lines on the three platforms.
8. Report with a prefix; chat link item with text; every place of section 6; compose field with blocks: mention picker, link sanitising, edit of a sent item.

## Interfaces

```
app types   Format.Strong, Block, TableRow, TableCell, Syntax; BlockFormat; TableAlign; Format.isMarker
Kotlin      MarkdownText(range, markerText); MsgBlock; MsgCell; msgBlocks; SelectionPart; ItemSelection.part; MsgBlocksView; MsgBlockView; CodeBlockView; TableBlockView
iOS         messageText(range:markerText:fontScale:bold:); MsgBlock; msgBlocks; ItemScrollFramesKey; MsgBlocksView; MsgBlockView; MsgCodeBlockView; MsgTableView; MsgTableCell
constants   MSG_MAX_TABLE_CELLS 600; CODE_MAX_WIDTH 2000; MSG_MAX_BLOCKS 500, equal to MSG_FLAT_MAX_LINES; MSG_MAX_TABLE_COLUMNS 32
strings     md_heading; md_list_item; md_quote; md_code; files: MsgBlocks.kt; MsgBlocksView.swift; MsgBlocksTest.kt
```

- Combined edits of the shared functions: "Shared code" of the read-more plan, column "rendering plan". Order: part 1, flat layout plan, read-more plan, part 2; part 1 is in release 1 and part 2 in release 2 (section 1). Flat layout plan: the maximum block width is its content width (2.4); the content ends with the meta reserve (2.4 of this plan). Block Markdown plan: sections 2.2, 2.4, 5 and 6 are the app side of its "Rendering contract".

## Depends on

```
block Markdown plan   "Types": constructors, fields, JSON tags; maxQuoteDepth 3, maxListDepth 6, maxSegments 8192
                      "Rendering contract": rows and cells of a table; segment index; appended segments; live message; the Strong change of the parser (strongP) in release 1
flat layout plan      LocalChatLayout, EnvironmentValues.chatLayout; MarkdownText(chatLayout), messageText(chatLayout:); content width (2.4); paddings of CIMarkdownText and ciMsgContentView
read-more plan        msgTeaser, itemTeaser, msgLines, msgPrefix; suffix and openModal; ItemContext.maxLines, ItemContext.teaserLines; SelectionManager.itemSegments; showFullMessage, FullMessageView: the viewer and its chatLayout value
                      msgTeaser: argument maxBlocks, passed MSG_MAX_BLOCKS by itemTeaser; when scrolling is chosen (open questions 2, 7): cost of a code line: max(1, ceil(width / MSG_CODE_LINE_CHARS)), MSG_CODE_LINE_CHARS = 240; cost of a table row: the largest cost of its cells, each max(1, ceil(width / 30))
agent profile plan    ChatInfo.chatLayout
message parts plan    CIMeta.partsMissing in the meta reserve; item text of up to 124,816 bytes
```

## Open questions

1. Block views in chat rows and in the viewer only; in the chat list preview, quotes, item info, descriptions and welcome messages block syntax is shown as written. Recommended: keep in this release.
2. Code lines and wide tables are scrolled horizontally inside the block; a table above 600 cells is drawn as source text; desktop scrollbar later. `MSG_MAX_BLOCKS` is 500, equal to `MSG_FLAT_MAX_LINES`; the teaser is cut at `MSG_MAX_BLOCKS` blocks, also for a lower value. Recommended: yes.
3. Text copied from a desktop selection is source text, block syntax included. Recommended: yes.
4. `*x*` is kept as `Bold` (bold in bubbles, italic in flat rows) and `**x**` is `Strong` (bold in both). Surfaces of a flat chat with italic `*x*`: rows only, or rows and the viewer. Recommended: rows and the viewer; bold in previews, compose context and item info.
5. Stored and new texts with a marker at a line start (`# x`, `- a`, `> x`, `8) x`, `---`) are drawn as blocks after the upgrade. Recommended: accept, with the `\` escape in the same release.
6. "Read more" in the flat layout: a flat row above 500 lines (16,000 characters on a phone) is a teaser, against "CLI layout does not need read more". Recommended: accept; smaller texts are drawn in full.
7. Design without horizontal scroll, in place of question 2: code lines wrapped, table columns fitted to the block width, a wider table drawn as written; the scroll state, `ItemScrollFramesKey` and the gesture changes of 2.6 and section 4 are removed. With scrolling, the cost of a code line and of a table row in `msgTeaser` is counted by their own widths ("Depends on"). Recommended: scrolling.
