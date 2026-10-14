# Block markdown: rendering in the apps

Date: 2026-10-13. Revised: 2026-10-14.

The Android, desktop and iOS apps decode the blocks of `formattedText` (`2026-10-13-block-markdown.md`) and draw the text of a chat item as a column of block views.

Paths: `K/` is `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`; `I/` is `apps/ios/Shared/Views/Chat/`. Layouts: `ChatLayout.Classic` and `ChatLayout.Terminal` (Kotlin `ChatModel.kt:2204-2205`, iOS `ChatTypes.swift:258-261`). Layout plan: `2026-10-13-flat-chat-layout-ui.md`. Read-more plan: `2026-10-13-message-read-more.md`.

## 1. Scope

```
block view   text of a chat item in a chat view: main chat view in both layouts, member support chat, reports view
             viewer of the read-more plan (2.7)
one text     every other place where the text of an item, a quote or an item version is drawn (section 6)
             the one-text form of the block list ("Rendering contract" of the block Markdown plan)
source form  chatParseMarkdown, as now: compose field, profile descriptions, welcome messages, link fields (section 6)
part 1       release 1, with the layout plan and Strong of the core: Format.Strong; Format.style; MarkdownText; messageText; MarkdownHelp
             MarkdownBlock and TableAlign app types, without JSON use; markdownSegments (read-more plan)
part 2       release 2, with the blocks of the core: every other change of this plan; applied after the layout and read-more plans
```

- Block view: drawn for a block list with a block other than Paragraph. A list of Paragraphs only, a teaser with the paragraph of the "Read more" link included, is drawn as one text: `markdownSegments` of the list, by `MarkdownText` (`K/views/chat/item/TextItemView.kt:125`) and `MsgContentView` (`I/ChatItem/MsgContentView.swift:28`), as now.

## 2. Specification

### 2.1. Sketches

```
classic
+------------------------------------------+
| Build steps                              |   heading, level 2
| | bash                          [copy] | |   code: language, copy button
| | ./build.sh --target android --releas>| |   one line per source line; scrolled inside the block
| | Target | Time  | Size  |               |   table: grid; row 1 bold, with background
| | arm64  | 4 min | 38 MB |               |   wider than the bubble: scrolled inside the block
|                                    10:42 |   meta on its own line: the last block is a table
+------------------------------------------+

terminal
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
block          view                    drawing
paragraph      one text                as now
heading        one text                bold; body font size for levels 4 to 6
                                       levels 1, 2, 3: font size x 1.375, 1.25, 1.125
quote          one text                start padding 12; one bar 3 wide, secondary colour, over the block height
list item      row: marker, one text   marker: "•" for a bullet, number as written; end-aligned in a column of 22, scaled with the font
                                       the column of an item is widened for a wider marker
                                       start padding: indent x 22
code           column: header, text    header: language in one line with ellipsis; copy button at the end
                                       text: monospace, font size x 0.875, one line per source line, left-to-right
                                       a line above CODE_MAX_WIDTH (2000) is wrapped
                                       background: quote colour; corners 8 x roundness
table          grid of cell texts      column width: widest cell, within 40 .. 240; cell text is wrapped
                                       column alignment: columns; none: start
                                       row 1: bold, quote colour; grid lines 1 wide
rule           line                    1 high, secondary colour, block width
unknownBlock   none; no gap before it

gap before     list item after a list item: 2; heading: 12; other block: 8; first block: 0; sizes: dp in Kotlin, points in iOS
limits         MSG_MAX_BLOCKS = MSG_TERMINAL_MAX_LINES (500) blocks in one item: the teaser is cut before block MSG_MAX_BLOCKS + 1 (msgTeaser, argument maxBlocks; read-more plan)
               MSG_MAX_TABLE_CELLS = 600 cells in one item; MSG_MAX_TABLE_COLUMNS = 32
               a table above a limit is drawn as a code block without its header: the one-text form of the table
               release condition for this value of MSG_MAX_BLOCKS: a measurement of a live item on a mid-range Android phone and an iPhone
```

- `prefix`: the report reason (`itemPrefixText`, `TextItemView.kt:118-121`; `reason.attrString`, `I/ChatItem/FramedItemView.swift:196`). It is drawn at the start of the first block when it is a paragraph; otherwise in a paragraph before the first block.
- Chat link item: the link is removed from the blocks first by `stripBlocksLink(blocks, link)`: the last segment of the content of the last block is removed when its text is the link, with a line break at the end of the segment before it; a block with empty content is removed.

### 2.3. Layouts

```
                classic                                               terminal                   viewer
block width     text width of the bubble                              content width of the row   viewer width
bubble width    width of the widest block, within the present limit   -                          -
quote colour    sentQuote or receivedQuote, by direction              receivedQuote              receivedQuote
```

- Quote colour: Kotlin `MaterialTheme.appColors` (`K/views/chat/item/FramedItemView.kt:121-122`), iOS `chatItemFrameContextColor` (`FramedItemView.swift:443`).
- Item limits: layout plan and read-more plan; the teaser is cut from the block list before it is drawn. `Format.Bold`: the rule of the layout plan; the `chatLayout` parameter is passed to every text of a block view. In the viewer its value is passed by `showFullMessage`.

### 2.4. Meta line, typing indicator, direction

```
last block: paragraph, heading, quote, list item   typing indicator and meta reserve at the end of its text, as now (TextItemView.kt:344-351, MsgContentView.swift:115-120)
last block: code, table, rule                      one text line after it, with the typing indicator and the meta reserve
meta reserve                                       reserveSpaceForMeta (K/views/chat/item/CIMetaView.kt:130), as now
                                                   with the marks of the message parts plan and of the message priority plan
meta view                                          bottom-end corner, as now (FramedItemView.kt:431-439, FramedItemView.swift:114-125)
block direction                                    Kotlin: of the block text, its first 50 characters, as now (TextItemView.kt:153-155)
                                                   iOS: of the item, isRightToLeft of the item text, as now (FramedItemView.swift:361)
                                                   code: left-to-right; the marker column, the paddings and the bars are placed by the block direction
```

### 2.5. Inline formats, taps, copy

- Content of a paragraph, heading, quote, list item and table cell: drawn by `MarkdownText` (`TextItemView.kt:125-424`) and `messageText` (`MsgContentView.swift:396-585`). `Strong` is bold. Code is text.
- Taps, long presses and hover: handled per text, as now. Kotlin: `ClickableText` (`:427-496`). iOS: `msgTextResultView` (`:139-150`).
- Shown secrets: state per text, as now (`TextItemView.kt:172`, `MsgContentView.swift:43`). `mentions`, `userMemberId` and the command action of the item are passed to every text of the item.
- Item menu and its Copy action: unchanged, `ci.content.text` (`K/views/chat/item/ChatItemView.kt:445-448`, `I/ChatView.swift:2494-2506`). Copy button of a code block: its `code`. Desktop selection: section 5.

### 2.6. Horizontal scroll, live items, cost

```
scroll enabled     while the content is wider than the block
Android            CodeBlockView, TableBlockView: Modifier.horizontalScroll, inside SwipeToDismissModifier of the row (K/views/helpers/Modifiers.kt:28)
                   the block is scrolled by a horizontal drag that the user begins inside an enabled scroll area; the user begins the reply swipe outside such areas
                   touch slop: 50 px, the value of the row (K/views/chat/ChatView.kt:1939, :3665)
iOS                chatItemWithMenu (section 4): the reply drag is ignored when the user begins it inside an enabled scroll area
                   EndlessScrollView.gestureRecognizer(_:shouldRecognizeSimultaneouslyWith:) (I/EndlessScrollView.swift:658): false when the view of the other recogniser is a UIScrollView inside a cell
desktop            Modifier.horizontalScroll: Shift + wheel, touchpad
item update        Kotlin: the blocks are composed again; a text is laid out again when its content is changed
                   iOS: the cell content is set and measured again (EndlessScrollView.swift:64-81), as now
typing indicator   updated every 250 ms in the last text of the item only (TextItemView.kt:178-183, MsgContentView.swift:84-86)
open code fence    code block to the end of the text; typing indicator on the line after it
kept               by block position and kind: scroll offset (rememberScrollState; ScrollView), shown secrets (2.5)
views per row      at most MSG_MAX_BLOCKS blocks and MSG_MAX_TABLE_CELLS cells; one row of the lazy list per item, as now
```

### 2.7. Viewer

```
items        msgLines (read-more plan): paragraph, heading, quote, list item and code are divided at line breaks; a table and a rule are one item each
             a line above 15,000 units is cut by the rule of the read-more plan
             MSG_MAX_TABLE_CELLS and MSG_MAX_TABLE_COLUMNS are applied per table
drawn by     MsgBlockView(block, range, first, last); range: the range of the block text in one viewer item (MsgLine); code lines are wrapped at the viewer width, without horizontal scroll
decoration   first line: gap, list marker, code header; every line: paddings, quote bar, code background
values       2.3; geometry and colours of the classic layout
```

## 3. Kotlin

```
K/model/ChatModel.kt
  Format (:5201), part 1                Strong: @SerialName("strong")
  Format.style (:5229), part 1          Strong: as Bold
  MarkdownBlock, new, part 1            sealed class; @Serializable(with = MarkdownBlockSerializer::class); the JSON tags of the block Markdown plan
                                        Paragraph(content: List<FormattedText>)
                                        Heading(level: Int, content: List<FormattedText>)
                                        CodeBlock(language: String? = null, code: String)
                                        Quote(content: List<FormattedText>)
                                        ListItem(number: String? = null, indent: Int, content: List<FormattedText>)
                                        Table(columns: List<TableAlign>, rows: List<List<List<FormattedText>>>)
                                        Rule
                                        UnknownBlock
  TableAlign, new, part 1               enum: none; left; center; right
  formattedText, part 2                 List<MarkdownBlock>? in ChatItem (:3433), CIQuote (:4439), ChatItemVersion (:5740)
  markdownSegments(blocks), new, part 1 one-text form; List<FormattedText>? for List<MarkdownBlock>?
  unknown Format tag                    the item is an invalid item, as now (K/model/SimpleXAPI.kt:6715-6728)
  unknown block tag                     UnknownBlock, by a custom serializer of MarkdownBlock, as MsgReactionSerializer (:4497-4519)
K/views/chat/item/TextItemView.kt
  MarkdownText (:125), part 1           Strong: the style of Bold (:232)
  itemDisplayText (:110-114)            removed: unused
  SelectableText (:499)                 made internal: code text
  stripBlocksLink, new                  2.2; next to stripFormattedTextLink (:622-632), which is unchanged
K/views/chat/item/FramedItemView.kt
  ciQuotedMsgTextView (:60-74)          markdownSegments(qi.formattedText)
  CIMarkdownText (:445-477)             blocks: remember(ci.formattedText, stripLink) { stripBlocksLink }; an empty list for an empty text, as now (:464)
                                        a list with a block other than Paragraph: MsgBlocksView in the Box of :462
                                        Paragraphs only: MarkdownText with markdownSegments of the list, as now
                                        with a teaser (read-more plan): the teaser blocks, then the paragraph of the "Read more" link
K/views/usersettings/MarkdownHelpView.kt, rows after :37
  **bold**, part 1                      MdFormat with Format.Strong
  heading                               syntax text and an example in the style of 2.2; the same for a list item, a quote and a code fence
moko-resources/base/strings.xml
  md_heading                            "heading"
  md_list_item                          "item"
  md_quote                              "quote"
  md_code                               "code"
  reused                                copy_verb; copied; bold_text; icon ic_content_copy

K/views/chat/item/MsgBlocks.kt, new file
blockRanges(blocks, prefixLength, displayText): List<List<IntRange>>   pure; for each block, the ranges of its texts in the display text of the item
                                                                       one range for a paragraph, heading, quote, list item content and code; one per cell for a table; an empty list for a rule
MsgBlocksView     Column of MsgBlockView with the gaps of 2.2; the added meta line: MarkdownText("", meta = meta)
MsgBlockView      one block with its paddings and quote bar (Modifier.drawBehind), in its block direction; block, range, first, last: 2.7
CodeBlockView, TableBlockView
parameters        blocks or block; selection: ItemSelection; quote colour; chatLayout (layout plan); openModal (read-more plan)
                  the parameters of the MarkdownText call of CIMarkdownText (FramedItemView.kt:463-475)

text         last text of the item: MarkdownText(text of the content, content, meta = meta), Modifier.fillMaxWidth(); stripLink: null
             every other text: MarkdownText(text of the content, content) without meta
             passed from the item: mentions, userMemberId, sendCommandMsg, uriHandler, linkMode, onLinkLongClick, chatLayout, openModal
             prefix: first block only; heading: style with fontSize and lineHeight x scale, FontWeight.Bold
list item    Row { Text(marker); text }
quote        Box(padding start 12, drawBehind: bar) { text }
code         Column(fillMaxWidth, background, clip)
               Row { Text(language); IconButton(ic_content_copy) }
               Box(horizontalScroll(rememberScrollState(), enabled while ScrollState.maxValue is above 0))
                 SelectableText(code): FontFamily.Monospace, TextDirection.Ltr, Modifier.widthIn(max = CODE_MAX_WIDTH)
             copy: clipboard.setText(code); showToast(copied)
table        Box(horizontalScroll, as code) { Layout }
               column width: largest maxIntrinsicWidth of its cells, with cell padding, within 40.dp .. 240.dp
               cell: MarkdownText of the cell; measured once with its column width; placed by TableAlign
               row height: tallest cell; drawBehind: background of row 1, grid lines
keys         key(position in the list, block kind) for each MsgBlockView
```

- Text of a content: the texts of its segments, joined. Block width in a bubble: `fillMaxWidth()` inside the column of `IntrinsicSize.Max` (`FramedItemView.kt:250`), as `headerModifier` (`:125`).

## 4. iOS

```
apps/ios/SimpleXChat/ChatTypes.swift
  Format (:5693), part 1                case strong
  MarkdownBlock, new, part 1            enum: Decodable, Hashable; cases and fields of section 3
  TableAlign, new, part 1               enum: String, Decodable, Hashable
  formattedText, part 2                 [MarkdownBlock]? in ChatItem (:3813, :3827), CIQuote (:4873), ChatItemVersion (:6412)
  markdownSegments(_ blocks:), part 1   new; one-text form; in SimpleXChat for Notifications.swift and the NSE
  unknown Format tag                    the item is an invalid item, as now (apps/ios/SimpleXChat/API.swift:280-288)
  unknown block tag                     .unknownBlock, by init(from:) of MarkdownBlock with a default case
I/ChatItem/MsgContentView.swift
  messageText (:396), part 1            case .strong: the font of .bold (:450-452)
  messageText, part 2                   fontScale: CGFloat = 1 and bold: Bool = false, applied to descr and font (:413-414)
  MsgContentView                        properties fontScale and bold, passed at :100
  handleTextTaps (:154-242)             used per text view, with the attributed string and size of that view; smallFont: true for fontScale above 1 (:170)
  stripBlocksLink, new                  2.2; next to stripFormattedTextLink (:608-616), which is unchanged
I/ChatItem/FramedItemView.swift
  ciQuotedMsgTextView (:336-340)        markdownSegments(qi.formattedText)
  ciMsgContentView (:359-387)           a list with a block other than Paragraph: MsgBlocksView in place of MsgContentView, with the modifiers of :375-379 and :382-386
                                        the chat link is removed by stripBlocksLink; with a teaser: the teaser blocks, then the paragraph of the "Read more" link
                                        Paragraphs only: MsgContentView with markdownSegments of the list, as now
I/ChatView.swift, chatItemWithMenu (:2121)
  coordinate space                      .coordinateSpace(name: "chatItem") on the VStack of :2131, after .simultaneousGesture (:2161)
  scrollFrames                          @State [CGRect]; set in .onPreferenceChange(ItemScrollFramesKey.self)
  DragGesture (:2162)                   (minimumDistance: 10, coordinateSpace: .named("chatItem"))
                                        at the first onChanged of a drag: ignored when value.startLocation is inside a frame of scrollFrames; reset in onEnded
I/EndlessScrollView.swift               gestureRecognizer(_:shouldRecognizeSimultaneouslyWith:) (:658): 2.6
apps/ios/Shared/Views/UserSettings/MarkdownHelp.swift, after :21: the rows of section 3; syntax text by Text(verbatim:); new string keys for the examples

I/ChatItem/MsgBlocksView.swift, new file, added to the SimpleX target in project.pbxproj
struct MsgBlocksView         VStack(alignment: .leading, spacing: 0) of MsgBlockView, ForEach by block position; the added meta line
struct MsgBlockView          one block with its paddings and quote bar (.overlay), in the item direction; used by MsgBlocksView and by the viewer
struct MsgCodeBlockView, MsgTableView
struct MsgTableCell          messageText and msgTextResultView with its own showSecrets, as TextBubble (I/ChatItemInfoView.swift:308-322)
                             properties: content, mentions, userMemberId, commands, sendCommand, openModal, chatLayout, fontScale, bold
struct ItemScrollFramesKey   PreferenceKey of [CGRect] in the coordinate space "chatItem"

text           the text with the meta: MsgContentView(chat:text:formattedText: content), parameters of FramedItemView.swift:363-374
               every other text of the item: MsgTableCell; prefix: first block only; heading: fontScale and bold
list item      HStack(alignment: .firstTextBaseline, spacing: 0) { Text(marker); text }
code           VStack: HStack { Text(language); Button with the image "doc.on.doc" }
               ScrollView(.horizontal) { Text(code) }, layoutDirection .leftToRight, .multilineTextAlignment(.leading)
               text width: NSAttributedString.boundingRect with the width limit CODE_MAX_WIDTH, rounded up, plus 1; .frame(width:)
               text height: .fixedSize(horizontal: false, vertical: true); copy: UIPasteboard.general.string; the image is "checkmark" for 1 s
table          ScrollView(.horizontal) { VStack(spacing: 0) { HStack(spacing: 0) { MsgTableCell.frame(width:alignment:) } } }
               column width: widest cell text by boundingRect with the width limit 240, rounded up, with cell padding; at least 40
added line     MsgContentView(chat:text: "", meta: meta)
block width    code and table: width of the content, at most the available width; terminal: .frame(maxWidth: .infinity, alignment: .leading)
               classic: DetermineWidth on each text and on the content of code and tables; then code, table and rule
               are at least msgWidth - 24 wide, as headerFrame (FramedItemView.swift:249-250)
scroll frame   code and table: the frame in "chatItem" is set in ItemScrollFramesKey while the content is wider than the scroll view
```

## 5. Desktop selection

```
K/views/chat/TextSelection.kt; the selection is created on desktop only (ChatView.kt:166)
display text of an item   item prefix, then the display texts (itemSegmentDisplayText, TextItemView.kt:88-108) of markdownSegments of the blocks of the item
part                      one text layout of a block view; its range in the display text: blockRanges (section 3)
outside the parts         line breaks between blocks, list markers, " | " and line breaks between cells, code headers, rules, added meta line, the paragraph of the "Read more" link

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
  anchor (:445-453)   offsetAt(anchorWindowX, anchorWindowY)
  focus (:455-480)    offsetAt(focusWindowX, focusWindowY); the character box of p.layout, moved by p.bounds
```

- Highlight: drawn per text by `selectionHighlight` (`TextItemView.kt:523-532`), clamped to the selectable end of the text (`:353`). Scrolled code and tables: `bounds` are the window bounds of the text inside the scroll area; the highlight is clipped by the scroll area.
- `selectedItemCopiedText` (`:264-292`) and `snapOffset` (`:295-307`): over `markdownSegments` of the blocks of the item, in place of `ci.formattedText`; the blocks are passed by the read-more plan (`SelectionManager.itemBlocks`: the teaser or `ci.formattedText`). A whole item (`0 until Int.MAX_VALUE`, `:256`): the item prefix and `ci.text`, the source text. Every other function of `TextSelection.kt` is unchanged by this plan. Live items are outside the selection (`:441`).

## 6. Call sites

```
one text: markdownSegments              Kotlin                                  iOS
chat list preview                       ChatPreviewView.kt:264                  ChatPreviewView.swift:315
quote in an item                        FramedItemView.kt:63                    FramedItemView.swift:337
compose context item                    ContextItemView.kt:60                   ContextItemView.swift:74
item info: versions, quote              ChatItemInfoView.kt:88, :140            ChatItemInfoView.swift:270, :345
notification: hideSecrets               NtfManager.kt:134-150                   Notifications.swift:200-219

source form: chatParseMarkdown         Kotlin                                  iOS
edit of a sent item                     ComposeView.kt:130                      ComposeView.swift:83
compose: sanitizeMessage                ComposeView.kt:1023, :1057              ComposeView.swift:560, :1984
compose: selectedMarkdown               GroupMentions.kt:171, :291              GroupMentions.swift:181
chat list draft                         text without formats (:232-235)         ChatPreviewView.swift:280
profile and group description           ChatInfoView.kt:787-819                 MsgContentView.swift:343, :380; GroupChatInfoView.swift:338
profile description, full               TextItemView.kt:56-58                   MsgContentView.swift:320
welcome message preview                 WelcomeMessageView.kt:151               GroupWelcomeView.swift:62
```

- Source form rows are unchanged, except the edit of a sent item: `parseToMarkdown(text) ?: FormattedText.plain(text)` (Kotlin) and `parseSimpleXMarkdown(text) ?? FormattedText.plain(text)` (iOS), with the text of the compose field (`ComposeView.kt:124-129`, `ComposeView.swift:77-82`), in place of `editingItem.formattedText`.
- Block view: `CIMarkdownText` (`FramedItemView.kt:445`), `ciMsgContentView` (`FramedItemView.swift:359`), and the viewer.
- Chat list preview, quote in an item (`FramedItemView.kt:63`, `FramedItemView.swift:337`), compose context item and notification text: the one-text form is cut by `msgPrefix` (read-more plan) before it is drawn or joined.

## 7. Docs and compatibility

```
apps/multiplatform/spec/client/chat-view.md:180, :319   MsgBlocks.kt; block view of a text item
apps/multiplatform/product/views/chat.md:57             Text row: blocks
apps/ios/spec/client/chat-view.md:39, :188              MsgBlocksView
apps/ios/product/views/chat.md:52                       Text row: blocks
compatibility                                           app and core are of one release; a desktop with a host of another release: block Markdown plan, "Compatibility"
```

## 8. Tests

```
apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/MsgBlocksTest.kt
decoding                   each block and format tag of "Types" of the block Markdown plan, with and without language and number
markdownSegments           each block kind; equal to the one-text form of the block Markdown plan
blockRanges                equal to the offsets in the joined display text, for blocks and cells
stripBlocksLink            a link after a code block; a link in its own paragraph; a text that ends with the link twice
selectedItemCopiedText     made internal; the item of 2.1 selected whole: its source text; the last code line alone; a range over an empty cell
```

Checks, Android, desktop and iOS:

1. The item of 2.1, sent and received, in both layouts: as the sketches. Six list levels; code in a list item; a quote; a list and a quote in right-to-left text in a left-to-right app; right-to-left app. A text of `\# x`: drawn without the backslash. A text without blocks: drawn as before.
2. Link, mention, secret and command in a heading, in a wrapped list item and in a table cell, also in a scrolled table: actions as in a paragraph; long press: item menu.
3. Code and a table wider than the block: scrolled inside, and the chat is kept in place; the chat is scrolled vertically from the same area; reply swipe from a block that fits; desktop: Shift + wheel. Scroll offset of a code block: kept during live updates; Kotlin: kept after the item is scrolled out of the list and back.
4. Copy button of code; desktop selection inside a paragraph, across blocks, inside scrolled code, across table cells and across items.
5. Last block of each kind: place of the meta; live item with an open code fence; "Read more" after an open code block: tap and long press.
6. Texts of 15,602 and of 124,816 bytes of `- a` lines, of code fences, of one table row, final and live, also in a classic chat with a search text: the limits of 2.2 are applied; frame times are recorded. A code block of one line of 124,816 bytes is drawn as a teaser.
7. Viewer: a paragraph and a code block of 124,816 bytes as lazy items per line, with code lines wrapped; a table wider than the viewer is scrolled. Code with tabs, emoji and CJK text: the same indentation and full last lines on the three platforms.
8. Report with a prefix; chat link item with text; every place of section 6; compose field with blocks: mention picker, link sanitising, edit of a sent item.

## Interfaces

```
app types   Format.Strong; MarkdownBlock; TableAlign; markdownSegments; stripBlocksLink
Kotlin      MsgBlocksView; MsgBlockView; CodeBlockView; TableBlockView; blockRanges; SelectionPart; ItemSelection.part
iOS         messageText(fontScale:bold:); ItemScrollFramesKey; MsgBlocksView; MsgBlockView; MsgCodeBlockView; MsgTableView; MsgTableCell
constants   MSG_MAX_TABLE_CELLS 600; CODE_MAX_WIDTH 2000; MSG_MAX_BLOCKS 500, equal to MSG_TERMINAL_MAX_LINES; MSG_MAX_TABLE_COLUMNS 32
strings     md_heading; md_list_item; md_quote; md_code
files       MsgBlocks.kt; MsgBlocksView.swift; MsgBlocksTest.kt
```

- Combined edits of the shared functions: "Shared code" of the read-more plan, column "rendering plan":

```
MarkdownText (TextItemView.kt:125)           Strong
CIMarkdownText (FramedItemView.kt:445)       block view for a list with a block other than Paragraph; Paragraphs only: one text; teaser blocks with the paragraph of the link
messageText, MsgContentView                  strong; fontScale, bold
ciMsgContentView (FramedItemView.swift:359)  MsgBlocksView
stripFormattedTextLink                       unchanged; stripBlocksLink is new
selectedItemCopiedText, snapOffset           over markdownSegments of the item blocks; a whole item: source text
ItemSelection, setupItemSelection            parts
chatItemWithMenu (ChatView.swift:2121)       coordinate space; drag gesture
```

- Order: part 1, layout plan, read-more plan, part 2; part 1 is in release 1 and part 2 in release 2 (section 1). Layout plan: the maximum block width is its content width; the content ends with the meta reserve (2.4 of this plan). Block Markdown plan: sections 2.2, 2.4, 5 and 6 are the app side of its "Rendering contract".

## Depends on

```
block Markdown plan   "Types": constructors, fields, JSON tags; maxListIndent 6, maxSegments 8192
                      "Rendering contract": blocks, rows and cells of a table, the one-text form, live message; Strong (strongP) in release 1
layout plan           LocalChatLayout, EnvironmentValues.chatLayout; MarkdownText(chatLayout), messageText(chatLayout:); content width; paddings of CIMarkdownText and ciMsgContentView; MSG_TERMINAL_MAX_LINES
read-more plan        msgTeaser over blocks, itemTeaser, msgLines and MsgLine, msgPrefix; the paragraph of the "Read more" link and openModal; the limits of ItemContext; SelectionManager.itemBlocks; showFullMessage, FullMessageView: the viewer and its chatLayout value
                      msgTeaser: argument maxBlocks, passed MSG_MAX_BLOCKS by itemTeaser
agent profile plan    ChatInfo.activeChatLayout, computed by the core (patch 01 rework, patches/ws/layout/work: ChatModel.kt:1872-1876, ChatTypes.swift:2343-2349)
message parts plan    CIMeta.partsMissing in the meta reserve; item text of up to 124,816 bytes
```

## Open questions

1. Block views in chat rows and in the viewer only. In the chat list preview, quotes, compose context, item info and notifications the one-text form is drawn: bullets and cells as text, headings as text. In descriptions and welcome messages the source form is drawn: block syntax as written. Recommended: keep in this release.
2. Code lines and wide tables are scrolled horizontally inside the block; a table above 600 cells or 32 columns is drawn as code; desktop scrollbar later. In place of it: code lines wrapped, table columns fitted to the block width, a wider table drawn as code; the scroll state, `ItemScrollFramesKey` and the gesture edits of 2.6 and section 4 are then removed. Recommended: scrolling.
3. Text copied from a desktop selection: a whole item is its source text; a part of an item is the one-text form of the selected range. Recommended: yes.
