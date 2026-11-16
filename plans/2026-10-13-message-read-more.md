# Read more for long messages

Date: 2026-10-13. Revised: 2026-10-14.

The app shows a long message as a teaser followed by a "Read more" link; after a tap on the link the app shows the whole message in a viewer.

## Scope

Only the Kotlin and iOS apps are changed. References are to branch `ep/llm-ux`, commit `31a8c86fb`.

- Input: `ChatItem.formattedText`, the list of blocks of the core parser (`MarkdownList = [MarkdownBlock]`, block Markdown plan, release 2). Release 1: the segment list of the item is taken as one paragraph.
- "Read more" is the app-only format `Format.Modal`: Kotlin `ChatModel.kt:5220`, iOS `ChatTypes.swift:5707`. Its other use, the description link (`ChatInfoView.kt:812`, `MsgContentView.swift:382`), is unchanged.
- Desktop linked to a mobile: the teaser is computed by the desktop app from the items of the mobile core.

```
agent profile plan    2026-10-13-agent-profile-chat-layout.md, patch 01 rework
layout plan           2026-10-13-flat-chat-layout-ui.md, layouts classic and terminal
block Markdown plan   2026-10-13-block-markdown.md
rendering plan        2026-10-13-block-markdown-rendering.md
message parts plan    2026-10-13-message-parts.md
```

## Items

```
text path                Kotlin CIMarkdownText (FramedItemView.kt:445)
                         iOS FramedItemView.ciMsgContentView (FramedItemView.swift:359)
layout of a chat view    ChatInfo.activeChatLayout, provided by the layout plan: Kotlin LocalChatLayout, iOS EnvironmentValues.chatLayout
texts                    every text drawn by the text path
chat link item           the link is removed first (rendering plan)
report                   the reason prefix (itemPrefixText) is outside the count
live item                truncated above the teaser cost while it is updated
```

Limits:

```
limit         classic            terminal layout, or search text not blank
maxLines      MSG_MAX_LINES      MSG_TERMINAL_MAX_LINES (layout plan)
teaserLines   MSG_TEASER_LINES   MSG_TERMINAL_TEASER_LINES (layout plan)
maxUnits      without limit      MSG_TERMINAL_MAX_LINES × MSG_LINE_CHARS
teaserUnits   without limit      MSG_TERMINAL_TEASER_LINES × MSG_LINE_CHARS
maxBlocks     MSG_MAX_BLOCKS     MSG_MAX_BLOCKS (rendering plan)
```

## Teaser rule

Input: the blocks of the item. A text with `formattedText == null` is taken as one paragraph with one segment without format. Line breaks in content are text of segments without format (block Markdown plan, "Rendering contract").

```
MSG_LINE_CHARS        32 on Android and iOS
                      80 on desktop
MSG_MAX_LINES         24
MSG_TEASER_LINES      16
MSG_CODE_LINE_CHARS   240, the units drawn in CODE_MAX_WIDTH (rendering plan)
MSG_CELL_CHARS        30, the units drawn in a table column of 240
MSG_PREVIEW_UNITS     1000
unit                  UTF-16 code unit
display text          of an inline segment: the displayText argument
                      Kotlin: itemSegmentDisplayText(ft, ci, linkMode) (TextItemView.kt:88), passed by itemTeaser
                      iOS: segmentDisplayText (new), the text drawn by messageText (MsgContentView.swift:446-570)
line break            Kotlin: a match of the regular expression \R; Swift: a Character with isNewline
width                 1 per unit
                      2 per unit in U+2E80-U+A4CF
                      2 per unit in U+AC00-U+D7A3
                      2 per unit in U+F900-U+FAFF
                      2 per unit in U+FF00-U+FF60
line cost             max(1, ceil(width of the line / chars)); chars: MSG_LINE_CHARS unless stated
```

Cost and units of a block:

```
block           cost                                                  units
paragraph       line costs of the display text of its content         display text
heading         as paragraph                                          display text
quote           as paragraph                                          display text
list item       as paragraph                                          display text
code block      1 for the header                                      code text
                line costs of the code lines, chars MSG_CODE_LINE_CHARS
table           per row: the largest line cost of its cells,          cell texts
                chars MSG_CELL_CHARS
rule            1                                                     0
unknown block   0                                                     0
gap             1 before a block
                0 before the first drawn block and before an unknown block
                0 before a list item after a list item
```

```
msgTeaser(blocks, live, maxLines, teaserLines, maxUnits, teaserUnits, maxBlocks):
  limits: maxLines and maxUnits; for a live item: teaserLines and teaserUnits
  cost, units and count of blocks within the limits and maxBlocks: null; the item is drawn in full
  teaser: the blocks in order while cost <= teaserLines, units <= teaserUnits and count <= maxBlocks
          the next block: cut by "Cut inside a block" to the remaining cost and units; left out when the cut is empty
  return the teaser; it can be empty

msgPrefix(segments, maxUnits):
  segments: one text; release 2: markdownSegments(blocks) (block Markdown plan, "Rendering contract")
  units <= maxUnits: the segments
  otherwise: inline cut at p = maxUnits
```

### Cut inside a block

p: the position of the remaining cost and units in the text of the block.

```
paragraph       inline cut at p
heading         inline cut at p
quote           inline cut at p
list item       inline cut at p
code block      the lines before the first line that does not fit
                that line, cut at the grapheme cluster boundary at or before p, when it is the first line
table           the header row (first row) and the rows before the first row that does not fit
                a table with the header row only is left out
rule            left out
unknown block   kept

inline cut
  p at a line break   line breaks and empty lines before p are dropped
  p inside a line     moved back to the last white space within the last MSG_LINE_CHARS units before p
                      without such white space: to the grapheme cluster boundary at or before p
  segment at p        none: cut
                      bold: cut; the format is kept
                      italic: as bold
                      strikeThrough: as bold
                      snippet: as bold
                      small: as bold
                      colored: as bold
                      secret: as bold
                      strong: as bold
                      other: replaced by a segment without format that has the cut display text
```

- The cut inline content is a prefix of the segment list with the last segment cut.
- The display text of a mention is built from the names of `ci.mentions`; of a SimpleX link, from the SimpleX link mode.
- Command: the drawn text is "/" and `commandStr` (`TextItemView.kt:270-279`), the display text is `ft.text` (`:106`).

## Truncated item

```
blocks         teaser
               paragraph of one segment: "Read more", Format.Modal with modal name "message" and text ""
drawn by       a list of paragraphs only: one text, the segments of markdownSegments, by MarkdownText and messageText
               any other list: MsgBlocksView of the rendering plan
end of text    typing indicator of a live item and meta reserve: at the end of the last paragraph (rendering plan)
selection      the paragraph of the link is outside the desktop selection
link           drawn and handled by the Format.Modal code of the description link (TextItemView.kt:334-340, 392-411; MsgContentView.swift:234-235, 560-565)
```

## Viewer

Kotlin `showFullMessage` (`TextItemView.kt`), iOS `FullMessageView` (`FullMessageView.swift`):

```
content        the blocks of the item, drawn with the geometry and colours of the classic layout
chatLayout     the layout of the chat view of the tap: Kotlin LocalChatLayout.current, iOS @Environment(\.chatLayout)
items          msgLines: one MsgLine (block index, range in the display text of the block) per lazy item
               one per line of a paragraph, heading, quote, list item and code block
               a line above 15,000 units: items of 15,000 units by the inline cut, as MarkdownFileView.kt:57, :95
               one per table, rule and unknown block, with the range null
               drawn by MsgBlockView(block, range, first, last) of the rendering plan
item           looked up by ID at each draw: Kotlin chatsCtx.chatItems, snapshot state (ChatModel.kt:424)
               iOS im.reversedChatItems, published with a 0.2 s throttle (ChatModel.swift:78-106)
closed         the item is removed from the chat items: deleted, or unloaded by pagination
               meta.itemDeleted of the item differs from its value at opening
               the open chat is changed: Kotlin KeyChangeEffect on chatModel.chatId, as ChatView.kt:753-755; iOS onChange of ChatModel.chatId
live item      the items are built from the looked-up item at each update; the texts are drawn without meta; the list state is kept
links          actions of the item text (TextItemView.kt:371-382, MsgContentView.swift:197-222)
secrets        shown per lazy item; reset with the item key
commands       sendCommandMsg (FramedItemView.kt:479, CommandsMenuView.swift:164), enabled as in the item text (FramedItemView.kt:465, MsgContentView.swift:108)
               then close (Kotlin), dismiss (iOS)
copy button    whole text, ci.content.text
```

## Kotlin

Paths are relative to `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.

```
model/ChatModel.kt, Format.Modal companion (:5221-5223)
  const val Message = "message"
views/chat/item/TextItemView.kt, new
  constants of "Teaser rule"
  msgTeaser(blocks, live, maxLines, teaserLines, maxUnits, teaserUnits, maxBlocks, lineChars, displayText): List<MarkdownBlock>?
                                    pure function; null: drawn in full
  msgPrefix(segments, maxUnits, displayText): List<FormattedText>
  itemTeaser(ci, linkMode, maxLines, teaserLines, maxUnits, teaserUnits): List<MarkdownBlock>?
                                    blocks drawn by CIMarkdownText: live text (FramedItemView.kt:459), chat link removed; maxBlocks = MSG_MAX_BLOCKS
  MsgLine(block: Int, range: IntRange?); msgLines(blocks, displayText): List<MsgLine>, items of "Viewer"
  showFullMessage(chatsCtx, chat, ci, chatLayout)
views/chat/item/TextItemView.kt, MarkdownText (:125)
  openModal: ((Format.Modal) -> Unit)? = null    new parameter
  MODAL tap (:392-396)                           openModal; null: openMarkdownModal
  long press (:357-366)                          condition hasLinks || hasModals; MODAL annotation: onLinkLongClick("")
views/chat/TextSelection.kt, ItemContext (:48-50)
  new fields                        the four line and unit limits of "Limits", Int = Int.MAX_VALUE
views/chat/ChatView.kt, ChatItemsList (:1750)
  terminal limits                   LocalChatLayout.current == ChatLayout.Terminal || searchValue.value.isNotBlank()
  :2440                             ItemContext(selectionIndex = index, and the limits)
  :2383                             SelectionHandler(manager, listState, mergedItems, revealedItems, linkMode, and the limits)
views/chat/item/FramedItemView.kt
  CIMarkdownText (:445)             teaser = itemTeaser(ci, linkMode, the limits of LocalItemContext.current)
  with a teaser                     blocks of "Truncated item"; link text MR.strings.whats_new_read_more
                                    openModal = showFullMessage(chatsCtx, chat, ci, LocalChatLayout.current)
  without a teaser                  drawn as by the rendering plan
  :225, :372, :380, :386, :415      onLinkLongClick is passed to CIMarkdownText
views/chat/item/TextItemView.kt, showFullMessage
  presentation                      main chat view (chatsCtx.secondaryContextFilter == null): ModalManager.center.showModalCloseable, as showMarkdownFile (MarkdownFileView.kt:99)
                                    Android: full screen (ModalView.kt:249-253); desktop: over the conversation (App.kt:419-420), closed when another chat is opened (App.kt:392-400)
                                    member support chat and reports view: ModalManager.end.showModalCloseable, stacked over that view, as item info (ChatView.kt:728-731)
                                    after back that view is composed again (ModalView.kt:194-223)
  title                             saved_message_title, sent_message or received_message, as ChatItemInfoView.kt:255
  toolbar                           copy_verb, with the toast `copied`
  body                              SelectionContainer { MarkdownFileColumn(title, false) { the items of "Viewer" } } (MarkdownFileView.kt:158, :227)
                                    MarkdownFileColumn is made internal; item key: index and text of the line
  text                              the parameters of CIMarkdownText (FramedItemView.kt:463-475), without meta and selection; style as TextItemView.kt:60
                                    first line: the report reason prefix
  selection                         SelectionContainer over the composed items; the copied text is the display text
views/chat/TextSelection.kt, desktop selection
  SelectionManager.itemBlocks       (ChatItem) -> List<MarkdownBlock>?, set in SelectionHandler with listState (:339-341)
                                    itemTeaser(ci, linkMode, and the limits) ?: ci.formattedText
  snapSelection (:121-137)          itemBlocks(ci)
  getSelectedCopiedText (:214-227)  item with the range 0 until Int.MAX_VALUE (:256): the item prefix and ci.text (rendering plan, section 5)
                                    anchor and focus items: itemBlocks(ci), the selected teaser text
  offsets                           in the display text of markdownSegments, by the rendering plan (selectedItemCopiedText :264, snapOffset :295)
```

## iOS

Paths are relative to `apps/ios/`. `FullMessageView.swift` is a new file, added to the SimpleX target in `project.pbxproj`.

```
SimpleXChat/ChatTypes.swift
  Format, after :5713              static let modalMessage = "message"
  after FormattedText (:5665)      public msgPrefix(_ segments, maxUnits:, displayText:) -> [FormattedText]
                                   public functions of "Cut inside a block", used by msgPrefix and msgTeaser
                                   units by String.utf16 indices with a limit; cuts at Character boundaries
                                   MSG_LINE_CHARS
                                   MSG_PREVIEW_UNITS
Shared/Views/Chat/ChatItem/MsgContentView.swift
  the other constants of "Teaser rule"
  msgTeaser(_ blocks, live:, maxLines:, teaserLines:, maxUnits:, teaserUnits:, maxBlocks:, mentions:) -> [MarkdownBlock]?
  segmentDisplayText(_ ft, mentions:) -> String        display text of a segment, as drawn by messageText (:446-570)
  MsgLine; msgLines(_ blocks, mentions:) -> [MsgLine]  items of the viewer, as in Kotlin
  MsgContentView.openModal: ((Format) -> Void)? = nil  passed to msgTextResultView (:123)
Shared/Views/Chat/ChatItem/FramedItemView.swift, ciMsgContentView (:359)
  terminal limits                  @Environment(\.chatLayout) is .terminal or @Environment(\.textSearch) is true
  teaser                           msgTeaser(ft ?? [paragraph of FormattedText.plain(text)], live: ci.meta.isLive, the limits, maxBlocks: MSG_MAX_BLOCKS, mentions: ci.mentions)
                                   ft: the blocks with the chat link removed (rendering plan)
  with a teaser                    blocks of "Truncated item"; link text: the key "Read more" of MsgContentView.swift:379
                                   openModal = showFullMessage(chat, im, ci, chatLayout); chatLayout: @Environment(\.chatLayout)
Shared/Views/Chat/ChatItemView.swift, EnvironmentValues (:12-39)
  textSearch: Bool                 default false
Shared/Views/Chat/ChatView.swift
  chatItemWithMenu (:2145-2146)    .environment(\.textSearch, searchText is not blank)
  chatItemsList (:996)             on a change of the blank state of searchText: scrollView.updateItems(mergedItems.boxedValue.items, true)
Shared/Views/Chat/ChatItem/FullMessageView.swift
  showFullMessage(_ chat, _ im, _ ci, _ chatLayout)
                                   showAppSheet (AppSheet.swift:34), as showMarkdownFile (MarkdownFileView.swift:31)
                                   screen protection by its redaction (AppSheet.swift:22-30)
  FullMessageView                  NavigationView with an inline title, as MarkdownFileView.swift:170-213
                                   title: the key "Saved message", "Sent message" or "Received message" of ChatItemInfoView.swift:61-67
                                   toolbar: "Copy"
                                   ScrollView with LazyVStack of the items of "Viewer"; id: index and text of the line
                                   @ObservedObject im; dismissed by the rules of "Viewer"
  MessageLineView                  one item of "Viewer": MsgBlockView of the rendering plan with the chatLayout of "Viewer"
                                   showSecrets state and msgTextResultView with sendCommand, as TextBubble (ChatItemInfoView.swift:308-322)
```

## Shared code

```
function                                         this plan            layout plan       rendering plan
MarkdownText (TextItemView.kt:125)               openModal            chatLayout        Strong; text of one block
CIMarkdownText (FramedItemView.kt:445)           teaser; openModal    paddings          MsgBlocksView
messageText, MsgContentView                      openModal            chatLayout        strong; text of one block
ciMsgContentView (FramedItemView.swift:359)      teaser; openModal    paddings; frame   MsgBlocksView
selectedItemCopiedText (TextSelection.kt:264)    blocks of the item                     offsets in markdownSegments
snapOffset (TextSelection.kt:295)                blocks of the item                     offsets in markdownSegments
chatItemWithMenu (ChatView.swift:2121)           textSearch           alignment         coordinate space
```

Order of the edits: rendering plan part 1, layout plan, this plan, rendering plan part 2. Release 1: the segment list of the item as one paragraph; release 2: the blocks of the core.

## Other views

```
chat list preview       msgPrefix(markdownSegments(blocks), MSG_PREVIEW_UNITS) before the renderer: ChatPreviewView.kt:277, ChatPreviewView.swift:302
compose context item    the same: ContextItemView.kt:59, ContextItemView.swift:74
quote in an item        the same: FramedItemView.kt:61, FramedItemView.swift:337; 2 or 3 lines, as now (FramedItemView.kt:96-99, FramedItemView.swift:324-327)
notification text       the same before hideSecrets, with displayText ft.text: NtfManager.kt:134, Notifications.swift:200
hidden deleted item     unchanged: header (ChatItemView.kt:698-700, ChatItemView.swift:75-76)
emoji only item         unchanged (isShortEmoji)
scroll to item          unchanged (ChatView.kt:3139, ChatView.swift:812)
item info               unchanged: versions in full (ChatItemInfoView.kt:55-73, ChatItemInfoView.swift:298-322)
copy, share, edit       unchanged: whole ci.content.text (ChatItemView.kt:426-448, 464-468; ChatView.swift:2479-2506, 2546-2557)
```

## Docs

- `apps/multiplatform/spec/client/chat-view.md:319`: `TextItemView.kt` row: teaser, viewer.
- `apps/multiplatform/spec/client/navigation.md:266-267`: message viewer in the rows of `ModalManager.center` and `ModalManager.end`.
- `apps/ios/spec/client/chat-view.md:188-192`: `MsgContentView`: teaser; new `FullMessageView` entry.
- Text row, teaser and "Read more": `apps/multiplatform/product/views/chat.md:57`, `apps/ios/product/views/chat.md:52`.

## Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/MsgTeaserTest.kt`, `msgTeaser` with `lineChars` 32, limits 24 and 16:

```
24 lines "a" in one paragraph                              null
25 lines "a" in one paragraph                              lines 1 to 16
"word " 600 times                                          102 words, 509 units
live: "a" 288 times, LF, "b" 225 times                     line 1, LF, "b" 224 times
one-line command: "/" and 2999 units                       one segment without format, 512 units
secret of 3000 units                                       one secret segment, 512 units
1500 surrogate pairs                                       256 pairs
U+4E00 400 times                                           256 units
24 lines "a" separated by CRLF; 25 by U+2028               null; lines 1 to 16
25 mentions of 41 units without white space, spaced        12 mentions
13 paragraphs of one line                                  8 paragraphs
30 list items of one line                                  16 list items
heading, then a paragraph of 30 lines                      heading; paragraph of 14 lines
code block of 30 lines                                     header and 15 lines
terminal limits, one code line of 124,816 units            code block of one line of 12,800 units
table: header row and 30 rows of short cells               header row and 15 rows
table: header row, one row with a cell of 3000 units       empty teaser
paragraph of 15 lines, rule, paragraph of 15 lines         paragraph of 15 lines
quote of 30 lines                                          quote of 16 lines
maxBlocks 3, 4 list items of one line                      3 list items
msgPrefix: "word " 600 times, 1000 units                   200 words, 999 units
```

`TextSelectionTest.kt`, `getSelectedCopiedText`: three items with the middle one truncated give the whole text of the middle item; a partly selected truncated item gives the selected teaser text. iOS: the vectors of `msgTeaser` and `msgPrefix`, checked as messages in a chat.

Checks, Android, desktop and iOS:

1. Classic layout: a message above the limit is drawn as the teaser with "Read more"; a message at the limit is drawn in full; the teaser has about 16 lines on a phone and in a desktop window.
2. Tap on "Read more" in the main chat view: the viewer with the whole message; each tap action of the item text; after a command tap the viewer is closed; after back the chat is at the same scroll position.
3. Viewer opened from a chat with the terminal layout: `*x*` is drawn by the rule of the layout plan.
4. Live item above the teaser cost: the item height is constant while the item is updated; the updates are shown in the open viewer. The viewer is closed when its item is deleted, marked deleted or removed by a timer.
5. Search with a match after the teaser: the match is drawn; after the search is closed the item is a teaser; iOS cell heights are correct in both states.
6. Desktop: with a drag selection over a truncated item the highlight ends at the teaser end; Ctrl+C over three items gives the whole text of the middle item. Kotlin: after a long press on "Read more" the item menu is shown.
7. Item of 124,816 bytes: chat list preview, compose context item, quote and notification show its first 1000 units.
8. Teaser with each block type last: code, table, list item, quote; the link paragraph and the meta after it.

## Interfaces

```
both      constants of "Teaser rule"
          msgTeaser
          msgPrefix
          showFullMessage
Kotlin    itemTeaser
          MarkdownText(openModal)
          Format.Modal.Message
          ItemContext: the line and unit limits of "Limits"
          SelectionManager.itemBlocks
iOS       MsgContentView.openModal
          Format.modalMessage
          EnvironmentValues.textSearch
```

## Depends on

```
agent profile plan    ChatLayout: Classic, Terminal, Unknown(type)
                      ChatInfo.activeChatLayout, computed by the core (patch 01 rework; ChatInfo.chatLayout on ep/llm-ux)
layout plan           LocalChatLayout, default ChatLayout.Classic
                      EnvironmentValues.chatLayout, default .classic
                      MSG_TERMINAL_MAX_LINES = 500
                      MSG_TERMINAL_TEASER_LINES = 400
                      the chatLayout parameter of the texts
                      the forced cell reload on iOS
block Markdown plan   MarkdownBlock of "Types", release 2: blocks without nesting; rows[0] of a table is the header row
                      formattedText == null for a plain text
                      markdownSegments: the one-text form
rendering plan        app types of MarkdownBlock, in release 1 without JSON use
                      markdownSegments in the apps from release 1; iOS: in SimpleXChat, used by the NSE
                      MsgBlocksView: a teaser list with another block kind and the link paragraph; gaps between blocks (2.2)
                      meta and typing indicator at the end of the last paragraph (2.4); openModal passed to every text (3)
                      MsgBlockView(block, range, first, last): range in the display text of the block, null for the whole block
                      MSG_MAX_BLOCKS
                      CODE_MAX_WIDTH
                      horizontal scroll of code and tables; with wrapped code lines: chars MSG_LINE_CHARS for code
                      removal of the chat link from the blocks
                      desktop selection over markdownSegments, the link paragraph excluded (5)
                      a list of paragraphs only: one text, markdownSegments, by MarkdownText and messageText (1)
message parts plan    item text of at most maxMsgParts × maxEncodedMsgLength = 124,816 bytes
```

## Open questions

1. "Read more" in the terminal layout: the product owner said that the terminal layout does not need read more. A terminal row above 500 lines (16,000 characters on a phone, 40,000 on desktop) or above `MSG_MAX_BLOCKS` blocks is a teaser; smaller texts are drawn in full. Recommended: keep these limits: one row of 124,816 bytes is one lazy item, laid out as a whole.
2. Maximum text of one item: 8 parts (124,816 bytes, 16 items per recipient queue) or 32 parts (499,264 bytes, 4 items). Recommended: 8.
3. `*x*` is kept as `Bold` (bold in the classic layout, italic in the terminal layout) and `**x**` is `Strong` (bold in both). Surfaces of a terminal chat with italic `*x*`: rows only, or rows and the viewer. Recommended: rows and the viewer; bold in previews, compose context and item info.
4. Read more in the classic layout: truncated above 24 lines, teaser of 16 lines (line: 32 units on phones, 80 on desktop); a viewer screen in place of expansion in the item; live items truncated while updated; search results with the terminal limits. Recommended: accept; thresholds adjusted after a trial.
5. Release grouping: three releases: (1) message priority, chat layout, terminal layout, read more, `Strong` with rendering part 1; (2) blocks of the core with rendering part 2; (3) message parts. Recommended: yes.
