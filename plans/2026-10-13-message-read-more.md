# Read more for long messages

Date: 2026-10-13.

The app shows a long message as a teaser followed by a "Read more" link; after a tap on the link the app shows the whole message in a viewer.

## Scope

Only the Kotlin and iOS apps are changed.

- The teaser is cut from the parsed segment list `ChatItem.formattedText`, the output of the core parser (`Markdown.hs:148-167`, `Messages.hs:265`, `Store/Messages.hs:2583`).
- `Format.Modal` is an app-only format: Kotlin `ChatModel.kt:5180`, iOS `ChatTypes.swift:5657`. Its other use, the description link (`ChatInfoView.kt:812`, `MsgContentView.swift:382`), is unchanged.
- Desktop linked to a mobile: the teaser is computed by the desktop app from the items of the mobile core.

## Items

```
text path                Kotlin CIMarkdownText (FramedItemView.kt:445)
                         iOS FramedItemView.ciMsgContentView (FramedItemView.swift:359)
limits, bubbles          MSG_MAX_LINES, MSG_TEASER_LINES
limits, flat             MSG_FLAT_MAX_LINES, MSG_FLAT_TEASER_LINES (flat layout plan)
search text not blank    the flat limits, in both layouts
layout of a chat view    flat layout plan: LocalChatLayout, EnvironmentValues.chatLayout
```

The teaser rule is applied to the text of:

```
text
link preview with text
image, video, voice or file with text
unknown content with text
chat link with text          the link is stripped first (stripTextLink, stripFormattedTextLink)
report                       the reason prefix (itemPrefixText) is outside the count
forwarded                    header unchanged
edited                       current text
live, while updated          truncated above the teaser cost
marked deleted, revealed
```

## Teaser rule

Input: the segment list of the item. A text with `formattedText == null` is one segment without format. From release 2 (block parsing of the core) the list has marker segments (`Block`, `TableRow`, `TableCell`, `Syntax`); the text of a marker is source text.

Constants and terms:

```
MSG_LINE_CHARS       32 on Android and iOS
                     80 on desktop
MSG_MAX_LINES        24
MSG_TEASER_LINES     16
MSG_PREVIEW_UNITS    1000
unit                 UTF-16 code unit
display text         the displayText argument
                     passed by itemTeaser: itemSegmentDisplayText(ft, ci, linkMode) (TextItemView.kt:88)
                     release 2 (block parsing of the core): for a marker (Format.isMarker), ft.text
                     iOS: segmentDisplayText (new), with the same marker case
text                 display texts of the segments, in order
line break           Kotlin: a match of the regular expression \R; Swift: a Character with isNewline
width                1 per unit
                     2 per unit in U+2E80-U+A4CF
                     2 per unit in U+AC00-U+D7A3
                     2 per unit in U+F900-U+FAFF
                     2 per unit in U+FF00-U+FF60
line cost            max(1, ceil(width of the line / MSG_LINE_CHARS))
                     release 2, line of a code block (by the Block marker before it): max(1, ceil(width / MSG_CODE_LINE_CHARS)); MSG_CODE_LINE_CHARS = 240, the units drawn in CODE_MAX_WIDTH
                     release 2, table row (by the Block marker before it): the largest of max(1, ceil(width of the cell / MSG_CELL_CHARS)) over its cells; MSG_CELL_CHARS = 30, the units drawn in a column of 240
cost                 sum of the line costs
```

`itemSegmentDisplayText` is changed by the rendering plan only: empty for a marker, for desktop selection.

```
msgTeaser(segments, live, maxLines, teaserLines, maxUnits, teaserUnits, maxBlocks):
  limit: cost above maxLines (teaserLines for a live item) or units above maxUnits (teaserUnits for a live item); within the limit: Nothing, drawn in full
  p: end of the longest prefix with cost <= teaserLines and units <= teaserUnits
  release 2: a list with more than maxBlocks Block markers is a teaser; p is at or before the start of the Block marker number maxBlocks + 1
  return segments cut at p

msgPrefix(segments, maxUnits):
  if units of text <= maxUnits: return segments
  return segments cut at p = maxUnits

p at a line break    line breaks and empty lines before p are dropped
p inside a line      moved back to the last white space within the last MSG_LINE_CHARS units before p
                     without such white space: to the grapheme cluster boundary at or before p

segment at p, by format
  none               cut
  bold               cut; the format is kept
  italic             as bold
  strikeThrough      as bold
  snippet            as bold
  small              as bold
  colored            as bold
  secret             as bold
  strong             as bold (release 1)
  marker             excluded (release 2)
  other              replaced by a segment without format that has the cut display text
```

- The result is a prefix of the segment list with the last segment cut. A teaser can be empty.
- The display text of a mention has the names of `ci.mentions`; of a SimpleX link, the text of the SimpleX link mode.
- Command: the drawn text is "/" and `commandStr` (`TextItemView.kt:270-279`), the display text is `ft.text` (`:106`).

## Truncated item

Text, in order:

```
teaser
line break           FormattedText("\n"); omitted after an empty teaser
                     release 2, list with a Block marker: the appended segments rule of the block Markdown plan ("Rendering contract")
"Read more"          Format.Modal: modal name "message", text ""
typing indicator     live item
meta reserve         on the line of the link
                     on the next line when the text direction differs from the app direction (TextItemView.kt:156-157, MsgContentView.swift:135)
```

The link is drawn and handled by the `Format.Modal` code of the description link (`TextItemView.kt:334-340, 392-411`; `MsgContentView.swift:234-235, 560-565`).

## Viewer

Kotlin `showFullMessage` (`TextItemView.kt`), iOS `FullMessageView` (`FullMessageView.swift`):

```
content        whole message, drawn by the message renderer (Kotlin MarkdownText, iOS messageText) with the geometry and colours of bubbles
chatLayout     ChatInfo.chatLayout of the chat, passed by showFullMessage to MarkdownText(chatLayout) and messageText(chatLayout:) (flat layout plan)
items          list without markers: one lazy item per line; segments without format are split at the line break of the teaser rule (msgLines)
               a line above 15,000 units is cut into items of 15,000 units by the segment rules of the teaser
               release 2, list with markers: the items of the rendering plan (msgBlocks, MsgBlockView)
item           looked up by ID at each draw: Kotlin chatsCtx.chatItems, snapshot state (ChatModel.kt:424)
               iOS im.reversedChatItems, published with a 0.2 s throttle (ChatModel.swift:78-106)
closed         the item is removed from the chat items: deleted, or unloaded by pagination
               meta.itemDeleted of the item differs from its value at opening
               the open chat is changed: Kotlin KeyChangeEffect on chatModel.chatId, as ChatView.kt:753-755; iOS onChange of ChatModel.chatId
live item      the lines are built from the looked-up item at each update; MarkdownText is called without meta; the list state is kept
links          actions of the bubble (TextItemView.kt:371-382, MsgContentView.swift:197-222)
secrets        showSecrets of MarkdownText (TextItemView.kt:172) and of MessageLineView, per lazy item; reset with the item key
commands       sendCommandMsg (FramedItemView.kt:479, CommandsMenuView.swift:164), enabled as in the bubble (FramedItemView.kt:465, MsgContentView.swift:108); then close (Kotlin), dismiss (iOS)
copy button    whole text, ci.content.text
state          the open viewer: an entry of ModalManager.center or ModalManager.end (Kotlin), a sheet (iOS)
```

## Kotlin

`model/ChatModel.kt`, `Format.Modal` companion (`:5181`): `Message = "message"`.

`views/chat/item/TextItemView.kt`, new:

```
MSG_LINE_CHARS, MSG_MAX_LINES, MSG_TEASER_LINES, MSG_PREVIEW_UNITS
msgTeaser(segments, live, maxLines, teaserLines, maxUnits, teaserUnits, maxBlocks, lineChars, displayText): List<FormattedText>?    pure function; null: drawn in full
msgPrefix(segments, maxUnits, displayText): List<FormattedText>          pure function
itemTeaser(ci, linkMode, maxLines, teaserLines, maxUnits, teaserUnits): List<FormattedText>?    on the text and segments drawn by CIMarkdownText: live text (FramedItemView.kt:459), chat link stripped; maxBlocks = MSG_MAX_BLOCKS (rendering plan)
msgLines(segments, displayText): List<List<FormattedText>>               items of the viewer
showFullMessage(chatsCtx, chat, ci)                                      viewer
```

`MarkdownText` (`:125`):

```
suffix: List<FormattedText>? = null            new parameter
openModal: ((Format.Modal) -> Unit)? = null    new parameter
segment loop (:229)                            over formattedText + suffix; selectableEnd (:344) is set at index formattedText.size
index lookups (:376-380, :394)                 in formattedText + suffix
MODAL tap (:392-396)                           openModal; null: openMarkdownModal
long press (:357-366)                          condition hasLinks || hasModals; MODAL annotation: onLinkLongClick("")
highlight (:213, :353)                         clamped to selectableEnd - 1
```

Limits and teaser:

```
ItemContext (TextSelection.kt:48-50)      new fields maxLines, teaserLines, maxUnits, teaserUnits: Int = Int.MAX_VALUE
ChatItemsList (ChatView.kt)               flat limits, when LocalChatLayout.current == ChatLayout.Flat || searchValue.value.isNotBlank(): MSG_FLAT_MAX_LINES, MSG_FLAT_TEASER_LINES, maxUnits = MSG_FLAT_MAX_LINES × MSG_LINE_CHARS, teaserUnits = MSG_FLAT_TEASER_LINES × MSG_LINE_CHARS; bubbles: MSG_MAX_LINES, MSG_TEASER_LINES, Int.MAX_VALUE for both unit limits
                                          :2440: ItemContext(selectionIndex = index, maxLines, teaserLines, maxUnits, teaserUnits)
                                          :2383: SelectionHandler(manager, listState, mergedItems, revealedItems, linkMode, maxLines, teaserLines, maxUnits, teaserUnits)
CIMarkdownText (FramedItemView.kt:445)    teaser = itemTeaser(ci, linkMode, the four limits of LocalItemContext.current)
with a teaser                             formattedText = teaser
                                          suffix = the line break and the link of "Truncated item"; link text MR.strings.whats_new_read_more
                                          openModal = showFullMessage(chatsCtx, chat, ci)
                                          stripLink = null; the link is stripped by itemTeaser
without a teaser                          the MarkdownText call is unchanged
FramedItemView.kt:225, :372, :380, :386, :415    onLinkLongClick is passed to CIMarkdownText
```

Viewer, `showFullMessage`:

```
presentation    main chat view (chatsCtx.secondaryContextFilter == null): ModalManager.center.showModalCloseable
                Android: full screen (ModalView.kt:249-253); desktop: over the conversation (App.kt:419-420), closed when another chat is opened (App.kt:393-399)
                member support chat and reports view: ModalManager.end.showModalCloseable, stacked over it, as item info (ChatView.kt:728-731); after back that view is composed again (ModalView.kt:194-223)
title           saved_message_title, sent_message or received_message, as ChatItemInfoView.kt:255
toolbar         copy, with the toast `copied`
body            SelectionContainer { MarkdownFileColumn(title, false) { the items of "Viewer" } } (MarkdownFileView.kt:158, 227); item key: index and text of the line
                MarkdownFileColumn (MarkdownFileView.kt:227) is made internal
line            MarkdownText with the arguments of CIMarkdownText (FramedItemView.kt:463-475), without meta and selection
                chatLayout: chat.chatInfo.chatLayout; style: as showFullProfileDescription (TextItemView.kt:60)
                first line: the report reason prefix
selection       SelectionContainer over the composed items; the copied text is the display text
```

Desktop text selection, `views/chat/TextSelection.kt`:

```
SelectionManager.itemSegments: (ChatItem) -> List<FormattedText>?    set in SelectionHandler with listState (:339-341): itemTeaser(ci, linkMode, maxLines, teaserLines, maxUnits, teaserUnits) ?: ci.formattedText
selectedItemCopiedText (:264)    new parameter segments, in place of ci.formattedText
snapOffset (:295)                new parameter segments, in place of ci.formattedText
snapSelection (:121-137)         itemSegments(ci)
getSelectedCopiedText (:214-227) item with the range 0 until Int.MAX_VALUE (:256): ci.formattedText, the whole text
                                 anchor and focus items: itemSegments(ci), the selected teaser text
selectable text                  teaser; the suffix, the typing indicator and the meta reserve are placed after selectableEnd
```

## iOS

`SimpleXChat/ChatTypes.swift`:

```
Format (:5663)                 modalMessage = "message"
after FormattedText (:5641)    public msgPrefix(_ segments, maxUnits:, displayText:) -> [FormattedText]; units by String.utf16 indices with a limit; cut at a Character boundary
                               MSG_LINE_CHARS, MSG_PREVIEW_UNITS
```

`Shared/Views/Chat/ChatItem/MsgContentView.swift`:

```
MSG_MAX_LINES, MSG_TEASER_LINES
msgTeaser(_ segments, live:, maxLines:, teaserLines:, maxUnits:, teaserUnits:, maxBlocks:, mentions:) -> [FormattedText]?    the cut of msgPrefix at the position of the teaser rule
segmentDisplayText(_ ft, mentions:) -> String           display text of a segment, as drawn by messageText (:446-568)
msgLines(_ segments, mentions:) -> [[FormattedText]]    items of the viewer
MsgContentView.openModal: ((Format) -> Void)? = nil     passed to msgTextResultView (:123)
```

`Shared/Views/Chat/ChatItem/FramedItemView.swift`, `ciMsgContentView` (`:359`):

```
limits           flat limits, when @Environment(\.chatLayout) is .flat or @Environment(\.textSearch) is true: MSG_FLAT_MAX_LINES, MSG_FLAT_TEASER_LINES, maxUnits = MSG_FLAT_MAX_LINES × MSG_LINE_CHARS, teaserUnits = MSG_FLAT_TEASER_LINES × MSG_LINE_CHARS; bubbles: MSG_MAX_LINES, MSG_TEASER_LINES, Int.max for both unit limits
text, ft         the link is stripped here (stripTextLink, stripFormattedTextLink) when stripLink is given
teaser           msgTeaser(ft ?? FormattedText.plain(text), live: ci.meta.isLive, maxLines:, teaserLines:, maxUnits:, teaserUnits:, maxBlocks: MSG_MAX_BLOCKS, mentions: ci.mentions)
with a teaser    formattedText = teaser, then the line break and the link of "Truncated item"; link text NSLocalizedString("Read more", ...)
                 openModal = showFullMessage(chat, im, ci)
```

Search state:

```
EnvironmentValues (ChatItemView.swift:12-39)    textSearch: Bool, default false
chatItemWithMenu (ChatView.swift:2145-2146)     .environment(\.textSearch, searchText is not blank)
chatItemsList (ChatView.swift:996)              on a change of the blank state of searchText: scrollView.updateItems(mergedItems.boxedValue.items, true)
```

New file `Shared/Views/Chat/ChatItem/FullMessageView.swift`, added to the SimpleX target in `project.pbxproj`:

```
showFullMessage(_ chat, _ im, _ ci)    showAppSheet (AppSheet.swift:34); screen protection by its redaction (AppSheet.swift:22-30)
FullMessageView                        NavigationView with an inline title: "Saved message", "Sent message" or "Received message", as ChatItemInfoView.swift:61-67; toolbar: copy
                                       ScrollView with LazyVStack of the items of "Viewer"; id: index and text of the line
                                       @ObservedObject im; dismissed by the rules of "Viewer"
MessageLineView                        as TextBubble (ChatItemInfoView.swift:308-322): showSecrets state; messageText of the line with chatLayout: chat.chatInfo.chatLayout; msgTextResultView with showSecrets and sendCommand
```

## Shared code

Order of the edits. Release 1: rendering plan part 1 (app types, `strong`), flat layout plan, this plan. Release 2: the marker, code line and table row rules of this plan; rendering plan part 2 (block views).

```
function                                         this plan      flat layout plan     rendering plan
MarkdownText (TextItemView.kt:125)               suffix         chatLayout           Strong
                                                 openModal                           MarkdownText(range, markerText)
CIMarkdownText (FramedItemView.kt:445)           teaser         paddings             segments with the suffix
                                                 openModal      chatLayout passed    block view for a list with a marker
messageText, MsgContentView                      openModal      chatLayout           strong
                                                                                     messageText(range:markerText:fontScale:bold:)
ciMsgContentView (FramedItemView.swift:359)      teaser         paddings             MsgBlocksView
                                                 openModal      frame
itemSegmentDisplayText (TextItemView.kt:88)                                          empty for a marker
stripFormattedTextLink (TextItemView.kt:622)                                         a last marker with white-space text is removed
selectedItemCopiedText (TextSelection.kt:264)    segments                            marker rule
ItemSelection, setupItemSelection (TextSelection.kt:428, 437)                        parts
chatItemWithMenu (ChatView.swift:2121)           textSearch     alignment            coordinate space
                                                                go-to button         drag gesture
```

## Other views

```
search results          flat limits
chat list preview       msgPrefix(segments, MSG_PREVIEW_UNITS) before the renderer: ChatPreviewView.kt:277, ChatPreviewView.swift:302
compose context item    the same: ContextItemView.kt:59, ContextItemView.swift:74
notification text       the same on the segments joined in hideSecrets, with displayText ft.text: NtfManager.kt:134, Notifications.swift:200
hidden deleted item     unchanged: header (ChatItemView.kt:698-700, ChatItemView.swift:75-76)
emoji only item         unchanged (isShortEmoji)
quote in a bubble       unchanged: 2 or 3 lines (FramedItemView.kt:96-99, FramedItemView.swift:324-327)
scroll to item          unchanged (ChatView.kt:3139, ChatView.swift:812)
item info               unchanged: versions in full (ChatItemInfoView.kt:55-73, ChatItemInfoView.swift:298-322)
copy, share, edit       unchanged: whole ci.content.text (ChatItemView.kt:426-448, 464-468; ChatView.swift:2479-2506, 2546-2557)
```

## Strings

```
reused          Kotlin, base/strings.xml    iOS key
link            whats_new_read_more         "Read more"
viewer title    saved_message_title         "Saved message"
                sent_message                "Sent message"
                received_message            "Received message"
toolbar         copy_verb, copied           "Copy"
```

## Docs

- `apps/multiplatform/spec/client/chat-view.md`: `TextItemView.kt` row: teaser, viewer.
- `apps/multiplatform/spec/client/navigation.md:266-267`: message viewer in the rows of `ModalManager.center` and `ModalManager.end`.
- `apps/multiplatform/product/views/chat.md:57`: Text row: teaser, "Read more".
- `apps/ios/spec/client/chat-view.md:188-192`: `MsgContentView`: line anchor, teaser; new `FullMessageView` entry.
- `apps/ios/product/views/chat.md:52`: Text row: teaser, "Read more".

## Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/MsgTeaserTest.kt`, `msgTeaser` with `lineChars` 32, limits 24 and 16:

```
24 lines "a"                                               null
25 lines "a"                                               lines 1 to 16
"word " 600 times                                          102 words, 509 units
live: "a" 288 times, LF, "b" 225 times                     line 1, LF, "b" 224 times
one-line command: "/" and 2999 units                       one segment without format, 512 units
URI of 3000 units                                          one segment without format, 512 units
secret of 3000 units                                       one secret segment, 512 units
"word ", then 3000 units without white space               512 units
1500 surrogate pairs                                       256 pairs
U+4E00 400 times                                           256 units
30 lines "a" separated by U+2028                           lines 1 to 16
24 lines "a" separated by CRLF                             null
16 empty lines, then 10 lines "a"                          empty list
15 lines "a", 5 empty lines, 10 lines "a"                  lines 1 to 15
25 mentions with display texts of 41 units, with spaces    12 mentions
marker segment at the cut position                         excluded
code line of 100 units; of 3000 units                      cost 1; cost 13
flat limits, one code line of 124,816 units                teaser of 12,800 units
table row with a cell of 3000 units                        cost 100; a text of this row alone: teaser
msgLines: 30 lines "a" separated by U+2028                 30 items
msgPrefix: "word " 600 times, 1000 units                   200 words, 999 units
```

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/TextSelectionTest.kt`, `getSelectedCopiedText`: three items with the middle one truncated give the whole text of the middle item; a partly selected truncated item gives the selected teaser text.

iOS `msgTeaser` and `msgPrefix`: the same vectors, checked as messages in a chat.

Checks, Android, desktop and iOS:

1. Bubbles: a message above the limit is drawn as the teaser with "Read more"; a message at the limit is drawn in full; the teaser has about 16 lines on a phone and in a desktop window.
2. Tap on "Read more" in the main chat view: the viewer with the whole message; after back the chat is at the same scroll position.
3. Viewer: each tap action of the bubble text; after a command tap the viewer is closed.
4. Viewer opened from a flat chat: `*x*` is italic.
5. Live item above the teaser cost: the bubble height is constant while the item is updated; the updates are shown in the open viewer.
6. The viewer is closed when its item is deleted, marked deleted or removed by a timer.
7. Search with a match after the teaser: the match is drawn; after the search is closed the item is a teaser; iOS cell heights are correct in both states.
8. Desktop: with a drag selection over a truncated item the highlight ends at the teaser end; Ctrl+C over three items gives the whole text of the middle item.
9. Kotlin: after a long press on "Read more" the item menu is shown.
10. Item of 124,816 bytes: chat list preview, compose context item and notification show its first 1000 units.

## Interfaces

```
both      MSG_LINE_CHARS, MSG_MAX_LINES, MSG_TEASER_LINES, MSG_PREVIEW_UNITS, MSG_CODE_LINE_CHARS, MSG_CELL_CHARS
          msgTeaser, msgPrefix, msgLines, showFullMessage
Kotlin    itemTeaser
          MarkdownText(suffix, openModal)
          Format.Modal.Message
          ItemContext.maxLines, ItemContext.teaserLines, ItemContext.maxUnits, ItemContext.teaserUnits
          SelectionManager.itemSegments
iOS       segmentDisplayText
          FullMessageView, MessageLineView
          MsgContentView.openModal
          Format.modalMessage
          EnvironmentValues.textSearch
```

## Depends on

- Agent profile plan: `ChatLayout`, effective `ChatInfo.chatLayout`.
- Flat layout plan: `LocalChatLayout`, `EnvironmentValues.chatLayout`, `MSG_FLAT_MAX_LINES`, `MSG_FLAT_TEASER_LINES`, `MarkdownText(chatLayout)`, `messageText(chatLayout:)`, the forced cell reload on iOS.
- Block Markdown plan: marker segments in the flat `formattedText`; the text of the segments in order is the source text; the appended segments rule of "Rendering contract".
- Block Markdown rendering plan: `Format.isMarker`, `Format.Strong`; the teaser and its suffix are drawn by its renderer; the viewer items of a list with markers (`msgBlocks`, `MsgBlockView`, paragraphs and code divided at line breaks).
- Message parts plan: item text of at most `maxMsgParts × maxEncodedMsgLength` = 124,816 bytes.

## Open questions

1. "Read more" in the flat layout: a flat row above 500 lines (16,000 characters on a phone) is a teaser, against "CLI layout does not need read more". Recommended: accept; smaller texts are drawn in full.
2. Maximum text of one item: 8 parts (124,816 bytes, 16 items per recipient queue) or 32 parts (499,264 bytes, 4 items). Recommended: 8.
3. `*x*` is kept as `Bold` (bold in bubbles, italic in flat rows) and `**x**` is `Strong` (bold in both). Surfaces of a flat chat with italic `*x*`: rows only, or rows and the viewer. Recommended: rows and the viewer; bold in previews, compose context and item info.
4. Read more in bubbles: truncated above 24 lines, teaser of 16 lines (line: 32 units on phones, 80 on desktop); a viewer screen in place of expansion in the bubble; live items truncated while updated; search results with the flat limits. Recommended: accept; thresholds adjusted after a trial.
5. Name of the second layout. Recommended: `flat`.
6. Text copied from a desktop selection is source text, block syntax included. Recommended: yes.
7. Release grouping: three releases: (1) silent messages, agent profile, flat layout, read more, `Strong` with rendering part 1; (2) block parsing of the core and rendering part 2; (3) message parts. Recommended: yes.

The line cost of a code line and of a table row follows the scrolling decision of the rendering plan: a code line is scrolled horizontally and wrapped above `CODE_MAX_WIDTH`; the text of a table cell is wrapped in a column of at most 240.
