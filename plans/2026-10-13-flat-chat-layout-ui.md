# Terminal chat layout: UI

Date: 2026-10-13. Revised: 2026-10-14, 2026-10-19, 2026-10-21, 2026-10-24.

Chats with the layout `terminal` are drawn by the Android and desktop apps as full-width start-aligned rows without bubbles. iOS draws terminal rows too (section 4).

D1–D33: the numbered decisions of the layout review of 2026-10-23 and 2026-10-24.

```
specification    apps/multiplatform/spec/client/chat-view.md, sections 4 and 10
product          apps/multiplatform/product/views/chat.md, "Terminal Layout"
layout value     ChatInfo.activeChatLayout (agent profile plan, section 4)
```

## 1. Scope

```
terminal     main chat view of a direct chat, group, business chat or channel
             member support chat view and reports view of a terminal group
             note folder, when the profile choice or the app setting is terminal
             theme screens of a terminal chat or profile: preview and wallpaper presets
classic      chat item info
unchanged    chat list preview
             notification text
             compose view
             date separators
             context menu
             multi-select
```

## 2. Decisions

```
row               the classic item without bubble fill and tail; start-aligned for both directions
text edge         CHAT_ITEM_TEXT_PADDING (12dp) inside the item: text, quotes, headers, media, cards, events, voice, calls, reactions
sender row        classic group geometry and style: name 13.5sp, role label; own name in the primary colour
avatars           groups and channels only; gutter: member image width and 12dp, the avatar 12dp from the start
gutter            other chats: none (user, 2026-10-25)
padding           one 12dp padding: screen edge to the text or the avatar, avatar to the text, time to the screen edge; row start and end 0 (user, I26, 2026-10-26)
sender groups     new sender row after an event or a sender change, as classic: time gaps and date changes do not repeat it
time, status      at the end of the row, for every item with a time; baseline of the time on the last baseline of the content (D3)
                  message text ends with a transparent copy of the time, joined to the last word by no-break spaces
                  last line without room: the last word moves to the next line with the time
                  code block, table, rule or emoji without room: the time under them
                  images and videos without caption: over the media in white, as classic, when the row has no space beside them
quote             Markdown quote design: bar, text at 70%, 15sp; 5.75sp above, 5sp below, over the whole quote row
headers           all item headers without background; 2.5sp above, 1.5sp below; name → header level 3, header → text level 2 (D8)
                  icon 18dp, as classic, centred on the header text without height
media             message corner clip, start at the text edge, 5.75sp above; quote thumbnails: message corner clip; roundness preference (D22)
quote thumbnail   12dp after the quote text; 12dp after the bar when the quote has no text and no sender (user, 2026-10-26)
captions          laid out as the text of a close follow-up message: full row width, the same gap below the media, the time on the caption only (D30)
                  media, chat link cards and voice at their own width and layout, as without text
rule-only message rules as wide as the copy of the time; the time beside them unless a wider quote or header widens the rule (D31)
item clip         none, so headings and emoji are drawn whole
cards             link preview and chat link in the quote colour, message corner radius
message text      onSurface at 95% in dark themes, both layouts (user, 2026-10-29)
line height       font size × 22/16 × f(w): message text, captions, quotes, code, headings, sender row, headers, event text, time (D1)
                  cards, files, voice and calls: classic until their redesign
                  f(w) = 1 − 0.1515 × (1 − w); w: L* contrast of the text colour divided by that of the message text, 0…1; names and the white time: f = 1
                  full line height for every line, one-line texts included (D7); extra space split by ascent and descent (Proportional, Trim.None)
                  slope of f(w) set by the quote (D2); light theme: message 22sp, quote 19.6sp, event text and time 15.1sp; dark theme: quote 19.9sp
spacing level 1   lines of a quote
spacing level 2   lines of a message; messages of one sender within 60 s; blocks of a message against the text next to them
spacing level 3   messages of one sender 60 s or more apart; sender name → message or header; paragraph break; above a heading, also at the start of a message (D9b)
spacing level 4   last line of a message → next sender name; above and below an event, whatever the time between them (D9a)
level targets     measured from a baseline or a block edge to the top of the tallest letter or a block edge
                  level 2: the line gap; level 3: 15.1sp; level 4: 20.8sp (D5)
                  levels and heading sizes built by multiplying base values upward
                  accepted: desktop message → event 19px (D9c)
                  all gaps: sp constants at the top of TerminalItemView.kt (D10)
                  one gap above a row, terminalRowGap: 4.75sp after a large gap, above a sender row, next to an event, above text at the item top starting with a heading; otherwise 0
                  0 below a date separator and the chat banner; 4.75sp above a date separator
                  sender row 6.5sp above, 5.5sp below; events 8.25sp above, 1.5sp below
                  text top padding: 0 below the sender row, a header, the quote, voice or a file, 5sp below media and cards; bottom 0
                  text starting with a heading below a quote, header or block: 4.75sp more above, terminalHeadingGap
                  item ending with a block (image or video without caption, chat link without text): 5sp below
                  code cards, table cards and rules: 5.75sp above, 5sp below all but the last block of a message without a meta line
                  that last block with the time beside it: 5sp below, by messageLastBlock() and TerminalMetaLayout; the time under it: 6sp below its edge
                  paragraph break and a heading inside a message: 4.75sp
                  below a heading: 2.375sp (half of 4.75sp), so a heading is nearer to its own text (D21)
                  headings: body × 1.72, 1.3125, 1 for levels 1, 2, 3 (D15, revised 2026-10-25); capitals placed as body capitals
                  files: 2sp above; voice drawn by FramedItemView (caption, quote or header): as voice alone, 4sp above, 6sp below
                  emoji-only items, also under a quote or header: 4.25sp above, 1.5sp below; not stretched to the item width
                  feature and preference icons: centred on the event text, no height; merged feature icons: one event line high, centred
                  classic: call item bottom, voice drawn without FramedItemView, the two rows of the forwarded-from header
meta width        content measured without the meta width, except text with the transparent copy, files, emoji, events and captionless media with overlayMeta
meta placement    never above the content; the row at least as tall as the time
                  after a rule with room beside it: the time on the baseline of the last text line above it (D24); otherwise under the rule
meta copy         FontWeight.Normal; icon spaces at the body1 size in dp; time and timer text at 12sp
                  conditions of CIMetaText, except that the timer is reserved for every timed message; file encryption and signature included; tall status icon condition shared with CIMetaView
                  one more space after the 17dp icons: sent and received status, via proxy (D29)
event text        layout direction in both layouts (unspecified text direction); separator and copy passed apart by ChatEventWithMeta, each view puts the separator where classic has two spaces (D28); no line limit (D23)
meta copy mark    U+200E (left-to-right layout) or U+200F (right-to-left) before the copy, so the copy stays at the line end after text in the other direction
                  U+2060 (word joiner) before the mark, so the copy stays with the last word after the spaces of an idle live message
event time        joined to the last word by no-break spaces
go-to button      search results: after the item at the row end, centred on it, as classic received items; all times of the results move left together
                  forwarded items: at the row end, centred on the item above the time line (one-line message: the forwarded line);
                  text narrowed by the arrow, the time keeps its place (user, 2026-10-25)
row tint          hover and press: indication over the whole row; selected: primary 10%; highlighted: primary 14%
colours           app, user and chat themes, as classic
wallpaper         background colour of the wallpaper, preset or custom; image omitted
theme screens     preview as terminal rows ("alice", then "bob" in the own colour), one height for both layouts
                  wallpaper presets as colours
                  custom image tile, image scale, scale slider and wallpaper accent hidden
theme export      colours and wallpaper; the layout is not exported
classic           unchanged apart from the message text colour: no copy of the time in bubbles (decided 2026-10-23), no spacing or line-height changes
build             kotlin.daemon.jvmargs=-Xmx4096m in apps/multiplatform/gradle.properties (D33)
rejected          hover action bar
                  reaction pills
                  reply line
                  date pill
                  compose field shape
                  hidden or moved timestamps
                  gutter time
                  colour-mode palette
```

## 3. Kotlin

Paths are relative to `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.

```
views/chat/ChatView.kt
  ChatView (:268-:273)                   terminal = activeChatLayout == Terminal; LocalTerminalLayout provided
  ChatLayout composable (:1010)          chatViewBackgroundModifier(drawWallpaperImage = !LocalTerminalLayout.current)
  ChatItemsList (:2460-:2466)            showAvatar = terminalItemShowsSender(item, older) when terminal
                                         terminalGap = terminalRowGap(item, older, separated, largeGap, showAvatar)
                                         separated = terminalRowsSeparated(older row) (:3817): the banner, or rowItemSeparation of the older row with a date
                                         rowItemSeparation(row) (:3812): one rule for the date separator, drawn and in the gap (D32)
  local ChatItemView (:2035)             terminal branch:
                                           ChatItemBox(top, bottom) with the row tint and terminalRowIndication; top 0
                                           bottom TERMINAL_LEVEL_STEP above a date separator; 8dp below the newest row; otherwise 0
                                           SelectedListItem when selecting
                                           TerminalChatItemRow(chatItemSender or null, terminalGutter, terminalGap, terminalStartOffset and swipeableModifier)
                                           ChatItemViewShortHand with the row interaction source
  memberNames                            public, used by chatItemSender
  classic                                sender rows and shouldShowAvatar as on master
views/chat/ChatItemsMerger.kt            MergedItem.revealed: true for Single, the flag of Grouped
views/chat/item/TerminalItemView.kt      LocalTerminalLayout, gap constants, terminalTextStyle, terminalItemShowsSender, sameSender, terminalRowGap, terminalHeadingGap
                                         row interaction, messageLastLine, messageLastBlock, inlineIcon, ItemWithMeta (endButton centred above the time line)
                                         ChatItemSender, chatItemSender, SenderNameAndRole, TerminalChatItemRow
                                         TerminalChatItemRow: no padding at the sides, the avatar at CHAT_ITEM_TEXT_PADDING; terminalGutter: member image + CHAT_ITEM_TEXT_PADDING
views/chat/item/ChatItemView.kt          alignEnd = sent && !terminal; item click without indication; reactions at the text edge
                                         GoToItemButton on the side given by alignEnd; terminal: after the item in search results
                                         and for marked deleted items, otherwise endButton of FramedItemView
                                         chatItemTailEnabled false; clipChatItem skipped
                                         hasQuoteOrHeader: the choice of EmojiItemView or CIVoiceView, and the heading gaps
views/chat/item/FramedItemView.kt        transparent background; headers and quote without background; quoteBar; mediaModifier; cardModifier; thumbnailClip; endButton to ItemWithMeta
                                         QuotedMsgOrGap: quote text box without weight fill, or a gap of CHAT_ITEM_TEXT_PADDING - QUOTE_BAR_GAP
                                         CIMarkdownText: parameter terminalTop plus terminalHeadingGap, no bottom padding, terminal text style, file encryption and signature for the copy
                                         metaReserved: passed to ItemWithMeta, bottom gap without text; captions by the classic conditions; clipChatItem skipped
                                         emoji: TERMINAL_EMOJI_* paddings, not stretched; quote row: TERMINAL_BLOCK_* paddings, icons without top padding
                                         CHAT_ITEM_TEXT_PADDING (text edge); classic vertical text padding named CHAT_ITEM_TEXT_VERTICAL_PADDING
                                         PriorityLayout(limitToPriorityWidth = !terminal || captionlessMedia), placeRelative when not limited; link preview caption without width limit; chat link card at its own width
views/chat/item/MarkdownBlocks.kt        Modifier.quoteBar, shared with Markdown quotes; Markdown quote text with terminalTextStyle and the colour rule
                                         heading scale TERMINAL_HEADING_RATIO, Modifier.headingBox with the px ratio of the font sizes
                                         spacers: TERMINAL_HEADING_BOTTOM_GAP after a heading, TERMINAL_LEVEL_STEP at a paragraph break or above a heading
                                         card margins of code, tables and rules; messageLastBlock() on the last block without a meta line (marksLastBlock)
                                         ruleMinWidth: rules of a rule-only message as wide as the copy of the time (D31)
                                         code: terminalTextStyle at the code size (classic: line height × 0.9); metaLine null in terminal rows unless live
views/chat/item/CIFileView.kt            row padding TERMINAL_FILE_TOP_GAP, icon without top padding; the reserve in the size line kept
views/chat/item/CIVoiceView.kt           voice drawn by FramedItemView: the layout of voice alone without its time; otherwise classic, start at the text edge
                                         play button and file status circle in the quote colour (user, 2026-10-26)
views/chat/item/EmojiItemView.kt         TERMINAL_EMOJI_* paddings at the text edge, messageLastLine()
views/chat/item/TextItemView.kt          messageTextColor(); terminalReserveSpaceForMeta at the end of message text
                                         meta line of a live message: encryption, signature and the outer direction, as the blocks
                                         ruleMinWidth measured from the copy of a rule-only message
views/chat/item/CIMetaView.kt            reserveSpaceForMeta with span parameters: the classic string and terminalReserveSpaceForMeta
                                         terminalReserveSpaceForMeta: word joiner and direction mark, icon spaces in dp; tallStatusIcon shared with CIMetaText
                                         time style with lineHeightColor
views/chat/item/CIChatFeatureView.kt     merged feature icons: one event line high, centred in terminal rows
views/chat/item/CIEventView.kt           chatEventPadding(), chatEventTextStyle(), ChatEventWithMeta (separator, and time or copy, apart; classic as before, D28)
                                         CIEventView without a line limit in terminal rows (D23)
item views with ItemWithMeta             FramedItemView, EmojiItemView, CIVoiceView, CICallItemView, DeletedItemView, MarkedDeletedItemView
                                         IntegrityErrorItemView, CIRcvDecryptionError, CIGroupInvitationView
item views with ChatEventWithMeta        CIChatFeatureView, CIFeaturePreferenceView, CIMemberCreatedContactView, event items of ChatItemView
views/chat/ChatInfoView.kt               ChatLayoutSection, setChatLayout; LocalTerminalLayout of the chat theme screen
views/helpers/ThemeModeEditor.kt         layoutSection slot of ChatWallpaperEditor; terminal passed to editColor
views/usersettings/Appearance.kt         ChatLayoutRow; PreviewInEitherLayout; terminal preview rows with terminalRowGap
                                         presets as colours; custom image tile, image scale, scale slider and accent hidden
                                         LocalTerminalLayout by profileLayoutIsTerminal
```

## 4. iOS

Paths are relative to `apps/ios/`.

```
SimpleXChat/ChatTypes.swift              Contact.chatLayout; ChatInfo.chatLayout, ChatInfo.uiThemes
                                         ChatInfo.defaultChatLayout, ChatInfo.activeChatLayout (profile layout passed in); ChatLayout.known, ChatLayout.displayName
                                         ChatItem.channelIsSender, textAtItemTop, startsWithHeading; CIContent.hasTerminalSender; MarkdownBlock.isHeading
SimpleXChat/Theme/ThemeTypes.swift       ThemeModeOverrides.withChatLayout: nil for overrides left empty
Shared/Model/ChatModel.swift             profileChatLayout; unknown values skipped
Shared/Views/Chat/ChatView.swift         terminal = activeChatLayout == .terminal; \.terminalLayout on the cells; visible cells rebuilt on a change
                                         wallpaper background colour without the image
                                         showAvatar = terminalItemShowsSender(item, older) when terminal
                                         terminalItemRow: SelectedChatItem, TerminalChatItemRow(chatItemSender or nil, gutter, terminalRowGap)
                                         gutter: memberImageSize + chatItemTextPadding in groups, 0 in other chats
                                         memberImageSize: 34pt × chat list profileImageSize ÷ 63 at the text size, classic rows too
                                         classic group bubble width: less scaledMemberImageSize + 8pt for the avatar column
                                         classic sender name on the bubble edge: memberImageSize + 10pt (user, I23)
                                         terminalRowsSeparated(olderRow); olderRow: the next merged item
                                         bottom: 8pt below the newest row, terminalLevelStep above a date separator, otherwise 0
                                         chatItemWithMenu: alignEnd = sent && !terminal; ChatItemClipped skipped; reactions at the text edge
                                         go-to button after the item in search results and for marked deleted items, otherwise endButton
Shared/Views/Chat/ChatItemsMerger.swift  MergedItem.revealed
Shared/Views/Chat/ChatItem/TerminalItemView.swift
                                         \.terminalLayout; gaps: Kotlin sp × body point size ÷ 16 (terminalGap)
                                         messageTextColor, terminalLineBox, TerminalLineBox view modifier; terminalItemShowsSender, sameSender, terminalRowGap, terminalHeadingGap
                                         MessageLastLine, MessageLastBlock: view modifiers with custom alignment guides; metaCopyJoiner
                                         ItemWithMeta: TerminalMetaLayout on iOS 16, HStack on the last text baseline on iOS 15
                                         ChatEventWithMeta: ts only; the separator of the classic text kept, the copy after WJ, a direction mark and two no-break spaces
                                         ChatItemSender, TerminalChatItemRow: no padding at the sides, the avatar chatItemTextPadding from the start
Shared/Views/Chat/ChatItemView.swift     endButton to FramedItemView; event items with ChatEventWithMeta; chatEventText(ts:)
Shared/Views/Chat/ChatItem/
  FramedItemView.swift                   backgrounds omitted; headers and quote at the text edge; terminalQuote with quoteBar; terminalBlock for media and cards
                                         ciMsgContentView(terminalTop:) plus terminalHeadingGap; metaPlacement; mediaLimit
                                         quote: sender at terminalNameSize; thumbnail 12pt after the quote text, or 12pt after the bar without text and sender
                                         overlayMeta for captionless media; terminal emoji paddings
  MsgContentView.swift                   messageText(textColor:); line boxes and lineSpacing, also for taps; meta line for live messages only
                                         reserveSpaceForMeta: metaCopyJoiner, three no-break spaces, ciMetaText(unbreakable: true) with file encryption and signature
                                         MarkdownBlocksView: heading gaps, terminalHeadingFont, cardMargins, marksLastBlock, ruleWidthText, ruleMinWidth, quoteBar()
                                         rule-only message: the rule widened by a wider quote or header (D31); ruleMinWidth from msgWidth
  CIMetaView.swift                       ciMetaText(unbreakable:): no-break separators, word joiners after the edit, timer and via-proxy icons
  CIFileView.swift, CIVoiceView.swift, FramedCIVoiceView.swift, CIEventView.swift, CIChatLinkHeader.swift
                                         terminal paddings; voice: received layout for both directions; chat link header without the meta reserve
  ItemWithMeta                           FramedItemView, EmojiItemView, CIVoiceView, CICallItemView, DeletedItemView, MarkedDeletedItemView,
                                         CIMsgError, CIRcvDecryptionError, CIGroupInvitationView
  ChatEventWithMeta                      CIChatFeatureView, CIFeaturePreferenceView, CIMemberCreatedContactView, event items of ChatItemView
Shared/Views/Helpers/ChatItemClipShape.swift
                                         tail off in terminal rows
Shared/Views/Helpers/ThemeModeEditor.swift
                                         ChatWallpaperEditor(layoutSection:); ChatLayoutSection; otherChatLayout
Shared/Views/Chat/ChatInfoView.swift     ChatWallpaperEditorSheet: ChatLayoutSection, setChatLayout, \.terminalLayout of the chat
Shared/Views/UserSettings/AppearanceSettings.swift
                                         ChatLayoutPicker after ThemeDestinationPicker; app destination: currentUser.uiThemes = layoutOnly()
                                         ChatThemePreview: terminal rows and classic rows in one ZStack; background colour only
                                         presets as colours; custom image tile, scale type, scale and wallpaper tint hidden
                                         \.terminalLayout by profileChatLayout
theme writes                             chatLayout kept (agent profile plan, section 6)
classic                                  message text in messageTextColor (MsgContentView); quote text in onSurface; sender name on the bubble edge; otherwise unchanged
differences                              row tint: selected only; hover and press indication omitted
                                         swipe to reply moves the item
                                         gaps × 17/16 at the default 17pt body (user, 2026-10-25); line heights from the SF font
```

## 5. Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/TerminalItemViewTest.kt`, `terminalItemShowsSender`:

- oldest item: shown
- same sender: hidden, also an hour later
- another group member; a direction change: shown
- after an event item: shown
- date change: hidden
- channel post after a member message: shown
- owner switching between posting as the channel and under the own name: shown
- integrity error: grouped with its sender
- after a deleted message of the same sender: hidden
- note folder items: grouped
- event item: no sender row

`terminalRowGap` and `terminalHeadingGap`:

- oldest item; close messages: 0
- large gap; sender row; next to an event: `TERMINAL_LEVEL_STEP`
- heading at the item top: row gap, no heading gap
- heading below a quote or a forwarded header: heading gap, no row gap
- heading not first; text without heading: neither
- separated rows: 0
- heading below a deletion, the live state, a file or media: heading gap; in unknown content without a file: row gap

`terminalRowsSeparated`: a date change of a single or revealed row, the chat banner: separated; a collapsed group, the same date, no older row: not separated

`apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/TerminalMetaCopyTest.kt`:

- classic reserve strings: unchanged, via proxy included
- copy text: word joiner and direction mark, the classic spaces, one more space after the tall status and via-proxy icons
- icon spaces: one width at font scales 0.7, 0.75, 1 and 1.3; the time scaled with the font; the marks without width
- copy after spaces, as in an idle live message: on the line of the last word
- copy after words in the other direction on a wrapped last line: at the line end, in both directions
- a message of a rule only: the rule as wide as the copy (D31)

`apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/TerminalMetaLayoutTest.kt`, `ItemWithMeta` in terminal rows:

- time baseline on the last baseline of the content
- content narrowed by the time box without `metaReserved`
- last line with room: time beside it; without room: `TERMINAL_META_GAP` under the content, the row taller
- last block at the bottom with the time beside it: `TERMINAL_BLOCK_BOTTOM_GAP` added
- content without a baseline: time `TERMINAL_META_GAP` above the content bottom
- content shorter than the time: time at the top, the row as tall as the time
- overlay meta when content and time exceed the row: at the bottom end of the content; under a wider quote, on the media corner
- overlay meta on content shorter than the time: at the top of the content, the row as tall as the time
- end button: at the row end, centred above the time box; content narrowed by it; the time at the row end
- last line with room, right-to-left

`apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/PriorityLayoutTest.kt`: terminal elements at their own width, classic elements at the media width; terminal placement at the start edge right-to-left, classic unmirrored.

`apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/ChatEventTextTest.kt`: classic separator and event text as before (D28); terminal separator of transparent no-break spaces before the copy.

Checked on desktop on 2026-10-21:

1. Direct chat, group and channel: rows, sender rows, avatars in groups, time at the row end for every item kind.
2. Spacing: close messages at the line pitch (22px).
3. Search and forwarded items: go-to button in the gutter.
4. Theme screens of a terminal chat and profile: preview, presets, hidden controls.
5. Layout choices: chat, profile and app; the open chat is redrawn at once; "Reset to app theme" keeps the chat layout; the choices are kept after a restart.

## Later

```
line heights    app-wide, in the redesign (D6)
items           link preview, chat link, files, voice and calls on the levels and line heights, in the redesign (D8)
style guide     one document for both apps with platform differences, after merge (D13)
```

## Depends on

```
agent profile plan             ChatLayout; ChatInfo.activeChatLayout; storage of the choices
block Markdown rendering       MarkdownBlocks; Modifier.quoteBar shared with Markdown quotes
```
