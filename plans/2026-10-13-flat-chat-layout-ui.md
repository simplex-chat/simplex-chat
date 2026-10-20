# Terminal chat layout: UI

Date: 2026-10-13. Revised: 2026-10-14, 2026-10-19.

Chats with the layout `terminal` are drawn in the Android, desktop and iOS apps as full-width rows without bubbles, in the colours of the colour mode, without wallpaper. Approved look: the mockup https://claude.ai/artifact/FWKgVq8mR9wqKVxpLq2WXz.

Revision 2026-10-19:

```
Kotlin rows    rows of branch sh/desktop-flat-layout, names flat -> terminal
               specification: apps/multiplatform/spec/client/chat-view.md, section 10
               product: apps/multiplatform/product/views/chat.md, "Terminal Layout"
excluded       desktop rail, compact chat list, developer flag of that branch
activation     ChatInfo.activeChatLayout == Terminal, every ChatView of the chat
colours        app, user and chat themes, as classic; to be confirmed by the user on desktop
wallpaper      omitted: drawWallpaper = false in ChatLayout
superseded     sections 2, 3 and 6 below, for Kotlin; open question 2
pending        section 4 (iOS): rewrite for the carried-over rows, after the desktop review
```

References are to branch `ep/llm-ux`, commit `31a8c86fb`; files changed by the patch 01 rework are cited in `patches/ws/layout/work` ("rework").

## 1. Scope

"Terminal" below: `ChatInfo.activeChatLayout` is `Terminal` (agent profile plan, computed by the core). Every other value is drawn as classic.

```
terminal    main chat view of a direct chat
            main chat view of a group, business chat or channel
classic     note folder
            member support chat view
            reports view
            chat item info
            compose context item
            theme preview
            full message viewer: geometry and colours (read-more plan); Format.Bold by 2.5
unchanged   chat list preview            ChatPreviewView.kt:277    ChatPreviewView.swift:302
            notification text            NtfManager.kt:134         Notifications.swift:200
            chat banner row              ChatView.kt:2402-:2418
            date separators              ChatView.kt:2243-:2245
            bottom bar of one-hand UI    ChatView.kt:1094-:1106
controls    the rework: chat theme screen, top section "Chat layout" (Kotlin ChatLayoutSection, ChatInfoView.kt:1308; iOS ChatLayoutSection, ThemeModeEditor.swift:459)
            Settings › Appearance › Themes, row "Chat layout" (Kotlin ChatLayoutRow, Appearance.kt:897; iOS ChatLayoutPicker, AppearanceSettings.swift:878)
```

## 2. Specification

### 2.1. Rows

```
received, first row of a sender group
    (A) Alice  admin  10:42
    Text of the message, laid out over the content width and
    wrapped at the end padding.                              [meta]
    [reaction 2] [reaction]
sent, first row of a sender group
    (U) you  10:43
    Own message: same structure, start-aligned.                [vv]
grouped row
    Next message of the same sender.                     [v 10:44]

(A), (U)                   avatars
[meta], [vv], [v 10:44]    CIMetaView: delivery status as now; time by showTimestamp (2.3)
```

### 2.2. Sender line

```
shown     first row of a sender group (2.3)
name      after the avatar; one line, ellipsis, medium weight, text colour
badge     after the name, as now
role      member: role text when above member, as now
          group: "channel" in a channel, "group" otherwise
time      time text of the item, secondary colour
tap       member: member info
          contact, group: chat info

item                                         sender    name                  avatar
directRcv                                    contact   contact display name  contact image
directSnd                                    user      "you"                 user profile image
groupRcv                                     member    member display name   member image
groupSnd                                     user      "you"                 membership image
groupRcv, groupSnd with showGroupAsSender    group     group name            group image
channelRcv                                   group     group name            group image
```

### 2.3. Sender groups

```
showSender(item, older) =
  item.content.showSender
  && ( older == nil
     || !older.content.showSender
     || !sameSender(item, older)
     || |item.meta.itemTs - older.meta.itemTs| >= 60 s
     || item.meta.itemTs and older.meta.itemTs are on different calendar days )

sameSender(a, b) = the sender column of 2.2 is equal for a and b; for two members, groupMemberId is equal

showTimestamp(item, older) = !showSender(item, older) && (older == nil || item time text != older time text)

content.showSender =
  showMemberName (ChatModel.kt:4356, ChatTypes.swift:4825), except rcvIntegrityError
  || sndMsgContent
  || sndDeleted
  || sndCall
  || sndGroupInvitation
  || sndModerated
```

- `item`: the newest item of the list row. `older`: the item before its oldest item, as for `shouldShowAvatar` (`ChatView.kt:2427`, `ChatView.swift:1777`).
- An integrity error row is drawn without sender line; a sender group is ended by it.

### 2.4. Geometry and colours

```
                             Kotlin                                    iOS
row start and end padding    12 dp                                     16
content width                row width - 24 dp                         row width - 32
row top padding              8 dp with sender line, otherwise 0        same
row bottom padding           2 dp; newest row 8 dp                     same
avatar                       20 dp x fontSizeSqrtMultiplier; 6 dp gap  20; gap 6
name, time                   13.5 sp, 12 sp                            .footnote, .caption
sender line to content       2 dp                                      2
text padding                 vertical 2 dp, horizontal 0               same
meta padding                 bottom 2 dp, end 0                        same
landscape image, video       min(500 dp, content width)                min(500, content width)
portrait image, video        0.75 x min(500 dp, window - 100 dp)       0.75 x min(500, content width)
```

```
colours       the palette of the colour mode: light, dark, simplex or black (DefaultTheme, Theme.kt:26-27)
              colours of the app, user and chat themes and of wallpaper presets are not applied; colour schemes later
              every view of a terminal chat: toolbar, rows and compose of the main chat view, member support chat and reports
background    background colour of the palette; wallpaper image omitted
text          onBackground; secondary text: secondary; links: primary
cards         quote, code, table header, link preview, chat link: receivedQuote of the palette, corners 18 x roundness
media         corners 18 x roundness
tails         omitted
```

### 2.5. Content

```
class       item                                       drawing; menu and tap area
plain       message content drawn by FramedItemView    without frame, background and clip; area: content box over the content width
own size    emoji only                                 as now, start-aligned; area: the view
            voice without text
            call
framed      collapsed deleted                          as now, start-aligned, tail omitted; area: the frame
            deleted
            group invitation
            decryption error
            message error
            integrity error
            invalid data
other       every other item                           as now, start-aligned

plain item part            drawing
text                       over the content width; bounded by 2.7
*x* (Format.Bold)          italic in the text and in the quote text of a terminal row, and in the viewer opened from it
                           bold in the chat list preview, compose context item, item info and notification text
**x** (Format.Strong)      bold in both layouts (block Markdown plan)
meta                       bottom-end corner of the content box; space reserved at the text end, as now
                           image or video without text: over the bottom-end corner of the media, white, as now
                           last block is code, a table or a rule: the meta line of the rendering plan (2.4)
                           part of CIMetaView in both layouts: the bell of the message priority plan, the partsMissing mark of the message parts plan
reactions                  below the content, start-aligned
quote                      card above the content; tap as now
item header                every kind, LIVE included: as now, without background
image, video               start-aligned; text below over the content width
file, voice with text      as now
link preview, chat link    card; tap as now
emoji after a header       start-aligned
```

### 2.6. Interaction

```
behaviour                 rule                                                            Kotlin                                    iOS
long press, right click   menu as now; area by class (2.5)                                combinedClickable (ChatItemView.kt:327)   contextMenu (ChatView.swift:2148)
swipe to reply            Android, iOS: sender line and content are moved                 swipeableModifier (ChatView.kt:1987)      swipeOffset (ChatView.swift:2159)
selection mode            row with checkmark: content inset by the checkmark width        TerminalItemRow (3)                       terminalItemRow (4)
                          the inset is applied without animation; text is laid out again
scrolled-to highlight     hover indication over the menu area, as now                     LaunchedEffect (ChatItemView.kt:316)
go-to button              at the row end, for sent and received items                     GoToItemButton (ChatItemView.kt:807)      goToItemButton (ChatView.swift:2151)
desktop text selection    sender line excluded; text layouts by 2.7                       setupItemSelection (TextSelection.kt:437)
right-to-left             row elements are mirrored by the app direction                  TerminalItemRow, TerminalSenderLine (3)   terminalItemRow, terminalSenderLine (4)
                          text is laid out over the content width in its own direction    MarkdownText (TextItemView.kt:153)        ciMsgContentView (FramedItemView.swift:359)
                          meta line rule for mixed directions as now                      MarkdownText (TextItemView.kt:156)        MsgContentView.swift:135
```

### 2.7. Text size

The text of a sent or received item is at most `maxMsgParts × maxEncodedMsgLength` = 124,816 bytes (message parts plan). A terminal row is bounded by cost and by units of display text (read-more plan, "Teaser rule"):

```
MSG_TERMINAL_MAX_LINES = 500       an item with a higher cost, or with more than MSG_TERMINAL_MAX_LINES × MSG_LINE_CHARS units, is drawn as a teaser with "Read more"
MSG_TERMINAL_TEASER_LINES = 400    cost of the teaser; at most MSG_TERMINAL_TEASER_LINES × MSG_LINE_CHARS units; a live item is truncated above these limits
teaser, viewer                     msgTeaser, showFullMessage (read-more plan)
list of paragraphs only            one text layout
other block list                   MsgBlocksView of the rendering plan, at most MSG_MAX_BLOCKS blocks (equal to MSG_TERMINAL_MAX_LINES)
```

## 3. Kotlin

Paths are relative to `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`. The `Format.Bold` line of `MarkdownText` is applied after part 1 of the rendering plan (`Format.Strong`).

```
ui/theme/ThemeManager.kt
  colorModeTheme(), new, after currentColors (:57)
                                     ActiveTheme of the palette of CurrentColors.value.base, as :62-66; wallpaper AppWallpaper(type = WallpaperType.Empty)
views/chat/ChatView.kt
  ChatView (:274-:278)               terminal chat: SimpleXThemeOverride(ThemeManager.colorModeTheme())
  LocalChatLayout                    staticCompositionLocalOf<ChatLayout> { ChatLayout.Classic }
  ChatLayout composable (:918)       value: chatInfo.value?.activeChatLayout (:974) when chatsCtx.secondaryContextFilter == null; otherwise Classic
                                     provided at :1021
                                     drawWallpaper (:1003): false when terminal
  terminalShowSender, sameSender     file functions after shouldShowAvatar (:3787); rules 2.3
  itemsIndexed (:2427-:2439)         terminal, with older = merged.oldest().nextItem:
                                       showAvatar = terminalShowSender(item, older)
                                       itemSeparation.copy(timestamp = !showAvatar && item.meta.timestampText != older?.meta?.timestampText)
  local ChatItemView (:1972)         Box (:2021): TerminalItemRow when terminal; the existing branches otherwise
  TerminalItemRow, new               Column(Modifier.fillMaxWidth()) with the paddings of 2.4
                                     swipeableModifier (:1987) unless selectionVisible
                                     TerminalSenderLine when showAvatar
                                     ChatItemViewShortHand(cItem, itemSeparation, range, swipeOffset = dismissState.offset.value)
                                     selectionVisible (:2023): start padding + 4.dp + 22.dp * fontSizeMultiplier; SelectedListItem as at :2107
  TerminalSenderLine, new            Row with the parts of 2.2
                                     merged row: getPrevHiddenMember, memberNames, as :2043-:2052
                                     showGroupAsSender: group name and image; new in Kotlin, as ChatView.swift:2007-:2012
                                     tap: showMemberInfo (:2086), showChatInfo (:2164)
                                     "you": MR.strings.sender_you_pronoun; "channel": channel_role_label; "group": group_role_label, new
                                     RcvDecryptionError: time omitted; it is drawn in the frame (CIRcvDecryptionError.kt:179, :208)
model/ChatModel.kt                   CIContent.showSender, after showMemberName (:4356)
views/chat/item/ChatItemView.kt, when terminal
  alignment (:120, :304)             start
  GoToItemButton                     :311-:313: skipped; :806-:808: composed for every item
  clickable Column (:323-:335)       clipChatItem omitted for an item with message content that is not collapsed deleted
  CIVoiceView (:578)                 sent = false
  tail                               chatItemOffset (:1207), clipChatItem (:1223): disabled
views/chat/item/FramedItemView.kt, when terminal
  sent (:52)                         false
  outer Box (:234)                   clipChatItem and background omitted
  content (:253-:429)                moved to a local composable Content; return@PriorityLayout (:368, :376) is replaced with return
                                     terminal: Column(Modifier.fillMaxWidth()) { Content() }; otherwise as now
  headerModifier (:120)              background omitted; start padding 0
  ciQuoteView (:155)                 receivedQuote background; clipChatItem()
  emoji after a header (:349-:364)   start-aligned; horizontal padding 0
  CIImageView, CIVideoView           in Box(Modifier.clipChatItem()); without text: CIMetaView in that Box, white
  ChatItemLinkView (:397)            in Box with receivedQuote background and clipChatItem(); the same Box for CIChatLinkHeader (:412)
  text under a link preview (:398)   widthIn limit omitted
  CIMetaView (:431)                  padding bottom 2.dp, end 0; skipped for image or video without text
  CIMarkdownText (:462)              padding vertical 2.dp, horizontal 0; MarkdownText modifier fillMaxWidth
views/chat/item/TextItemView.kt
  MarkdownText                       new parameter chatLayout: ChatLayout = ChatLayout.Classic
                                     passed LocalChatLayout.current from CIMarkdownText (FramedItemView.kt:463) and ciQuotedMsgTextView (:61)
  Format.Bold (:232)                 style of Format.Italic when chatLayout is Terminal
views/helpers/ThemeModeEditor.kt (rework)
  ChatWallpaperEditor (:235)         new parameter terminal: Boolean, passed by ChatWallpaperEditorModal (ChatInfoView.kt:1297) from activeChatLayout
                                     terminal: after layoutSection() (:254) SectionTextFooter(chat_layout_no_wallpaper); WallpaperPresetSelector (:347-:374) alpha 0.3, taps ignored
views/chat/ChatInfoView.kt (rework)  setSendReceipts (:84-:86), MuteButton (:904-:922): ChatInfo.Direct of the current contact (:68, parameter contact), in place of chat.chatInfo of the remembered chat (:69)
moko-resources/base/strings.xml      group_role_label: "group"
                                     chat_layout_no_wallpaper: "Wallpapers are not shown in the terminal layout."
```

## 4. iOS

Paths are relative to `apps/ios/Shared/`. `case .bold` of `messageText` is applied in the order of section 3.

```
Theme/ThemeManager.swift
  colorModeTheme(), new, after currentColors (:64)
                                           ActiveTheme of the palette of CurrentColors.base, as :70-73; wallpaper .empty
Views/Chat/ChatItemView.swift
  EnvironmentValues.chatLayout (:12-:39)   new key, default .classic
  framedItemView (:102)                    terminal: mediaMaxWidth = min(maxWidth, 500), passed to FramedItemView; imgWidth, videoWidth derived from it
Views/Chat/ChatView.swift
  buildTheme (:3091-:3110)                 terminal chat: AppTheme of ThemeManager.colorModeTheme()
  viewBody (:123)                          wallpaper modifier skipped when terminal
                                           .onChange(of: chat.chatInfo.activeChatLayout): theme = buildTheme(); scrollView.updateItems(mergedItems.boxedValue.items, true)
                                           .onChange(of: selectedChatItems == nil), terminal: the same updateItems call
  chatItemsList (:902)                     inside the cell closure: chatLayout = chat.chatInfo.activeChatLayout when im.secondaryIMFilter == nil; otherwise .classic
                                           .environment(\.chatLayout, chatLayout) at :958
                                           maxWidth (:926): g.size.width - 32 when terminal
  terminalShowSender, sameSender           file functions after shouldShowAvatar (:17); rules 2.3
  ChatItemWithMenu.body (:1777-:1782)      terminal: showAvatar = terminalShowSender(item, merged.oldest().nextItem); itemSeparation.timestamp by showTimestamp (2.3)
  chatItemListView (:1921)                 first branch when terminal: terminalItemRow
  terminalItemRow, new                     VStack(alignment: .leading, spacing: 2): terminalSenderLine when showAvatar
                                           HStack: SelectedChatItem when selecting, as :1966; chatItemWithMenu
                                           paddings of 2.4; maxWidth less 36 when selecting
  terminalSenderLine, new                  HStack with the parts of 2.2; names and taps as :2001-:2070
                                           "you", "channel" (:1939) and "group" (:2009): existing keys
                                           .offset(x: swipeOffset)
  chatItemWithMenu (:2121), terminal       alignment (:2122): .leading
                                           goToItemButton(true) (:2134): skipped; goToItemButton(false) (:2151): for every item
                                           ChatItemClipped (:2147): tail omitted; plain for an item with message content that is not collapsed deleted
../SimpleXChat/ChatTypes.swift             CIContent.showSender, after showMemberName (:4825)
Views/Chat/ChatItem/FramedItemView.swift, when terminal
  mediaMaxWidth                            new property, default maxWidth
  background (:127)                        theme.colors.background; clear for image or video without text, as now
  maxMediaWidth() (:397)                   nil: the limits at :252, :309, :382 are skipped
  content VStack (:34)                     .frame(maxWidth: .infinity, alignment: .leading); image or video without text: media width
  headerFrame (:243)                       background omitted; horizontal padding 0
  ciQuoteView (:259)                       receivedQuote background, rounded clip, full width
  emoji after a header (:145-:154)         horizontal padding 0
  ciMsgContentView (:359)                  padding vertical 2, horizontal 0
                                           .frame(maxWidth: .infinity, alignment: rtl ? .trailing : .leading)
                                           containerBackground (:375): theme.colors.background
  CIMetaView (:115)                        horizontal padding 0, bottom 2; image or video without text: as now
  CIImageView, CIVideoView                 maxWidth: mediaMaxWidth; rounded clip
  CILinkView, CIChatLinkHeader             maxWidth: mediaMaxWidth; receivedQuote background; rounded clip
Views/Chat/ChatItem/CIVoiceView.swift (:39)    terminal: leading arrangement for every item
Views/Helpers/ChatItemClipShape.swift      ChatItemClipped: new initialiser parameter plain: Bool; plain: .roundRect(radius: 0)
Views/Chat/ChatItem/MsgContentView.swift
  messageText (:396)                       new parameter chatLayout: ChatLayout = .classic
                                           case .bold (:450): the italic font of case .italic when .terminal
  MsgContentView                           @Environment(\.chatLayout), passed to messageText (:100)
Views/Helpers/ThemeModeEditor.swift (rework)
  ChatWallpaperEditor (:216)               terminal (chat.chatInfo.activeChatLayout): Section footer "Wallpapers are not shown in the terminal layout." after ChatLayoutSection (:243)
                                           WallpaperPresetSelector (:245-:259): .disabled(true), .opacity(0.3)
```

- The constants of 2.7 are defined next to the read-more constants: Kotlin `TextItemView.kt`, iOS `MsgContentView.swift`.

## 5. Docs

```
terminal rows                apps/multiplatform/spec/client/chat-view.md, "Item Layout" (:124-:129)
                             apps/ios/spec/client/chat-view.md, after "Bubble Direction" (:139); terminalShowSender at :160-:161
                             apps/multiplatform/product/views/chat.md
                             apps/ios/product/views/chat.md
colour mode, no wallpaper    apps/multiplatform/spec/services/theme.md
                             apps/ios/spec/services/theme.md
```

## 6. Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/TerminalSenderTest.kt`, `terminalShowSender`:

- first item of the chat: true
- same sender after 59 s: false; after 60 s: true
- another member; a member after an own item: true
- same sender after an event item: true
- same sender on the next calendar day: true
- `channelRcv` after `groupSnd` with `showGroupAsSender`: false
- `groupSnd` with `showGroupAsSender` after `groupSnd` without it: true
- integrity error item: false; the next item of the same sender: true

Manual checks, Android, desktop and iOS:

1. Direct chat, group, business chat, channel with `terminal`: rows as 2.1 and the mockup; each item of 2.5; time in the meta of a grouped row with another minute; `*x*` italic and `**x**` bold.
2. Colour modes light, dark, simplex and black: the palette colours of 2.4 in toolbar, rows and compose; a chat with a chat theme and wallpaper: neither is applied; the same chat in classic: both are applied.
3. Chat theme screen of a terminal chat: the note and the disabled presets; of a classic chat: as now.
4. Channel owner: own posts and received posts under one sender line with the channel name.
5. Texts of 124,816 bytes, final and live: plain lines; one code line of 124,816 units; one table row of 124,816 units. Each row is a teaser within the limits of 2.7; frame time while the row is scrolled and updated is at most that of a 15,602-byte item in the classic layout on the same device. Its viewer: classic colours, `*x*` italic.
6. Image without text in a wide desktop window: meta over the image corner. Desktop text selection over several rows, copy: sender names excluded.
7. Selection mode in left-to-right and right-to-left apps: inset, text wrapped again; iOS row heights correct after entering and leaving.
8. Swipe to reply; menu by long press and right click on the empty part of a plain row; go-to button on every row in search and content filter mode.
9. Right-to-left text in a left-to-right app and the reverse; one-hand UI with the bottom bar.
10. Layout changed by each control of the rework: the open chat is drawn again at once; iOS row heights correct. Kotlin contact info: layout changed, then mute, then send receipts: the layout of the open chat is kept.
11. High priority item: bell in the meta of a terminal row and of a classic bubble.
12. Note folder, member support chat, reports, chat item info, chat list preview, notifications: unchanged.

## Interfaces

```
Kotlin           LocalChatLayout: ProvidableCompositionLocal<ChatLayout>, default Classic
                 MarkdownText(chatLayout)
                 ThemeManager.colorModeTheme()
iOS              EnvironmentValues.chatLayout, default .classic
                 messageText(chatLayout:)
                 FramedItemView.mediaMaxWidth
                 ThemeManager.colorModeTheme()
both             MSG_TERMINAL_MAX_LINES = 500
                 MSG_TERMINAL_TEASER_LINES = 400

Boundary with the rendering plan
this plan        the row around the message and its layout value (sections 2-4)
                 the chatLayout parameter of MarkdownText and messageText, used for Format.Bold
rendering plan   the views drawn inside the message for blocks, in both layouts
                 horizontal scrolling of a block and its relation to the reply swipe (ChatView.kt:1987, ChatView.swift:2161)
                 text layouts per item and desktop selection over them (TextSelection.kt:437)
contract         maximum block width: the content width of 2.4
                 the content ends with the meta reserve of reserveSpaceForMeta (CIMetaView.kt:130), placed by the "meta" row of 2.5
                 block views in an item: at most MSG_MAX_BLOCKS
```

## Depends on

```
agent profile plan    ChatLayout: Classic, Terminal, Unknown(type); iOS: classic, terminal, unknown
                      ChatInfo.activeChatLayout, computed by the core; the controls of section 8 of that plan
                      ChatWallpaperEditor(layoutSection), ChatLayoutSection: rework
read-more plan        msgTeaser with the limits of 2.7; showFullMessage, with the layout of the chat view of the tap
                      "Shared code": the combined edits of the functions changed by this plan, that plan and the rendering plan; their order
rendering plan        part 1 (release 1): Format.Strong; markdownSegments; MsgBlocksView in part 2
block Markdown plan   Strong for **x**, bold in both layouts; Bold for *x*
message priority plan the bell in CIMetaView, both layouts
message parts plan    text of a sent or received item: at most 124,816 bytes; partsMissing in CIMetaView
```

The `Format.Bold` rule is in release 1, with `Strong` of the core parser and part 1 of the rendering plan.

## Open questions

1. "Read more" in the terminal layout: the product owner said that the terminal layout does not need read more. A terminal row above 500 lines (16,000 characters on a phone, 40,000 on desktop) or above `MSG_MAX_BLOCKS` blocks is a teaser; smaller texts are drawn in full. Recommended: keep these limits: one row of 124,816 bytes is one lazy item, laid out as a whole.
2. Colours of a terminal chat: the palette of the colour mode in every view of the chat (toolbar, rows, compose; member support chat and reports included); the colours of app, user and chat themes and of wallpaper presets are not applied. Recommended: yes; colour schemes later.
3. Chat theme screen of a terminal chat: in the mockup, the note and disabled wallpaper presets are shown, and the chat colours, unused by terminal rows, are kept. Recommended: as in the mockup; the chat colours are applied when the chat is switched to classic.
4. `*x*` italic in terminal rows: in the item text, in quotes of terminal rows and in the viewer opened from them; bold in previews, compose context, item info and notifications. Recommended: as listed.
5. Own rows: in the mockup they are named "you"; in groups this replaces the membership name. Recommended: "you" in every chat.
6. Maximum text of one item: 8 parts (124,816 bytes, 16 items per recipient queue) or 32 parts (499,264 bytes, 4 items). Recommended: 8.
