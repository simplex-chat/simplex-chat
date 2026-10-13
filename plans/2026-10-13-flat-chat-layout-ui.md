# Flat chat layout: UI

Chats with the effective layout `flat` are drawn in the Android, desktop and iOS apps as full-width rows without bubbles; the layout is overridden per chat in the chat info and group info screens.

## 1. Scope

"Flat" below: `ChatInfo.chatLayout` (agent-profile plan) is `Flat`. Every other value is drawn as bubbles.

```
flat        main chat view of a direct chat
            main chat view of a group, business chat or channel
bubbles     note folder
            member support chat view
            reports view
            chat item info
            compose context item
            theme preview
            full message viewer: geometry and colours; Format.Bold by 2.5
unchanged   chat list preview            ChatPreviewView.kt:277    ChatPreviewView.swift:302
            notification text            NtfManager.kt:134         Notifications.swift:200
            chat banner row              ChatView.kt:2402-:2418
            date separators              ChatView.kt:2243-:2245
            bottom bar of one-hand UI    ChatView.kt:1094-:1106
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

item                                         sender    name                       avatar
directRcv                                    contact   contact display name       contact image
directSnd                                    user      "you"                      user profile image
groupRcv                                     member    member display name        member image
groupSnd                                     user      membership display name    membership image
groupRcv, groupSnd with showGroupAsSender    group     group name                 group image
channelRcv                                   group     group name                 group image
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
  showMemberName (ChatModel.kt:4316, ChatTypes.swift:4775), except rcvIntegrityError
  || sndMsgContent
  || sndDeleted
  || sndCall
  || sndGroupInvitation
  || sndModerated
```

- `item`: the newest item of the list row. `older`: the item before its oldest item, as for `shouldShowAvatar` (ChatView.kt:2427, ChatView.swift:1777).
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

background    theme background colour; the wallpaper image is omitted
chat theme    editor and its preview: unchanged; the wallpaper of a flat chat is stored
cards         quote, link preview, chat link: receivedQuote background, corners 18 x roundness
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
*x* (Format.Bold)          italic in message text and in quote text; release: "Depends on"
                           viewer of a flat chat: italic; the chatLayout of the chat is passed by showFullMessage (open question 3)
meta                       bottom-end corner of the content box; space reserved at the text end, as now
                           image or video without text: over the bottom-end corner of the media, white, as now
                           last block is code, a table or a rule: the meta line of the rendering plan, 2.4
                           partsMissing mark (message-parts plan, section 8): part of CIMetaView in both layouts
reactions                  below the content, start-aligned
quote                      card above the content; tap as now
item header                every kind: as now, without background
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
selection mode            row with checkmark: content inset by the checkmark width        FlatItemRow (3)                           flatItemRow (4)
                          the inset is applied without animation; text is laid out again
scrolled-to highlight     hover indication over the menu area, as now                     LaunchedEffect (ChatItemView.kt:316)
go-to button              at the row end, for sent and received items                     GoToItemButton (ChatItemView.kt:807)      goToItemButton (ChatView.swift:2151)
desktop text selection    sender line excluded; text layouts by 2.7                       setupItemSelection (TextSelection.kt:437)
right-to-left             row elements are mirrored by the app direction                  FlatItemRow, FlatSenderLine (3)           flatItemRow, flatSenderLine (4)
                          text is laid out over the content width in its own direction    MarkdownText (TextItemView.kt:153)        ciMsgContentView (FramedItemView.swift:359)
                          meta line rule for mixed directions as now                      MarkdownText (TextItemView.kt:156)        MsgContentView.swift:135
```

### 2.7. Text size

The text of a sent or received item is at most `maxMsgParts × maxEncodedMsgLength` = 124,816 bytes (message-parts plan, "Interfaces"). A flat row is bounded by cost and by units of display text:

```
MSG_FLAT_MAX_LINES = 500       an item with a higher cost, or with more than MSG_FLAT_MAX_LINES × MSG_LINE_CHARS units of display text, is drawn as a teaser with "Read more"
MSG_FLAT_TEASER_LINES = 400    cost of the teaser; the teaser has at most MSG_FLAT_TEASER_LINES × MSG_LINE_CHARS units; a live item is truncated above the limits of the teaser
cost                           lines of MSG_LINE_CHARS: 32 on Android and iOS, 80 on desktop (message-read-more plan)
teaser                         msgTeaser (message-read-more plan, "Teaser rule")
viewer                         showFullMessage (message-read-more plan, "Viewer")
list without markers           one text layout
list with markers              the block views of the rendering plan (2.2, 2.6, 5), at most `MSG_MAX_BLOCKS` per item (rendering plan, 2.2; equal to `MSG_FLAT_MAX_LINES`); the teaser is cut at `MSG_MAX_BLOCKS` blocks (rendering plan, 2.2)
```

## 3. Kotlin

Paths are relative to `views/chat`. The `Format.Bold` line of `MarkdownText` is applied after part 1 of the rendering plan (`Format.Strong`, markers), in the release of "Depends on". The other edits of the functions of "Shared code" (message-read-more plan) are applied before that plan.

```
ChatView.kt
  LocalChatLayout                    staticCompositionLocalOf<ChatLayout> { ChatLayout.Bubbles }
  ChatLayout composable (:918)       value: chatInfo.value?.chatLayout (:974) when chatsCtx.secondaryContextFilter == null; otherwise Bubbles
                                     provided at :1021
                                     drawWallpaper (:1003): false when flat
  flatShowSender, sameSender         file functions after shouldShowAvatar (:3787); rules 2.3
  itemsIndexed (:2427-:2439)         flat, with older = merged.oldest().nextItem:
                                       showAvatar = flatShowSender(item, older)
                                       itemSeparation.copy(timestamp = !showAvatar && item.meta.timestampText != older?.meta?.timestampText)
  local ChatItemView (:1972)         Box (:2021): FlatItemRow when flat; the existing branches otherwise
  FlatItemRow, new                   Column(Modifier.fillMaxWidth()) with the paddings of 2.4
                                     swipeableModifier (:1987) unless selectionVisible
                                     FlatSenderLine when showAvatar
                                     ChatItemViewShortHand(cItem, itemSeparation, range, swipeOffset = dismissState.offset.value)
                                     selectionVisible (:2023): start padding + 4.dp + 22.dp * fontSizeMultiplier; SelectedListItem as at :2107
  FlatSenderLine, new                Row with the parts of 2.2
                                     merged row: getPrevHiddenMember, memberNames, as :2043-:2052
                                     showGroupAsSender: group name and image; new in Kotlin, as ChatView.swift:2007-:2012
                                     tap: showMemberInfo (:2086), showChatInfo (:2164)
                                     RcvDecryptionError: time omitted; it is drawn in the frame (CIRcvDecryptionError.kt:179, :208)
../../model/ChatModel.kt             CIContent.showSender, after showMemberName (:4316)
item/ChatItemView.kt, when flat
  alignment (:120, :304)             start
  GoToItemButton                     :311: skipped; :806: composed for every item
  clickable Column (:325)            clipChatItem omitted for an item with message content that is not collapsed deleted
  CIVoiceView (:578)                 sent = false
  tail                               chatItemOffset (:1208), clipChatItem (:1225): disabled
item/FramedItemView.kt, when flat
  sent (:52)                         false
  outer Box (:234)                   clipChatItem and background omitted
  content (:253-:429)                moved to a local composable Content; return@PriorityLayout (:368, :376) is replaced with return
                                     flat: Column(Modifier.fillMaxWidth()) { Content() }; otherwise as now
  headerModifier (:120)              background omitted; start padding 0
  ciQuoteView (:155)                 clipChatItem()
  emoji after a header (:349-:364)   start-aligned; horizontal padding 0
  CIImageView, CIVideoView           in Box(Modifier.clipChatItem()); without text: CIMetaView in that Box, white
  ChatItemLinkView (:397)            in Box with receivedQuote background and clipChatItem(); the same Box for CIChatLinkHeader (:412)
  text under a link preview (:398)   widthIn limit omitted
  CIMetaView (:431)                  padding bottom 2.dp, end 0; skipped for image or video without text
  CIMarkdownText (:462)              padding vertical 2.dp, horizontal 0; MarkdownText modifier fillMaxWidth
item/TextItemView.kt
  MarkdownText                       new parameter chatLayout: ChatLayout = ChatLayout.Bubbles
                                     passed LocalChatLayout.current from CIMarkdownText (FramedItemView.kt:463) and ciQuotedMsgTextView (:61)
  Format.Bold (:232)                 style of Format.Italic when chatLayout is Flat; release: "Depends on"
  constants of 2.7                   new
```

## 4. iOS

Paths are relative to `Shared/Views/Chat`. `case .bold` of `messageText` and the other edits of the functions of "Shared code" are applied in the order of section 3.

```
ChatItemView.swift
  EnvironmentValues.chatLayout (:12-:39)   new key, default .bubbles
  framedItemView (:102)                    flat: mediaMaxWidth = min(maxWidth, 500), passed to FramedItemView; imgWidth, videoWidth derived from it
ChatView.swift
  viewBody (:123)                          wallpaper modifier skipped when flat
                                           .onChange(of: chat.chatInfo.chatLayout): scrollView.updateItems(mergedItems.boxedValue.items, true)
                                           .onChange(of: selectedChatItems == nil), flat: the same call
  chatItemsList (:902)                     inside the cell closure: chatLayout = chat.chatInfo.chatLayout when im.secondaryIMFilter == nil; otherwise .bubbles
                                           .environment(\.chatLayout, chatLayout) at :958
                                           maxWidth (:926): g.size.width - 32 when flat
  flatShowSender, sameSender               file functions after shouldShowAvatar (:17); rules 2.3
  ChatItemWithMenu.body (:1777-:1782)      flat: showAvatar = flatShowSender(item, merged.oldest().nextItem); itemSeparation.timestamp by showTimestamp (2.3)
  chatItemListView (:1921)                 first branch when flat: flatItemRow
  flatItemRow, new                         VStack(alignment: .leading, spacing: 2): flatSenderLine when showAvatar
                                           HStack: SelectedChatItem when selecting, as :1966; chatItemWithMenu
                                           paddings of 2.4; maxWidth less 36 when selecting
  flatSenderLine, new                      HStack with the parts of 2.2; names and taps as :2001-:2070
                                           .offset(x: swipeOffset)
  chatItemWithMenu (:2121), flat           alignment (:2122): .leading
                                           goToItemButton(true) (:2134): skipped; goToItemButton(false) (:2151): for every item
                                           ChatItemClipped (:2147): tail omitted; plain for an item with message content that is not collapsed deleted
apps/ios/SimpleXChat/ChatTypes.swift       CIContent.showSender, after showMemberName (:4775)
ChatItem/FramedItemView.swift, when flat
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
ChatItem/CIVoiceView.swift (:39)           flat: leading arrangement for every item
../Helpers/ChatItemClipShape.swift         ChatItemClipped: new initialiser parameter plain: Bool; plain: .roundRect(radius: 0)
ChatItem/MsgContentView.swift
  messageText (:396)                       new parameter chatLayout: ChatLayout = .bubbles
                                           case .bold (:450): the italic font of case .italic when .flat; release: "Depends on"
  MsgContentView                           @Environment(\.chatLayout), passed to messageText (:100)
  constants of 2.7                         new
```

## 5. Override control

```
title      Chat layout
values     default (<profile layout>)
           bubbles
           flat
selected   chatLayoutOverride of the contact or group, read from the model on each draw; Bubbles or Flat, otherwise default
default    profile.chatLayout of the contact, groupProfile.chatLayout of the group, when Bubbles or Flat; otherwise bubbles
written    setChatLayout (agent-profile plan): /_layout <chatRef> bubbles|flat|default, then the model
shown      chat info screen and group info screen, enabled for every chat

ChatInfoView.kt                 ChatLayoutOption(profileLayout, override, onSelected): ExposedDropDownSettingRow<ChatLayout?>, icon ic_article
                                ChatInfoLayout, SectionView (:593): before WallpaperButton (:608)
                                onSelected: setChatLayout(chat.remoteHostId, chat.chatInfo, layout, chatModel)
                                setSendReceipts (:84-:86), MuteButton (:920): contact.chatSettings and ChatInfo.Direct(contact), in place of the remembered chat.chatInfo (:69)
group/GroupChatInfoView.kt      GroupChatInfoLayout, SectionView (:732): before WallpaperButton (:740); the same onSelected
strings.xml (base)              chat_layout: "Chat layout"
                                chat_layout_bubbles: "bubbles"
                                chat_layout_flat: "flat"
                                group_role_label: "group" (sender line)
                                default label: chat_preferences_default
ChatInfoView.swift              chatLayoutOption(): WrappedPicker over [nil, .bubbles, .flat], label as sendReceiptsOption (:497)
                                selection: Binding(get: override of chat.chatInfo.contact, set: setChatLayout(chat, layout:))
                                new Section after the Section of :185-:206
Group/GroupChatInfoView.swift   the same option for chat.chatInfo.groupInfo; Section (:192): before the "Chat theme" link (:200)
iOS strings                     "Chat layout", and the values above with "default (bubbles)" and "default (flat)"
```

## 6. Docs

```
flat rows                  apps/multiplatform/spec/client/chat-view.md, after :127
                           apps/ios/spec/client/chat-view.md, after "Bubble Direction" (:139); flatShowSender at :160-:161
                           apps/multiplatform/product/views/chat.md
                           apps/ios/product/views/chat.md
Chat layout control        apps/multiplatform/product/views/contact-info.md
                           apps/multiplatform/product/views/group-info.md
                           apps/ios/product/views/contact-info.md
                           apps/ios/product/views/group-info.md
wallpaper of a flat chat   apps/multiplatform/spec/services/theme.md
```

## 7. Tests

`apps/multiplatform/common/src/commonTest/kotlin/chat/simplex/app/FlatSenderTest.kt`, `flatShowSender`:

- first item of the chat: true
- same sender after 59 s: false; after 60 s: true
- another member; a member after an own item: true
- same sender after an event item: true
- same sender on the next calendar day: true
- `channelRcv` after `groupSnd` with `showGroupAsSender`: false
- `groupSnd` with `showGroupAsSender` after `groupSnd` without it: true
- integrity error item: false; the next item of the same sender: true

Manual checks, Android, desktop and iOS:

1. Direct chat, group, business chat, channel with `flat`: rows as 2.1; each item of 2.5; plain background in a chat with wallpaper; time in the meta of a grouped row with another minute. `*x*` italic and `**x**` bold.
2. Channel owner: own posts and received posts under one sender line with the channel name.
3. Texts of 124,816 bytes, final and live: plain lines; one code line of 124,816 units; one table row of 124,816 units. Each row is a teaser within the limits of 2.7; frame time while the row is scrolled and updated is at most that of a 15,602-byte item in bubbles on the same device. Its viewer: bubble colours, `*x*` italic.
4. Image without text in a wide desktop window: meta over the image corner. Desktop text selection over several rows, copy: sender names excluded.
5. Selection mode in left-to-right and right-to-left apps: inset, text wrapped again; iOS row heights correct after entering and leaving.
6. Swipe to reply; menu by long press and right click on the empty part of a plain row; go-to button on every row in search and content filter mode.
7. Right-to-left text in a left-to-right app and the reverse; one-hand UI with the bottom bar.
8. Control: three values, kept after restart; the open chat is redrawn at once; iOS row heights correct.
9. Kotlin contact info: layout changed, then mute, then send receipts: the layout of the open chat is unchanged.
10. Note folder, member support chat, reports, chat item info, chat list preview, notifications: unchanged.

## Interfaces

```
Kotlin           LocalChatLayout: ProvidableCompositionLocal<ChatLayout>, default Bubbles
                 MarkdownText(chatLayout)
iOS              EnvironmentValues.chatLayout
                 messageText(chatLayout:)
                 FramedItemView.mediaMaxWidth
both             MSG_FLAT_MAX_LINES = 500
                 MSG_FLAT_TEASER_LINES = 400

Boundary with the block-markdown-rendering plan
this plan        the row around the message and its layout value (sections 2-4)
                 the chatLayout parameter of MarkdownText and messageText, used for Format.Bold
rendering plan   the views drawn inside the message for block segments, in both layouts
                 horizontal scrolling of a block and its relation to the reply swipe (ChatView.kt:1987, ChatView.swift:2161)
                 text layouts per item and desktop selection over them (TextSelection.kt:437)
contract         maximum block width: the content width of 2.4
                 the content ends with the meta reserve of reserveSpaceForMeta (CIMetaView.kt:130), placed by the "meta" row of 2.5
                 block views in an item: at most `MSG_MAX_BLOCKS` (rendering plan)
```

## Depends on

```
agent-profile plan       ChatLayout: Bubbles, Flat, Unknown(type); iOS: bubbles, flat, unknown
                         ChatInfo.chatLayout: effective layout, computed in the apps
                         Contact.chatLayoutOverride, GroupInfo.chatLayoutOverride: local override
                         profile.chatLayout, groupProfile.chatLayout: profile value
                         setChatLayout: Kotlin (rhId, chatInfo, layout, chatModel); iOS (_ chat, layout:)
message-read-more plan   the limits of 2.7 are applied to the message text and to desktop selection of a flat chat view
                         msgTeaser: the two unit limits of 2.7 are new parameters; MsgTeaserTest.kt: the code line and table row cases of check 3 (section 7)
                         showFullMessage: viewer of a truncated flat item; ChatInfo.chatLayout of the chat is passed to its texts
                         "Shared code": the combined edits of the functions changed by this plan, that plan and the rendering plan; their order
rendering plan           app types: Format.Strong and the marker classes
block-markdown plan      core types: Strong for **x**, bold in both layouts; Bold for *x*
Format.Bold rule: 2.5, sections 3 and 4; release 1, with `Strong` of the core parser and part 1 of the rendering plan
message-parts plan       text of a sent or received item: at most maxMsgParts × maxEncodedMsgLength = 124,816 bytes
```

## Open questions

1. "Read more" in the flat layout: a flat row above 500 lines (16,000 characters on a phone) is a teaser, against "CLI layout does not need read more". Recommended: accept; smaller texts are drawn in full.
2. Maximum text of one item: 8 parts (124,816 bytes, 16 items per recipient queue) or 32 parts (499,264 bytes, 4 items). Recommended: 8.
3. `*x*` is kept as `Bold` (bold in bubbles, italic in flat rows) and `**x**` is `Strong` (bold in both). Surfaces of a flat chat with italic `*x*`: rows only, or rows and the viewer. Recommended: rows and the viewer; bold in previews, compose context and item info.
4. Layout in profiles is set by API, CLI and libraries only; the apps have the per-chat override. Recommended: yes.
5. An item with a part that is not received is shown with the received text and a red warning icon in the meta; it is skipped by forwarding and history. Recommended: yes.
6. Read more in bubbles: truncated above 24 lines, teaser of 16 lines (line: 32 units on phones, 80 on desktop); a viewer screen in place of expansion in the bubble; live items truncated while updated; search results with the flat limits. Recommended: accept; thresholds adjusted after a trial.
7. Flat row design: sender line with avatar (in place of a start gutter); own rows in the same form with the name "you"; wallpaper image omitted; time of grouped rows in the meta. Recommended: as listed.
