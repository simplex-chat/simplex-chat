# Terminal chat layout: UI

Date: 2026-10-13. Revised: 2026-10-14, 2026-10-19, 2026-10-21.

Chats with the layout `terminal` are drawn by the Android and desktop apps as full-width start-aligned rows without bubbles. iOS draws the classic layout; its layout pickers were removed until it draws terminal rows.

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
avatars           groups and channels only; gutter: member image width
gutter            other chats: GO_TO_ITEM_BUTTON_SIZE (22dp)
sender groups     new sender row after an event or a sender change, as classic: time gaps and date changes do not repeat it
time, status      at the end of the row, level with the last text line, for every item with a time
                  images and videos without caption: over the media in white, as classic, when the row has no space beside them
quote             Markdown quote design: bar, text at 70%
headers           forwarded and report headers without background
media             message corner clip, start at the text edge
cards             link preview and chat link in the quote colour, message corner radius
message text      onSurface at 87% in dark themes, both layouts
spacing           item gap 1dp, 4dp after a large gap; sender row 8dp; text top padding 0 for the first part of an item, bottom 1dp
go-to button      in the gutter, centred on the message, below the avatar when the message is shorter
row tint          hover and press: indication over the whole row; selected: primary 10%; highlighted: primary 14%
colours           app, user and chat themes, as classic
wallpaper         background colour of the wallpaper, preset or custom; image omitted
theme screens     preview as terminal rows ("alice", then "bob" in the own colour), one height for both layouts
                  wallpaper presets as colours
                  custom image tile, image scale, scale slider and wallpaper accent hidden
theme export      colours and wallpaper; the layout is not exported
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
  ChatView (:269-:274)                   terminal = activeChatLayout == Terminal; LocalTerminalLayout provided
  ChatLayout composable (:1007)          chatViewBackgroundModifier(drawWallpaperImage = !LocalTerminalLayout.current)
  ChatItemsList (:2458)                  showAvatar = terminalItemShowsSender(item, older) when terminal
  local ChatItemView (:2031)             terminal branch:
                                           ChatItemBox with the row tint and terminalRowIndication
                                           SelectedListItem when selecting
                                           TerminalChatItemRow(terminalSender or null, terminalGutter, itemTopGap, terminalStartOffset and swipeableModifier, GoToItemButton)
                                           ChatItemViewShortHand with the row interaction source
views/chat/item/TerminalItemView.kt      LocalTerminalLayout, sender rule, row interaction, ItemWithMeta, TerminalChatItemRow, terminalSender
views/chat/item/ChatItemView.kt          alignEnd = sent && !terminal; item click without indication; reactions at the text edge
                                         GoToItemButton and GO_TO_ITEM_BUTTON_SIZE at top level
                                         chatItemTailEnabled false
views/chat/item/FramedItemView.kt        transparent background; headers and quote without background; quoteBar; mediaModifier; cardModifier
                                         CIMarkdownText: parameter first, bottom padding, messageTextBottom
                                         ItemWithMeta; CHAT_ITEM_TEXT_PADDING, CHAT_ITEM_TEXT_VERTICAL_PADDING
views/chat/item/MarkdownBlocks.kt        Modifier.quoteBar, shared with Markdown quotes
views/chat/item/TextItemView.kt          messageTextColor()
views/chat/item/CIEventView.kt           chatEventHorizontalPadding(), ChatEventWithMeta
item views with ItemWithMeta             EmojiItemView, CIVoiceView, CICallItemView, DeletedItemView, MarkedDeletedItemView
                                         IntegrityErrorItemView, CIRcvDecryptionError, CIGroupInvitationView
item views with ChatEventWithMeta        CIChatFeatureView, CIFeaturePreferenceView, CIMemberCreatedContactView, event items of ChatItemView
views/chat/ChatInfoView.kt               ChatLayoutSection, setChatLayout; LocalTerminalLayout of the chat theme screen
views/helpers/ThemeModeEditor.kt         layoutSection slot of ChatWallpaperEditor; terminal passed to editColor
views/usersettings/Appearance.kt         ChatLayoutRow; PreviewInEitherLayout; terminal preview rows
                                         presets as colours; custom image tile, image scale, scale slider and accent hidden
                                         LocalTerminalLayout by profileLayoutIsTerminal
```

## 4. iOS

```
drawing         classic
controls        none; the pickers return with terminal rows
theme writes    chatLayout kept (agent profile plan, section 6)
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

Checked on desktop on 2026-10-21:

1. Direct chat, group and channel: rows, sender rows, avatars in groups, time at the row end for every item kind.
2. Spacing: close messages at the line pitch (22px).
3. Search and forwarded items: go-to button in the gutter.
4. Theme screens of a terminal chat and profile: preview, presets, hidden controls.
5. Layout choices: chat, profile and app; the open chat is redrawn at once; "Reset to app theme" keeps the chat layout; the choices are kept after a restart.

## Depends on

```
agent profile plan             ChatLayout; ChatInfo.activeChatLayout; storage of the choices
block Markdown rendering       MarkdownBlocks; Modifier.quoteBar shared with Markdown quotes
```

## Open questions

1. Search: a one-line first message of a sender group is 17dp taller, as the avatar and the search icon are stacked in the gutter (59dp).
