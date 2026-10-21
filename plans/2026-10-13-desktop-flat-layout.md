# Desktop: experimental flat (Slack/Discord-like) layout

## Problem

Desktop chats use the mobile bubble layout: sent messages on the right, received on the left, each message in its own bubble with a tail. On a wide window, in groups and in chats with bots, this wastes horizontal space and is harder to scan than the flat row layout of Slack or Discord. Profiles and chat lists (tags) are reachable only through the user picker and the tag chip row above the chat list.

## Solution summary

A desktop-only developer option, "Flat layout (experimental)", switches the whole desktop window to a flat layout for evaluation:

- message rows without bubbles, with the sender avatar, name, role and time shown once per group of messages;
- a hover action bar on each message: 3 quick reactions, add reaction, reply, forward, and More (the existing context menu);
- a rail at the far left with user avatars, "All", preset and user lists, "Add list" and settings;
- compact one-line chat list rows.

Existing theme colors only. No change on Android, and no change on desktop while the option is off.

Out of scope: a thread side panel. It needs core work (see Roadmap).

## Technical design

Paths are relative to `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/` unless stated otherwise.

### Mode flag

- `AppPreferences.desktopFlatLayout` (key `DesktopFlatLayout`, default `false`) in `model/SimpleXAPI.kt`.
- Toggle in `DeveloperView`, inside the desktop-only block of developer options; string `desktop_flat_layout`.
- `App.kt`: `LocalFlatLayout = staticCompositionLocalOf { false }` and `desktopFlatLayoutEnabled = appPlatform.isDesktop && developerTools && desktopFlatLayout`. `DesktopScreen` provides `LocalFlatLayout` and its body moved to the private `DesktopScreenLayout`. All columns and `ModalManager.fullscreen` are inside the provider, so end-panel chats (reports, member support) are covered.
- `Appearance.kt` provides `LocalFlatLayout = false` around the message-shape preview so roundness and tail settings stay previewable.

Views read `LocalFlatLayout.current` only where the bubble look or the list chrome is decided; no existing view is forked.

### Bubble chrome off

- `ChatItemView.kt`: `chatItemTailEnabled()` = tail preference `&& !LocalFlatLayout.current`, used by `chatItemOffset`, `clipChatItem` and `FramedItemView`, so `shapeStyle` never draws a tail.
- `ChatItemView`: alignment is start for all items, the bubble click has no indication, voice items (with or without a caption) are not mirrored, and `GoToItemButton` is always on the right.
- `FramedItemView`: transparent background, 2dp vertical text padding, forwarded and report header rows without fill (the report header also pads at the bottom). The outer container is a `Column`, so the meta follows only the body.
- Replies: in the flat layout the quote block is replaced by `ciReplyLine`, one compact line above the body: reply icon, `ciReplyAvatar` (16dp scaled, every direction; `FlatUserImage` handles incognito), the sender in SemiBold capped at 160dp scaled with an ellipsis, and a one-line 13sp `secondary` snippet. Media without text shows a label; voice shows "Voice message" and the duration; chat links use their name as prefix and strip the link. A click calls `openQuotedItem` (shared with the bubble quote block); hover tints the text `onBackground` without indication; long-press or right-click opens the menu.
- Compose: `Modifier.flatComposeField()` (onBackground 6% fill, 8dp corner, 12dp/8dp margins) on the main and "send message to connect" input rows; the top `Divider` is hidden.
- `ChatItemsList` passes `showTimestamp = itemSeparation.timestamp && !flat`; the time moves to the row header or the hover gutter. `flatMetaVisible` drops the meta and its reserved space for received items that would show only the time; the delivery status is shown only for sent items.
- `DeletedItemView` and `MarkedDeletedItemView`: transparent, 2dp vertical padding, 14sp text; `CIMetaView` shows no time for deleted content. `CIEventView` uses the flat text padding.
- Reactions are pills (`flatReactionPill`) with the count always shown and 14sp emoji. `DateSeparator` renders `FlatDateSeparator`, a date pill on a divider.
- Group invitations and error items (`CIGroupInvitationView`, `IntegrityErrorItemView`, `CIRcvDecryptionError`) keep their rounded coloured card.

The 3 remaining direct reads of `chatItemTail` in `ChatView.kt` are used only by the bubble branches of `ChatViewListItem`, which the flat layout does not reach.

### Flat rows (`views/chat/item/FlatChatItemView.kt`)

- `CIContent.hasFlatSender` (private): exhaustive `when`, true for content with a sender (messages, deleted/moderated/blocked, calls, errors, group invitations, invalid JSON), false for events, feature/preference changes, E2EE info and the chat banner.
- `flatItemHasHeader(item, older)`: pure function. True when the item has a sender and the older item is missing, has no sender, is from a different sender (`sameFlatSender`: same direction, same `groupMemberId` for `GroupRcv`, same `showGroupAsSender` for `GroupSnd`), is 60s or more apart by `createdAt`, or is on a different local date. `getItemSeparationLargeGap` stays private in `ChatView.kt`. The rule does not use `previousItemSeparationLargeGap`, which is false for the oldest loaded item.
- `FlatChatItemRow`: full-width hoverable row with a `hoverBar` slot. Background, drawn inside a 4dp inset with 6dp rounded corners: `primary` at 12% when highlighted, `onBackground` at 4% (`FLAT_SUBTLE_FILL_ALPHA`) when hovered. `FlatAvatarContrast` wraps the gutter and reply avatars, mixing `secondaryVariant` 15% toward `onBackground` so placeholder avatars keep contrast. A 56dp gutter shows the avatar on header rows, or the time (10sp, end-aligned) on hover otherwise; a column holds the sender header and the item content. Rows with message content have a minimum height that fits the hover bar.
- Sender: `GroupRcv` member (role above `Member`), `ChannelRcv` and `GroupSnd` with `showGroupAsSender` the channel ("channel" label, group image, opens chat info), other `GroupSnd` the user's membership (incognito-correct), `DirectRcv` the contact, other sent and local directions the current user or "Incognito". Avatar click: member info for `GroupRcv`, chat info for `ChannelRcv`, `DirectRcv` and a post shown as the channel; none for other own messages.
- `ChatView.kt`: `ChatItemsList` computes `flatHeader = LocalFlatLayout.current && flatItemHasHeader(item, merged.oldest().nextItem)` and passes it through `ChatViewListItem`, whose inner wrapper renders `FlatChatItemRow` around `ChatItemViewShortHand` instead of the Group/Direct branches. The selection checkbox and selection click overlay are shared; `Modifier.flatStartOffset` moves the row by the selection offset for sent and received items.

### Hover action bar

Refactor of the context menu in `ChatItemView.kt`, no behavior change:

- `data class ItemQuickActions(react, reply, forward)` and `itemQuickActions(cInfo, cItem, live)`: all false without message content, for temporary items and reports; otherwise the existing menu conditions.
- `availableReactions(cItem)`, top-level `MsgReactionsMenu(cInfo, cItem, setReaction, showMenu)`, and `replyToItem(composeState, cItem, useLinkPreviews)`. The menu and swipe to reply in `ChatView.kt` use them; swipe keeps its `TEMP_LIVE_CHAT_ITEM_ID` guard.

`itemQuickActions` is evaluated inside the menu's `DefaultDropdownMenu` content. `ChatItemView` and `ChatItemViewShortHand` take a `showMenu` parameter.

The private `FlatHoverBar` (in `FlatChatItemView.kt`) is a bordered `Surface` with the first 3 available reactions, an add-reaction button opening `MsgReactionsMenu` in a `DefaultDropdownMenu`, reply, forward, and More, which sets `showMenu`. `ChatItemsList` creates one `showMenu` per item and passes it to `ChatItemViewShortHand` and to `FlatHoverBarSlot`, which `FlatChatItemRow` renders in its `hoverBar` slot at the row's top end. `flatHoverBarVisible` shows the bar when selection is off, the item has message content and is not the live dummy, and the row is hovered or the reaction picker is open while reactions are available; a `LaunchedEffect` closes the picker otherwise. Flat rows do not write `hoveredItemId`. Strings `add_reaction`, `more_actions`.

### Rail (`views/chatlist/DesktopRail.kt`)

- `DESKTOP_RAIL_WIDTH = 64.dp`, scaled by `fontSizeSqrtMultiplier`.
- Surfaces: `flatRailColor()` darkens the theme background 7% (light) or 50% (dark); `flatListColor()` darkens it 3.5% (light) or lifts it 4.5% toward white (dark). `FlatListSurface` paints the chat-list pane and overrides `MaterialTheme.colors.background` and `secondaryVariant` (`flatContrastSecondaryVariant`) for that pane only. The chat pane keeps the theme background and wallpaper. No dividers between rail, list and chat in the flat layout; the start pane content is a remembered `movableContentOf`, so toggling the layout keeps its state.
- Profile tiles (keyed by user id): profile image, or `firstGrapheme` of the name on a `primary` 15% tint. Corners follow `profileImageCornerRadius` and scale to 60% on hover or when active; the left marker animates to 0, 40% (hover) or 80% (active) of the tile height. The scroll area ends in a 12dp fade.
- Users: active and non-hidden users by `activeOrder`, active-user mark, `userUnreadBadge` for others, drawn outside the avatar clip. Click on an inactive user hides `UserPicker` and calls `switchToUser`, extracted from `UserPicker.onUserClicked`; click on the active user opens `UserPicker`. `profileSwitchAllowed`, extracted from `ShareListView`, blocks switching while sharing a forward, chat link or address; switching is also blocked while users are being switched. Inactive users are disabled while chat is not running and no remote host is connected; the active user still opens the picker.
- Lists: "All", preset tags with chats in `PresetTagKind` order (icons and labels from `presetTagLabel`, now `internal`), user tags with unread dot and `TagsDropdownMenu` on long-press or right-click, "Add list" opening `TagListEditor`. A private `toggleFilter` sets `chatModel.activeChatTagFilter`. Items have a 6dp inset and an 8dp rounded click area; the active item is neutral (onBackground icon and label on a 40x32dp onBackground 10% background); icons scale with `fontSizeSqrtMultiplier`. List, "All", "Add list" and Settings clicks close start modals first.
- Settings at the bottom, below a divider, opens `SettingsView` in `ModalManager.start`.
- `DesktopScreenLayout` draws the rail first and shifts every start offset by the rail width. `ModalManager` adds the rail width when it expands the window for an end modal.
- `ChatListView`: `TagsView` returns early; the toolbar shows the profile button only when `desktopNoUserNoRemote` and hides the no-user Settings menu button; the divider next to the connect-by-name row is dropped.

### Compact chat list

- `ChatPreviewView`: the unread/mention/muted/favorite block became the local composable `chatPreviewUnreadIndicator()`, used by both layouts. In the flat layout the view renders one row: `ChatInfoImage` at 28dp with a 12sp inactive overlay, `chatPreviewTitle()`, the indicator (badge without vertical offset), a spacer and `chatStatusImage()`. Direct and group titles use `titleStyle`/`titleWeight` (`body1`, SemiBold when unread) and the secondary color when muted without unread mentions. A `LaunchedEffect` stops a deleted chat's playing voice message. `ContactRequestView` and `ContactConnectionView` use the 28dp image.
- `views/chatlist/ChatListNavLinkView.desktop.kt` (desktopMain): rows inset 6dp horizontally and 1dp vertically with 6dp rounded corners, 4dp vertical padding instead of 8dp, selected row at onBackground 10%, no dividers.

### Deviations from the approved plan

- The rail widens the start pane by 64dp instead of keeping the total start width and shrinking the chat list: the desktop `UserPicker` computes its item spacing from `DEFAULT_START_MODAL_WIDTH` (388dp, `views/chatlist/UserPicker.desktop.kt` in desktopMain).
- Tag toggling and the preset tag list were not extracted from `ChatListView`; the rail has its own `toggleFilter` and filter. There is no `ChatTagFilterTest`.
- The user switch helper is named `switchToUser`, not `changeActiveUser`, which is the existing `ChatController` API it calls.
- `flatItemHasHeader` uses `hasFlatSender` and its own `sameFlatSender` instead of `mergeCategory`, message-content checks and `getItemSeparationLargeGap`.
- Rail items have text labels under the icons instead of tooltips.
- Chat list rows have no added hover background.
- After the user asked for a "general Discord vibe and feel, with SimpleX features, lean design", the Slack-like quote bar (step 12) was replaced by a compact reply line, and the user approved surface tones, rounded inset highlights, rail profile tiles and a rounded compose field (flat layout only). A shaded chat pane in dark themes (step 19) was dropped in step 20 to keep the theme background and wallpaper; as a result the list is lighter than the chat in dark themes.

### Known limitations

- Start-pane modals, `ShareListView` and `SetDeliveryReceiptsView` use the plain theme background over the shaded list.
- The app-bar blur underlay uses the plain theme background.
- In dark themes the list is lighter than the chat.

### Threat model

Presentation-only change. No new data is read, stored or sent; actions in the hover bar call the same functions with the same conditions as the context menu, so a malicious contact or member cannot reach any action that the menu would not offer.

## Implementation steps

Each step is one commit.

1. `desktop: add experimental flat layout toggle`: preference, Developer options toggle, string, `LocalFlatLayout`, `DesktopScreen` provider (reading `developerTools && desktopFlatLayout` inline).
2. `ui: extract item reply, reaction and forward checks`: `ItemQuickActions`, `itemQuickActions`, `availableReactions`, top-level `MsgReactionsMenu`, `replyToItem`; context menu and swipe to reply use them; `ItemQuickActionsTest`.
3. `desktop: drop bubble chrome in flat layout`: `chatItemTailEnabled`, alignment, indication, voice and `GoToItemButton` changes, transparent `FramedItemView` background, Appearance preview override.
4. `desktop: render chat items as flat rows`: `FlatChatItemView.kt` (`hasFlatSender`, `flatItemHasHeader`, `FlatChatItemRow`, sender header and avatar), `ChatView.kt` branch and `flatHeader`, `getItemSeparationLargeGap` internal, no inline timestamp; `FlatChatItemViewTest`.
5. `desktop: add hover action bar to flat rows`: `FlatHoverBar` and its overlay in `ChatItemView`; strings `add_reaction`, `more_actions`.
6. `desktop: add profile and tag rail to flat layout`: `desktopFlatLayoutEnabled` (also read by `ModalManager`), `DesktopRail.kt`, `DesktopScreenLayout` offsets, `ModalManager` window expansion, `switchToUser`, `presetTagLabel` internal, hidden `TagsView` and toolbar user button.
7. `desktop: compact chat list rows in flat layout`: `chatPreviewUnreadIndicator`, compact `ChatPreviewView` row, `ChatListNavLinkLayout` padding and dividers.
8. `desktop: skip flat header check when layout is off`: `flatHeader` is computed only when `LocalFlatLayout.current` is true.
9. `desktop: close start modals before rail actions`: settings, "Add list" and the active user avatar close open start-pane modals first, so repeated clicks do not stack modals.
10. `docs: document desktop flat layout`: spec, product, CODE.md Document Map, this plan.
11. `desktop: fix flat row header, hover bar and highlight`: `sameFlatSender` header rule, hover bar moved into `FlatChatItemRow` (`hoverBar` slot, `FlatHoverBarSlot`, `flatHoverBarVisible`, `showMenu` parameter), highlight tint, overlay removed from `ChatItemView`, `getItemSeparationLargeGap` private again.
12. `desktop: slack-style quotes, reactions and dates`: `flatQuoteBar`, `flatReactionPill`, `FlatDateSeparator`, flat padding in `FramedItemView`, `CIEventView` and deleted item views.
13. `desktop: fix rail badges, profile switching and lists`: badge outside the clip, `profileSwitchAllowed`, switching guards, rail active background and icon scaling, toolbar buttons, connect-by-name dividers, compact row avatar size, title style and muted color, compact images in contact request and connection rows.
14. `desktop: polish flat meta, deleted items and list`: `flatMetaVisible`, no time for deleted items, divider above Settings, compact row spacing.
15. `desktop: address flat layout re-review findings`: `flatStartOffset`, RTL quote bar, hover bar picker reset in `LaunchedEffect`, row min height only for message items, clickable channel avatar for own channel posts, status only for sent items, `FlatHoverBar` private, deleted-chat voice stop; more tests.
16. `desktop: align flat selection checkbox with avatar`: `flatSelectionTopPadding`; `ChatItemBox` content gets a `BoxScope` receiver.
17. `docs: update desktop flat layout after review`: spec, product, impact, README links, this plan.
18. `desktop: layered surfaces, reply line and rail tiles`: rail and list shades, `FlatListSurface`, first reply line, rail profile tiles with animated corners and marker, rounded rail items, rounded inset list rows, inset row highlight, compose field.
19. `desktop: fix dark layering and reply line in flat layout`: dark-theme shades (including a shaded chat pane), reply line moved above the body (`Column`), `openQuotedItem` and `ciReplyAvatar` for every direction, hover tint and menu on the reply line, neutral active rail item, stronger selected list row.
20. `desktop: fix flat list surface, reply line and rail tiles`: start pane as `movableContentOf`, chat pane back to the theme background, `flatQuoteBar` removed, `FlatUserImage`, scaled reply avatar, capped sender, voice and chat-link snippets, report header padding, rail tiles keyed by user id, `firstGrapheme` and `DesktopRailTest`.
21. `desktop: avatar contrast, rail fade and review nits`: `FlatAvatarContrast` and `flatContrastSecondaryVariant`, rail bottom fade, scaled sender cap, list row constant names; more `DesktopRailTest` cases.
22. `docs: document lean desktop flat style`: spec (chat-view, chat-list, navigation, compose, impact), product (chat, chat-list, concepts), this plan.

Tests in `common/src/commonTest/kotlin/chat/simplex/app/`:

- `ItemQuickActionsTest` (11): allActionsForRegularMessage, reactDisabledWhenFeatureOff, reactDisabledAfterThreeUserReactions, replyHiddenForLocalNote, replyHiddenWhenContactCannotReceive, replyAndForwardHiddenWhenLive, noQuickActionsForReport, noQuickActionsForLiveDummy, noQuickActionsForDeletedItem, noQuickActionsForEventItem, availableReactionsExcludeUserReacted.
- `DesktopRailTest` (6): firstGraphemeOfPlainName, firstGraphemeKeepsSurrogatePairEmoji, firstGraphemeKeepsFlagEmoji, firstGraphemeTakesOnlyFirstOfTwoFlags, firstGraphemeKeepsCombiningMarks, firstGraphemeOfEmptyName.
- `FlatChatItemViewTest` (20): headerShownForOldestItem, headerHiddenForSameSenderWithinMinute, headerShownAfterMinuteGap, headerShownForDifferentGroupMember, headerShownWhenDirectionChanges, headerShownAfterEventItem, headerShownOnDateChangeWithinMinute, headerShownWhenChannelPostFollowsMemberMessage, headerShownWhenOwnerSwitchesBetweenChannelAndOwnName, headerHiddenAfterDeletedMessageFromSameSender, headerGroupsLocalNotes, noHeaderForEventItem, hoverBarShownForHoveredMessage, hoverBarHiddenWhenRowNotHovered, hoverBarHiddenInSelectionMode, hoverBarHiddenForEventAndLiveDummyItems, hoverBarStaysOpenForReactionPickerUntilReactionsUnavailable, metaHiddenForPlainReceivedMessage, metaShownForEditedReceivedMessage, metaShownForSentMessage.

## Verification

- Read-only code reviews (initial review of rows, rail and docs; re-review of the fix commits; final docs and code check) and four headless screenshot renders (`ImageComposeScene` with real composables, not the running app) reviewed against Slack; their findings are addressed in steps 11 to 17. For the lean Discord-style pass (steps 18 to 21) the user approved the surface tones, rounded inset highlights, compact reply line, rail profile tiles and rounded compose field; steps 19 to 21 address two read-only code reviews and four further headless screenshot renders in light, dark, black and SimpleX themes (with and without the default wallpaper), reviewed against the lean Discord-like goal.
- Deliberate choices kept after review:
  - The hover bar stays inside the row rather than overlapping the row above, because the bar hides as soon as the pointer leaves the row; rows with message content get a minimum height so the bar fits.
  - Sent messages keep their delivery status ticks, which Slack does not show.
  - The chat pane keeps the theme background and wallpaper, so in dark themes the list is lighter than the chat.

## Affected product and spec documents

- `apps/multiplatform/product/views/chat.md`: Desktop Flat Layout section.
- `apps/multiplatform/product/views/chat-list.md`: platform layout row, Desktop Flat Layout section (rail, compact rows), `DesktopRail.kt` source.
- `apps/multiplatform/product/views/settings.md`: Flat layout developer option.
- `apps/multiplatform/product/README.md`: Desktop Features row.
- `apps/multiplatform/product/concepts.md`: PC32 Desktop Flat Layout.
- `apps/multiplatform/spec/client/chat-view.md`: flat adjustments in ChatItemView (item views, quotes, meta, reactions, date separators), quick action helpers in Context Menu Actions, section 10 Flat Layout (header rule, sender, row background, hover bar, selection), `[REC]` thread side panel.
- `apps/multiplatform/spec/client/chat-list.md`: compact row, nav link layout, `presetTagLabel`, `switchToUser`, `profileSwitchAllowed`, section 11 Desktop Rail.
- `apps/multiplatform/spec/client/navigation.md`: `DesktopScreen` flat layout flag, rail column and offsets, flat surfaces, dividers, `movableContentOf`, known limitations.
- `apps/multiplatform/spec/client/compose.md`: flat compose field.
- `apps/multiplatform/spec/state.md`: `desktopFlatLayout` preference.
- `apps/multiplatform/spec/impact.md`: PC32, new files, changed rows (including item, meta, event, deleted, contact request/connection and share list views).
- `apps/multiplatform/spec/README.md`: document index descriptions, concept count.
- `apps/multiplatform/CODE.md`: Document Map rows for `FlatChatItemView.kt` and `DesktopRail.kt`.

## Roadmap

Thread side panel (replies to a message in the end column) needs core support: `getChatItemIDs` (`src/Simplex/Chat/Store/Messages.hs`) has no filter for items quoting a given message, `chat_items.quoted_shared_msg_id` has no index, and there is no reply count. The client would add a `SecondaryContextFilter` variant and open the panel via `ModalManager.end`, as `showGroupReportsView` does.
