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
- `ChatItemView`: alignment is start for all items, the bubble click has no indication, voice-only items render as received, and `GoToItemButton` is always on the right.
- `FramedItemView`: transparent background.
- `ChatItemsList` passes `showTimestamp = itemSeparation.timestamp && !flat`; the time moves to the row header or the hover gutter.

The 3 remaining direct reads of `chatItemTail` in `ChatView.kt` are used only by the bubble branches of `ChatViewListItem`, which the flat layout does not reach.

### Flat rows (`views/chat/item/FlatChatItemView.kt`)

- `CIContent.hasFlatSender`: exhaustive `when`, true for content with a sender (messages, deleted/moderated/blocked, calls, errors, group invitations, invalid JSON), false for events, feature/preference changes, E2EE info and the chat banner.
- `flatItemHasHeader(item, older)`: pure function. True when the item has a sender and the older item is missing, has no sender, is on a different date, or `getItemSeparationLargeGap(item, older)` is true (different sender or direction, or 60s or more apart). `getItemSeparationLargeGap` in `ChatView.kt` became `internal`. The rule does not use `previousItemSeparationLargeGap`, which is false for the oldest loaded item.
- `FlatChatItemRow`: full-width hoverable row that sets `hoveredItemId`, tints its background on hover, has a 56dp gutter (avatar on header rows, time on hover otherwise) and a column with the sender header and the item content.
- Sender: `GroupRcv` member (role above `Member`), `ChannelRcv` channel with `channel_role_label`, `GroupSnd` the user's membership (incognito-correct), `DirectRcv` the contact, other sent and local directions the current user or "Incognito". Avatar click opens member info or chat info for received items.
- `ChatView.kt`: `ChatItemsList` computes `flatHeader = LocalFlatLayout.current && flatItemHasHeader(item, merged.oldest().nextItem)` and passes it through `ChatViewListItem`, whose inner wrapper renders `FlatChatItemRow` instead of the Group/Direct branches. The selection checkbox and selection click overlay are shared; the selection offset also applies to sent items.

### Hover action bar

Refactor of the context menu in `ChatItemView.kt`, no behavior change:

- `data class ItemQuickActions(react, reply, forward)` and `itemQuickActions(cInfo, cItem, live)`: all false without message content, for temporary items and reports; otherwise the existing menu conditions.
- `availableReactions(cItem)`, top-level `MsgReactionsMenu(cInfo, cItem, setReaction, showMenu)`, and `replyToItem(composeState, cItem, useLinkPreviews)`. The menu and swipe to reply in `ChatView.kt` use them; swipe keeps its `TEMP_LIVE_CHAT_ITEM_ID` guard.

`FlatHoverBar` (in `FlatChatItemView.kt`) is a bordered `Surface` with the first 3 available reactions, an add-reaction button opening `MsgReactionsMenu` in a `DefaultDropdownMenu`, reply, forward, and More, which sets the existing `showMenu`. `ChatItemView` renders it as an unbounded overlay at its top end when the layout is flat, the item is not a preview or the live dummy, has a sender, selection is off, and the item is hovered or its reaction picker is open. Strings `add_reaction`, `more_actions`.

### Rail (`views/chatlist/DesktopRail.kt`)

- `DESKTOP_RAIL_WIDTH = 64.dp`, scaled by `fontSizeSqrtMultiplier`.
- Users: active and non-hidden users by `activeOrder`, active-user mark, `userUnreadBadge` for others. Click on an inactive user calls `switchToUser`, extracted from `UserPicker.onUserClicked`; click on the active user opens `UserPicker`. Disabled while chat is not running and no remote host is connected.
- Lists: "All", preset tags with chats in `PresetTagKind` order (icons and labels from `presetTagLabel`, now `internal`), user tags with unread dot and `TagsDropdownMenu` on long-press or right-click, "Add list" opening `TagListEditor`. A private `toggleFilter` sets `chatModel.activeChatTagFilter`.
- Settings at the bottom opens `SettingsView` in `ModalManager.start`.
- `DesktopScreenLayout` draws the rail first and shifts every start offset by the rail width. `ModalManager` adds the rail width when it expands the window for an end modal.
- `ChatListView`: `TagsView` returns early and the toolbar user button is hidden in the flat layout (the settings menu button for the no-user case stays).

### Compact chat list

- `ChatPreviewView`: the unread/mention/muted/favorite block became the local composable `chatPreviewUnreadIndicator()`, used by both layouts. In the flat layout the view renders one row: `ChatInfoImage` at 32dp, `chatPreviewTitle()`, the indicator and `chatStatusImage()`.
- `views/chatlist/ChatListNavLinkView.desktop.kt` (desktopMain): 4dp vertical padding instead of 8dp, no dividers.

### Deviations from the approved plan

- The rail widens the start pane by 64dp instead of keeping the total start width and shrinking the chat list: the desktop `UserPicker` computes its item spacing from `DEFAULT_START_MODAL_WIDTH` (388dp, `views/chatlist/UserPicker.desktop.kt` in desktopMain).
- Tag toggling and the preset tag list were not extracted from `ChatListView`; the rail has its own `toggleFilter` and filter. There is no `ChatTagFilterTest`.
- The user switch helper is named `switchToUser`, not `changeActiveUser`, which is the existing `ChatController` API it calls.
- `ChatItemView` has no new `showMenu` parameter: the hover bar is rendered inside `ChatItemView` and uses its own `showMenu`.
- `flatItemHasHeader` uses `hasFlatSender` instead of `mergeCategory` and message-content checks.
- Rail items have text labels under the icons instead of tooltips.
- Chat list rows have no added hover background.

### Threat model

Presentation-only change. No new data is read, stored or sent; actions in the hover bar call the same functions with the same conditions as the context menu, so a malicious contact or member cannot reach any action that the menu would not offer.

## Implementation steps

Each step is one commit.

1. `desktop: add experimental flat layout toggle`: preference, Developer options toggle, string, `LocalFlatLayout`, `desktopFlatLayoutEnabled`, `DesktopScreen` provider.
2. `ui: extract item reply, reaction and forward checks`: `ItemQuickActions`, `itemQuickActions`, `availableReactions`, top-level `MsgReactionsMenu`, `replyToItem`; context menu and swipe to reply use them; `ItemQuickActionsTest`.
3. `desktop: drop bubble chrome in flat layout`: `chatItemTailEnabled`, alignment, indication, voice and `GoToItemButton` changes, transparent `FramedItemView` background, Appearance preview override.
4. `desktop: render chat items as flat rows`: `FlatChatItemView.kt` (`hasFlatSender`, `flatItemHasHeader`, `FlatChatItemRow`, sender header and avatar), `ChatView.kt` branch and `flatHeader`, `getItemSeparationLargeGap` internal, no inline timestamp; `FlatChatItemViewTest`.
5. `desktop: add hover action bar to flat rows`: `FlatHoverBar` and its overlay in `ChatItemView`; strings `add_reaction`, `more_actions`.
6. `desktop: add profile and tag rail to flat layout`: `DesktopRail.kt`, `DesktopScreenLayout` offsets, `ModalManager` window expansion, `switchToUser`, `presetTagLabel` internal, hidden `TagsView` and toolbar user button.
7. `desktop: compact chat list rows in flat layout`: `chatPreviewUnreadIndicator`, compact `ChatPreviewView` row, `ChatListNavLinkLayout` padding and dividers.
8. `desktop: skip flat header check when layout is off`: `flatHeader` is computed only when `LocalFlatLayout.current` is true.
9. `desktop: close start modals before rail actions`: settings, "Add list" and the active user avatar close open start-pane modals first, so repeated clicks do not stack modals.
10. `docs: document desktop flat layout`: spec, product, CODE.md Document Map, this plan.

Tests in `common/src/commonTest/kotlin/chat/simplex/app/`:

- `ItemQuickActionsTest`: allActionsForRegularMessage, reactDisabledWhenFeatureOff, reactDisabledAfterThreeUserReactions, replyHiddenForLocalNote, replyAndForwardHiddenWhenLive, noQuickActionsForReport, noQuickActionsForLiveDummy, noQuickActionsForDeletedItem, noQuickActionsForEventItem, availableReactionsExcludeUserReacted.
- `FlatChatItemViewTest`: headerShownForOldestItem, headerHiddenForSameSenderWithinMinute, headerShownAfterMinuteGap, headerShownForDifferentGroupMember, headerShownWhenDirectionChanges, headerShownAfterEventItem, headerShownOnDateChangeWithinMinute, noHeaderForEventItem.

## Affected product and spec documents

- `apps/multiplatform/product/views/chat.md`: Desktop Flat Layout section.
- `apps/multiplatform/product/views/chat-list.md`: platform layout row, Desktop Flat Layout section (rail, compact rows), `DesktopRail.kt` source.
- `apps/multiplatform/product/views/settings.md`: Flat layout developer option.
- `apps/multiplatform/product/README.md`: Desktop Features row.
- `apps/multiplatform/product/concepts.md`: PC32 Desktop Flat Layout.
- `apps/multiplatform/spec/client/chat-view.md`: flat adjustments in ChatItemView, quick action helpers in Context Menu Actions, section 10 Flat Layout, `[REC]` thread side panel.
- `apps/multiplatform/spec/client/chat-list.md`: compact row, nav link layout, `presetTagLabel`, `switchToUser`, section 11 Desktop Rail.
- `apps/multiplatform/spec/client/navigation.md`: `DesktopScreen` flat layout flag, rail column and offsets.
- `apps/multiplatform/spec/state.md`: `desktopFlatLayout` preference.
- `apps/multiplatform/spec/impact.md`: PC32, new files, changed rows.
- `apps/multiplatform/spec/README.md`: document index descriptions, concept count.
- `apps/multiplatform/CODE.md`: Document Map rows for `FlatChatItemView.kt` and `DesktopRail.kt`.

## Roadmap

Thread side panel (replies to a message in the end column) needs core support: `getChatItemIDs` (`src/Simplex/Chat/Store/Messages.hs`) has no filter for items quoting a given message, `chat_items.quoted_shared_msg_id` has no index, and there is no reply count. The client would add a `SecondaryContextFilter` variant and open the panel via `ModalManager.end`, as `showGroupReportsView` does.
