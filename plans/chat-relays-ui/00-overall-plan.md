# Channels on iOS — Overall Implementation Plan

## Contents
1. [Summary](#1-summary)
2. [Existing Code](#2-existing-code)
3. [Dependencies & Order](#3-dependencies--order)
4. [Sub-plans](#4-sub-plans)
5. [Rules](#5-rules)

---

## 1. Summary

Channel = group with `useRelays = true`. Most backend types exist in Swift. Work is 7 UI sub-plans: icon, messages, compose, info, relay settings, creation flow, join flow. Plans 01–03 have no backend blockers. Plans 04–07 need backend work. Product plan: `ios-channels-product-plan.md`.

---

## 2. Existing Code

**Types (all exist, no new definitions needed):**
- `GroupInfo.useRelays: Bool` (ChatTypes.swift:2338), `.relayOwnStatus: RelayStatus?` (:2339)
- `CIDirection.channelRcv` (:3464), handled in merger (:270) but NOT in rendering
- `RelayStatus` (:2497), `GroupRelay` (:2504), `GroupMemberRole.relay` (:2744)
- `UserChatRelay` (AppAPITypes.swift:1934), `UserOperatorServers.chatRelays` (:1726)
- `UserServersWarning.noChatRelays` (:1767), `SimplexLinkType.channel` (:4709)

**API (all exist):**
- `apiNewPublicGroup` → `(GroupInfo, GroupLink, [GroupRelay])` (SimpleXAPI.swift:1813)
- `apiSendMessages(..., sendAsGroup: Bool = false, ...)` (:527)
- `apiConnectPreparedGroup` (:1138)
- Events: `publicGroupCreated` (AppAPITypes.swift:914), `groupLinkRelaysUpdated` (:1095)

**NOT yet in code:** `apiTestChatRelay`; `channelRcv` rendering in ChatView; channel icon in `chatIconName`; "you are subscriber" label; `sendAsGroup: true` call site; all channel-specific views.

---

## 3. Dependencies & Order

```
01, 02, 03 ── parallel, no backend deps
04 ─────────── needs subscriber list protocol changes
05 ─────────── needs APITestChatRelay (can stub)
06 ─────────── needs relay state events
07 ─────────── needs relay state events
```

04–05 can parallel. 06 before 07.

---

## 4. Sub-plans

### 01 — Channel Icon

**ChatTypes.swift:2382** `chatIconName` — add `if useRelays { return "antenna.radiowaves.left.and.right" }` BEFORE `businessChat` switch.

### 02 — Channel Message Rendering

**ChatView.swift** — 3 functions need `channelRcv` handling:
- `:1824` `chatItemListView` — add `.channelRcv` case using `showGroupAsSender` path (group profile image + group name as sender). Consecutive `.channelRcv` items group without repeating avatar.
- `:1653` `shouldShowAvatar` — handle `.channelRcv` (always show group avatar for first in sequence, same grouping logic as `.groupRcv`).
- `:2027` `showMemberImage` — handle `.channelRcv` in `switch prevItem?.chatDir`.

**ChatPreviewView.swift** — channel messages with `showGroupAsSender=true` already suppress sender prefix (no code change needed ✓).

**Exhaustive switch audit** — add `.channelRcv` in all `switch chatDir`:
- `ChatItemView.swift`, `ComposeView.swift`, `ChatItemInfoView.swift`, `ChatItemsMerger.swift` (already done ✓)
- `ChatTypes.swift:3234` `memberToModerate` — handle `.channelRcv` (return nil or channel moderation)

### 03 — Owner Compose & Subscriber Label

**ComposeView.swift:1301** `send()` — pass `sendAsGroup: true` when `chat.chatInfo.groupInfo?.useRelays == true && membership.memberRole >= .admin`.

**ComposeView.swift** placeholder — find `textFieldPlaceholder` / `"Message"` string and return `"Broadcast"` when channel owner/admin.

**ChatTypes.swift:1564** `userCantSendReason` — in the `.group` case, `.none` scope branch (:1578-1581), add before the observer check: `if groupInfo.useRelays { return ("you are subscriber", nil) }`.

**ShareAPI.swift:71** — resolve TODO: pass `sendAsGroup: true` for channel owner.

### 04 — Channel Info

**GroupChatInfoView.swift** — conditional labels/sections when `useRelays` (matching product plan section order):

Owner view sections:
1. "Channel link" (not "Group link") / "Owners & subscribers" → NavigationLink
2. "Edit channel profile" / "Welcome message"
3. "Chat theme" / "Delete messages after" (existing, no change)
4. "Chat relays" → NavigationLink / "Clear chat" / "Delete channel" (not "Delete group"); no "Leave channel" for single owner

Subscriber view sections:
1. "Channel link" / "Owners" → NavigationLink
2. "Welcome message"
3. "Chat theme" / "Delete messages after" (existing, no change)
4. "Chat relays" → NavigationLink / "Clear chat" / "Leave channel" (not "Leave group")

All existing "group" strings conditionally become "channel" when `useRelays`.

**NEW `ChannelMembersView.swift`** — OWNERS section (role >= owner), SUBSCRIBERS section with count header. Requires protocol changes for accurate counts.

**NEW `ChannelRelaysView.swift`** — read-only list. Row: hostname + status (owner: `RelayStatus`; subscriber: connection state). Footer: "Chat relays forward messages to channel subscribers." Relay data: fetch via event/response, store in `@State`.

### 05 — Chat Relay Management

**OperatorView.swift** — new "CHAT RELAYS" section INSIDE the `if operator_.enabled` guard (line 70), BEFORE the "Use for messages" SMP section (~line 72). List `chatRelays` with enable toggles. Footer: "Chat relays forward messages in channels you create."

**ProtocolServersView.swift** (Your Servers) — "CHAT RELAYS" section for custom relays. Extend "Add server" confirmationDialog with "Chat relay" option.

**NEW `ChatRelayView.swift`** — follows `ProtocolServerView` pattern. Preset: read-only address + test + toggle. Custom: editable + test + toggle + delete.

**NetworkAndServers.swift** — `serversCanBeSaved` already detects chatRelay changes via `UserOperatorServers` Equatable (no modification needed). Process `UserServersWarning.noChatRelays` → footer warning.

Test button: stub until `apiTestChatRelay` backend exists.

### 06 — Channel Creation

**NewChatMenuButton.swift** — add "Create channel" after "Create group", icon `antenna.radiowaves.left.and.right`.

**NEW `AddChannelView.swift`** — 3-phase `@State` view:

**Step 1 (Profile):** Name + image. "Configure relays..." → Network & Servers. "Create channel" button disabled when name invalid or no enabled relays. Auto-select relay IDs from enabled `UserChatRelay`s via `getUserServers()`.

**Step 2 (Progress):** Call `apiNewPublicGroup`. Progress bar "N/M relays connected". Tap to expand per-relay status. "Channel link" button enabled when ≥1 relay `rsActive`. Auto-advance to Step 3 when ALL relays reach `rsActive`. If "Channel link" tapped while connecting: alert "Not all relays connected. Proceed?" Subscribe to `CEvtGroupLinkRelaysUpdated` for status updates.

**Step 3 (Link):** Reuse `GroupLinkView` pattern — QR + share. "Continue" opens channel chat.

**Failure modes:**
- Sync (API fails): Alert "Error creating channel" + Retry / Cancel.
- Partial async: "2/3 connected, 1 failed". Red ● for failed. Link button enabled.
- All async fail: "0/3 connected, 3 failed" red. Alert Retry / Cancel.

### 07 — Join Channel

**NewChatView.swift:1332** `planAndConnect` — channel detected via `GroupShortLinkInfo.groupRelays` being non-empty (from `GroupLinkPlan.ok` response). Pre-join view: channel avatar + name + "N relays" (tap to expand hostnames from `GroupShortLinkInfo.groupRelays`). "Join channel" button.

**ComposeView.swift / ChatView.swift** — after join, progress bar above "you are subscriber": "Connecting... N/M relays". Tap to expand per-relay status. All connected → bar disappears.

**Failure modes:**
- Sync (all fail on connect): Alert "Failed to join channel" + Retry / Cancel.
- Partial: "2/3 connected, 1 failed". Channel works.
- All async fail: Red bar "Channel not connected". TBC: manual retry button (deferred).

---

## 5. Rules

1. **Single discriminator:** `groupInfo.useRelays`. Check BEFORE `businessChat` in all conditionals.
2. **No new types.** All exist in code.
3. **Strings:** "channel" not "group" in all labels when `useRelays`.
4. **Icon:** `antenna.radiowaves.left.and.right`.
5. **Compose:** "Broadcast" placeholder for owner/admin.
6. **Relay data:** Comes via response/event `[GroupRelay]` params, NOT from `GroupInfo` field. Store in `@State`.
7. **Switch exhaustiveness:** `channelRcv` must be handled in ALL `switch chatDir` statements.
8. **MVP scope:** If all relays removed/disconnected, no UI indication in MVP (product plan line 102). Do not build relay-loss detection UI.
