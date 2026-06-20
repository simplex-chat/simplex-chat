# iOS: fix chat list subscription status indicator stuck at empty "%"

## Problem

The connection-status indicator in the chat list (the "wave" icon + optional percentage) could stay stuck showing the initial **"share unknown"** state — a faded/grey icon and a bare `%` with no number — even when subscriptions were fully active. Opening the servers summary from the same indicator showed **100%**.

## Root cause

`SubsStatusIndicator` (`apps/ios/Shared/Views/ChatList/ChatListView.swift`) is hosted in the **navigation toolbar** (`.principal` / `.bottomBar`). It held its subscription totals in `@State` (`subs`, `hasSess`) and refreshed them with a 1-second poll of `getAgentSubsTotal()` started from `.onAppear`.

The toolbar **rebuilds frequently**: its builders (`topToolbar` / `bottomToolbar` / `bottomToolbarGroup`) read `chatModel.chats`, `chatModel.users`, `chatModel.chatRunning` (via `shouldShowOnboarding`, `leadingToolbarItem`, `trailingToolbarItem`) — all `@Published`, changing on every message / unread / chat-list update. Each rebuild **recreates** the toolbar-hosted indicator view, which **resets its `@State` totals to the empty initial value** (`SMPServerSubs.newSMPServerSubs`, `hasSess = false`). That empty state renders as the faded icon + bare `%`, so the indicator kept snapping back to it.

The detail view works because it fetches `getAgentServersSummary()` fresh on open. The two views read different sources, which is why the indicator and the summary disagreed.

`SMPServerSubs.newSMPServerSubs` with `total == 0 && !hasSess` is exactly the `"share unknown"` branch in `SubscriptionStatusPercentageView` (`ServersSummaryView.swift`), which renders `"%"` (introduced deliberately to show the `%` sign when the share is not known yet).

## Fix

Hold the totals in a **dedicated observable** so they **survive the view being recreated**, instead of binding poll state into the widely-observed `ChatModel`:

- Add `SubsStatusModel: ObservableObject` (a `static let shared` singleton, alongside the other small models in `ChatModel.swift` — `ChannelRelaysModel`, `ConnectProgressManager`), holding `@Published var totalSubs: SMPServerSubs` and `@Published var hasSession: Bool`.
- `SubsStatusIndicator`: drop the `@State` totals; observe the model with `@ObservedObject private var subsModel = SubsStatusModel.shared` and read `subsModel.totalSubs` / `subsModel.hasSession`. The existing 1-second poll calls `SubsStatusModel.shared.update(subs, hasSess)`.

Because the totals live in the singleton (not in the view's `@State`), they persist when the toolbar recreates the indicator; the view re-attaches to the same singleton and reads the current values. Only this indicator observes the model, so a poll update re-renders just the indicator — `ChatModel`'s many observers are never involved.

### Why a separate observable, not `ChatModel`

An earlier revision put the totals on `ChatModel` (`+2 @Published`). That binds transient, view-specific poll state into the app's central model, which is observed by ~70 views — every set risks waking all of them, and it grows `ChatModel` for a concern only one indicator cares about. The codebase already keeps such focused, polled/UI state in dedicated singletons (`ChannelRelaysModel`, `ConnectProgressManager`, `ChatItemDummyModel`); `SubsStatusModel` follows that pattern. The poll still publishes **only on change** (so the indicator does not re-render every idle second), which is why `SMPServerSubs` is made `Equatable`:

```swift
func update(_ totalSubs: SMPServerSubs, _ hasSession: Bool) {
    if self.totalSubs != totalSubs || self.hasSession != hasSession {
        self.totalSubs = totalSubs
        self.hasSession = hasSession
    }
}
```

The poll stays in the view; its suspension gating (`AppChatState == .active`) and `onDisappear` cancellation are unchanged, so there is no lifecycle regression. A model-owned poller (started/stopped with `ChatReceiver`) would also de-duplicate the poll and drop the `.onAppear` dependency, but that is a larger change not required to fix the reported bug.

## Scope

- `apps/ios/Shared/Model/ChatModel.swift` — add `SubsStatusModel` singleton (no change to `ChatModel`'s own fields).
- `apps/ios/Shared/Model/AppAPITypes.swift` — `SMPServerSubs: Equatable` (for the publish-on-change guard).
- `apps/ios/Shared/Views/ChatList/ChatListView.swift` — indicator observes `SubsStatusModel`; the poll calls `update(...)`.

No change to the status colour/percentage logic or the detail view, and `ChatModel` is untouched.

## Verification

- On the chat list, the indicator shows the real subscription percentage (matching the servers summary) instead of a bare `%`, and stays correct as the chat list updates / the toolbar rebuilds.
- When subscriptions are unchanged and idle, the indicator is not re-rendered every second (publish-on-change), and `ChatModel`'s observers are never touched by the poll.
- iOS build/verify on macOS (Xcode) — not compilable on the Linux build host used here.
