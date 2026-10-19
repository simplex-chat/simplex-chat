# Pop chat collector reads the primary chat list

## Problem

Each `ChatsContext` (the main chat list, and the secondary context of a group's reports view or a member support chat — "Chat with admins" or an admin's chat with a member) has its own `PopChatCollector`, which moves recently active chats to the top of that context's list. The collector was a nested class that took its context as a constructor parameter, and only its `init` block used that parameter. In `popCollectedChats()` the unqualified `getChat` and `chats` resolve to the `ChatModel` object's members, which read the primary list.

So in a secondary context the collector builds a reordered copy of the primary list and installs it as the secondary list. It runs, for example, when a chat that is not first in the secondary list gets a new item: incoming items for any chat are also sent to the secondary context, which adds chats it does not have. `ChatView` takes the open chat's unread count from its context's list, and a group's id is the same in both lists, so after the replacement the reports view or "Chat with admins" shows the group's main-chat unread count (the counter on the top-right scroll-to-unread button).

The main list is not affected: there `chats` is the same list. iOS keeps a single chat list and is not affected.

## Fix

Make `PopChatCollector` an `inner` class, so `getChat` and `chats` resolve to the context that owns it. `ChatsContext` declares no other name the collector uses, so nothing else is rebound. The `TODO` asking whether the nesting is needed is removed: now it is.

## Alternatives considered

- Qualify the two calls through a stored context: same effect, but a new method in the class could make the same mistake silently.
- Move the class to top level and pass the context: a misplaced name would not compile, but it moves ~45 lines to fix one.
- Do not reorder secondary contexts at all: the UI looks chats up by id, not by order, but it changes behaviour at several call sites.
- Fold into #7072: different lines and a different symptom (#7072 fixes writes to the wrong row of the main list).

## Stacked on #7072

On master this bug's copy re-aligns the secondary list's indices with the main list after each pop, which hides some of the wrong-row writes that #7072 fixes. Tested on master alone, this fix made them visible: with "Chat with admins" open, a burst of messages from two contacts put a preview on the wrong row of the main list in 3 of 5 bursts (master: 0 of 5). So this PR is stacked on #7072 and should be merged after it.

## Not included

- After this fix these views show no main-chat unread count; a count of their own messages would be a separate feature.

## Testing

Desktop AppImages: in a group with "Chat with admins" open, receive messages so that a chat other than the first in that context's list gets a new item; master shows the group's main-chat unread count on the view's top-right button, this fix does not. Run on master vs this fix alone, and on #7072 vs this PR (stacked) together with the burst check above. Regression: main-list reordering with and without that view open, closing and reopening it, switching profiles, stopping and starting chat. Android APK (master vs this fix alone): the same main check.
