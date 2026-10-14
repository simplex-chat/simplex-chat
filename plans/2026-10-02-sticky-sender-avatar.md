# Sticky sender avatar in group chats

## Problem

In a group, a sender's consecutive messages show one avatar, on the first (top) message. When the sender writes several messages, or one long one, the avatar scrolls away with the first message while the rest of their messages are still on screen, and nothing near them says who wrote them.

## Behaviour

- A sender's consecutive received messages form a run (the same run that today shows a single avatar: time gaps and date separators don't break it).
- When the top of the run scrolls under the top bar, the avatar stays 4dp/pt below the top bar (or the reports/support bar, or the window top in one-hand UI) and scrolls with the run.
- The bottom of the run's last row pushes it up; then the next sender's avatar takes over. A single tall message behaves the same way.
- Tapping the pinned avatar opens member info, as before, and drags and wheel scrolling over it scroll the list.
- Groups only (including business and member-support chats). Channels are unchanged: follow-on channel rows have no avatar column, so a pinned avatar would cover text.

## Design

One rule on both platforms: the avatar's top is `max(naturalTop, min(belowTopBar, runBottom − avatarSize))`.

### Android and desktop (Compose)

- **The run's first row moves its own avatar.** A layout modifier on the row's avatar reads its `coordinates` during placement. Compose re-runs such placement whenever an ancestor moves (`notifyChildrenUsingCoordinatesWhilePlacing`), so the avatar follows the scroll in the same frame, with no overlay and no extra state. The avatar draws outside its row; rows are not clipped and older rows are placed after newer ones, so it is drawn and hit-tested above the rows it covers. It stays clickable as before.
- **A stand-in when that row is gone.** LazyColumn disposes the first row once it scrolls out, but the run can be taller than the screen. An overlay above the list draws the same avatar at the same position only while the run's first row is not in `visibleItemsInfo`, so exactly one of the two is drawn and the hand-off is seamless. It is composed per member (compared by `memberId`, since messages of one sender carry different member snapshots) in a composable lambda, which has its own restart scope, so its reads don't recompose the chat list; it is shown or hidden during placement, which runs in the same frame as the list.
- **The stand-in is clickable without blocking scrolling.** A no-op pointer-input node with `sharePointerInputWithSiblings() = true` lets the list under it still receive drags and wheel events.
- Run geometry (`senderRunBottom`, `senderRunStartVisible`) only walks visible items, so the per-frame cost does not depend on run length.

Rejected:
- Overlay only, with the row's avatar hidden: it needs the avatar's position inside the row in the overlay, i.e. shared mutable layout state.
- An avatar composed in every row of the run: Android decodes the profile image on every composition (`base64ToBitmap` has no cache there).
- `stickyHeader`: it pins to the start of the list, which is the bottom in this reversed list, and it adds list items, while the code relies on one list item per merged item.

### iOS (SwiftUI)

- Rows are SwiftUI views inside UIKit cells positioned by `EndlessScrollView`, so a per-frame offset inside a cell would trail the cell by a frame and jitter. Instead an overlay in `ChatView` draws the pinned avatar, and the first row's own avatar is hidden (opacity) while it is pinned.
- The pin is computed in the existing scroll listener (`onChatItemsUpdated`, called from `layoutSubviews` on every scroll frame) and published only when it changes. Rows observe only the hidden-row id, so pushing the avatar up does not re-render every visible row.
- Rows report where their avatar is inside the row (named coordinate space), and the pin is recomputed when that changes.
- The overlay is above the scroll view, so it would swallow drags. It doesn't take touches; a tap recognizer on the scroll view begins only inside the pinned avatar, doesn't cancel other touches, and ignores a tap that stops a fling.

## Known limitations

- If the run's last message is the last one of a day, the avatar stops at the bottom of the date separator below it, not at the bubble.
- In selection mode, a run whose first message can't be deleted has its pinned avatar offset horizontally; in right-to-left languages the existing row selection offset is not mirrored, so the avatar shifts sideways when the stand-in takes over.
- In selection mode, tapping a pinned avatar can open member info instead of selecting the message under it (Android/desktop while the run's first row is still in the list).
- iOS: no pinned avatar in a pending member's "Chat with admins", where a title bar is drawn over the top of the list.
- iOS: SwiftUI may draw the overlay a frame after UIKit moves the cells, so the avatar can trail by one frame of scroll when it pins, unpins or is pushed up. When the avatar is pushed out at the bar, its last 4pt can disappear a frame early, and the row's own avatar can reappear blurred under the translucent bar once the next sender's messages reach it.

## Testing

- Desktop AppImage with a group of three CLI clients: runs of 15 short/long messages, 3 short, 1 message, and 8 long ones; member images set. Checked pinned position (bar edge y=55, avatar image y=64, i.e. 4dp plus the image's own inset, identical for row avatar and stand-in), pinning through runs taller than the screen, push-up at run ends, hand-off to the next sender, one-hand UI, click on the row avatar and on the stand-in, wheel scrolling over the stand-in.
- Tap/drag/wheel/fling over the stand-in also checked with real pointer events in a standalone Compose 1.8.2 harness.
- iOS was not built or run.
