# Feature: create a new profile when choosing a profile for an invitation

Branch `nd/new-profile-for-invitation`, PR #7329. Core, Android/desktop and iOS —
the iOS part is unverified, see §7.

> Accept this invitation as someone new.

Every place that asks "which profile for this connection?" should also let you answer
"a new one", without leaving the invitation to make one in Settings. The people who
benefit most have a *single* profile — for them, expanding the picker today shows one
profile and Incognito and nothing else.

## 1. Surfaces

| # | Surface | In scope |
|---|---|---|
| 1 | Prepared contact/group from a link (`ComposeContextProfilePickerView`) | **yes** |
| 2 | "Share profile" on a one-time link (`ActiveProfilePicker` from `InviteView`) | **yes** |
| 3 | Legacy connect alerts, links without short-link data (`askCurrentOrIncognitoProfileAlert`) | follow-up |
| 4 | Choose profile to share content into (`ShareListView`) | no — a new profile has no chats to share into |
| 5 | Accept an incoming contact request | **not possible** — see below |
| 6 | Connect via a contact card's address | deferred — needs re-preparing an existing contact |
| 7 | Accept a group invitation | no choice exists; identity fixed by `membership.memberIncognito` |
| 8 | App-wide profile switcher | already has "Add profile" |

Coverage follows the link format: links **with** short-link data prepare a chat
(surface 1); those without fall to the legacy alert (surface 3, follow-up).

**Contact requests (5) — state this precisely in the PR.** Accepting with a new
*incognito* profile already ships. Accepting with a new *chat profile* is impossible:
`APIAcceptContact` has no `UserId` and resolves the request under the active user, and
accept-then-move fails because an accepted request already has a connection. The reason
is protocol, not a missing command — the requester already sent their confirmation to a
queue owned by that profile's address, and an in-flight queue cannot move across agent
users, only be recreated (which changes the link). Any future feature here is "reject
and re-invite from another profile", not "accept as another profile".

## 2. Why `keepActiveUser` is needed

Two facts collide:

- **Creating a user activates it** — the handler hardcoded `True` for the `activeUser`
  parameter `createUserRecordAt` already had, which deactivates the rest and sets the
  `currentUser` TVar. (The `UPDATE users SET active_user = 0` is gated on it.)
- **The reassignment resolves the chat under the *active* user** —
  `APIChangePreparedContactUser` is `withUser $ \user -> getContact db cxt user contactId`.

So create-then-reassign is dead: the profile that owns the invitation is no longer
active, and the chat cannot be found.

Rejected alternatives:

- **Create, switch back, reassign, switch forward.** No core change, two defects: it
  fails for a **hidden** current profile (`validateUserPassword_` accepts a missing
  password only when the *current* user matches, and by then it doesn't — the UI has no
  password to offer); and it opens a window where core's active user disagrees with
  `chatModel.currentUser`, so any concurrent `withUser` command runs as the wrong profile.
- **Re-prepare the link under the new profile.** Dead on a fact: `PreparedContact` keeps
  only `connLinkToConnect`, not the `contactShortLinkData` that `apiPrepareContact`
  requires — so it needs a fresh network fetch and **fails offline**.

With the flag the sequence reads as the story, with nothing to undo: create (active user
untouched) → reassign (old profile still active, still owns it) → switch once.

Known, deliberately not fixed: `activateNewUser` comes from a `readTVarIO`, and
`deleteChatUser` clears `currentUser` when the *only visible* profile is deleted — so a
concurrent delete could leave none active. It needs deleting your last profile while
creating one from an invitation picker, which the UI cannot reach; master has the same
window with the opposite outcome.

`BoolDef` gives `omittedField = False`, so absent = today's behaviour and iOS, the CLI
and older callers are untouched. The flag is ignored when there is no active user to keep,
which would otherwise leave none at all.

Two limits of the flag, both deliberate and neither covered by a test:

- It is **ignored when there is no active user** (`isNothing curUser_`), which would
  otherwise leave none at all. A client cannot distinguish that from a stale host that
  dropped the field — both come back as `activeUser = True`.
- `active_order = 0` for a profile that was never activated sorts it **below** activated
  ones, but `M20240920_user_order` back-filled every existing row with 0, so on a migrated
  database it *ties* with them and the order falls back to row order. `userQuery` has no
  `ORDER BY`, and the terminal harness cannot observe `activeOrder`, so this is stated
  rather than tested.

Response stays `CRActiveUser` — it carries the created user, which on this path is not
the active one. Documented at the field; no client decoder changes.

## 3. What the branch contains

- **core** — `keepActiveUser` (§2), its two tests, and the regenerated client artifacts
  plus the two hand-synced ones no test covers (§5).
- **both surfaces** — the "Add profile" row, and the shared
  `createProfileForInvitation(rhId) { newUser -> … }` beside the other profile-creation
  helpers. Both call it and differ only in what they do with the created profile; written
  out twice it was ~35 duplicated lines free to drift.
- **`CreateProfile` gains an optional submit callback**, passing *raw fields* rather than
  a `Profile`: the two existing paths build different ones (no-profile-setup drops
  `shortDescr`), so a `Profile`-shaped callback would silently change behaviour. Optional,
  so neither existing call site changes.
- **`ActiveProfilePicker`'s row handler is extracted** so the create flow takes the same
  path instead of duplicating it. Pure move — review it by checking the body is unchanged
  apart from indentation.
- **two pre-existing fixes ride along**, each in its own commit and each droppable: a
  failed prepared-chat reassignment used to switch profile anyway and strand the
  invitation (reachable today, and worse once a profile has just been created for it);
  and the first chat profile's avatar was stored as its description (desktop-only, from
  the user picker's "Create chat profile" row).

Uses the existing `users_add` ("Add profile") string — **zero new translation keys**.

## 4. Implementation gotchas

- **Positional arguments cost a bug.** `Profile` is
  `(displayName, fullName, shortDescr, description, image, …)`. Passing `image`
  positionally puts it in **`description`** — it type-checks, both are `String?`, and the
  avatar silently vanishes. Always `image = image`. (Swift cannot hit this; its init
  requires labels.)
- **Row placement is layout-dependent.** Surface 1's list is `reverseLayout = true`, so
  the row must be emitted **last** to render at the top. Surface 2's is not reversed, and
  the row must sit **outside** the `activeProfile != null` branch or it disappears
  whenever search text filters the active profile out.
- **The modal must open in `ModalManager.fullscreen`.** Every other placement is wrong on
  desktop, where the four managers are four panes: `center` sets `ChatModel.chatId = null`
  (`ModalView.kt`) and the chat view **is** the centre pane, so the form closed the very
  invitation it was for, taking the typed compose draft with it, and only the success path
  reopened it. `end` leaves the compose picker live in the pane beside the form — and
  `apiChangeConnectionUser` recreates the connection, so a row tapped meanwhile
  invalidates the `pccConnId` the form is about to use — and its
  `desktopExpandWindowToWidth` widens the window for good. `start` works for surface 2 but
  disposes that picker, losing its search text. `fullscreen` is an opaque `Surface` over
  every pane: no picker can be operated while the form is up, none is torn down, and
  nothing else moves. On Android all four are the same manager regardless.
- **`keepingChatId` does not open the chat**, it only preserves its place in the reloaded
  list. It does not need to: the earlier claim that you land on the chat list without an
  explicit `chatModel.chatId.value = chat.id` was the `center` placement above, not
  `keepingChatId`. Keep the assignment, but only once the switch is known to have
  happened — `updateChats` deliberately clears `chatId` when the id is absent from the
  reloaded list. The *old* `chat.id` stays valid because the reassignment updates in
  place — `updatePreparedContactUser` runs
  `UPDATE contacts SET user_id = ? WHERE contact_id = ?` and re-reads the same id, and the
  group path does the same for `group_id`. Do not "fix" this to use the returned chat.
- **In-flight flags must not live in the picker's composition at all**, and must not be
  released when the profile exists. `rememberSaveable` strands them `true` (process death
  skips the resetting `finally`) and a lazy item is disposed by scrolling — but so is the
  whole picker: on Android every `ModalManager` placement shares one stack and
  `showInView` renders only the top entry, so pushing the form disposes surface 2's
  picker (a modal) and it returns with every `remember` reset. Surface 1's picker is not
  a modal and survives on both platforms; the flag is shared anyway. Hence
  `ChatModel.creatingProfileForInvitation` — in the model, not a top-level `val`, so it
  is not first created inside a composition — and a **suspending** `onCreated` so the flag
  covers the reassignment rather than just the creation (`changeProfile`/`selectProfile`
  only *launch* it). Hand `onCreated` back to `Dispatchers.Main` before calling it: it
  reassigns and switches, and every structure it touches is also written by the receiver
  loop on Main.
- **Re-check the active user and the host before reassigning.** The reassignment resolves
  the invitation under whatever is active when it runs, and nothing holds
  `changingActiveUserMutex` across this flow: a notification tap
  (`NtfManager.acceptContactRequestAction`) switches the user from another dispatcher, and
  a remote host connect/disconnect switches the host and closes modals.
- **`listUsers` throws and `withApi` does not catch** (`wrapWithLogging` has no catch),
  so an exception after creation aborts silently. Use `runCatching` for the cosmetic
  refresh, and the safe `changeActiveUser` wrapper rather than `changeActiveUser_`.
- **Create first, close only on success** — `apiCreateActiveUser` shows its own alert and
  returns null, so dismissing first discards everything typed on a duplicate name. Guard dismissal-during-creation with a `ModalViewId` +
  `isLastModalOpen` — and when that check fails, **stop**: continuing merely without
  closing still reassigns and switches under a screen the user has left. The check also
  has to ignore modals already staged in `toRemove`, or during the ~250ms close animation
  a dismissed form still reads as open and the second `close()` pops the screen beneath.
- **Only switch or dismiss once the invitation actually moved.** Surface 2 did both
  unconditionally. Unlike the prepared-chat reassignment, which is pure DB,
  `APIChangeConnectionUser` → `recreateConn` provisions a new queue — so it is what fails
  offline, while creation, being local, always succeeds. Dismissing there strands the
  profile just created with nothing pointing at it. (`appPrefs.incognito` is cleared
  before that check, as on master — moving it is a separate, pre-existing fix.)
- **Stale remote host**: an older core ignores the unknown field and activates anyway.
  There is no version to gate on — but the response carries `activeUser`, so check it and
  resync rather than issuing a reassign that must fail.
- Reassignment can **rename** the contact (`alice` → `alice_1`), likelier here because a
  new profile always has the SimpleX Team/Status cards. Core returns the updated contact;
  do not pre-check.

Two hazards left as they are on master, so review does not keep re-raising them:

- `selectProfileAsync`'s trailing `close()` pops whatever is on top rather than the
  picker, so backing out during the connection change dismisses the screen underneath.
  The ordinary row tap has had exactly that window since before this branch; creating a
  profile first is local and fast and barely widens it. Fixing it needs a `ModalViewId`
  on the picker at both call sites, including `ShareListView`, which does not offer this
  feature at all.
- `alertAfterDismissal` waits a fixed 0.5s for a sheet transition rather than observing
  it. A slow device or a late-released interactive dismissal can still outlast it. The
  deterministic version needs the presenting controller's completion handler, which is
  not reachable from where these alerts are raised.

## 5. Generated and hand-synced artifacts — easy to miss

`NewUser` is a documented API type, and `apiDocsTest` generates **11 files** from those
definitions (markdown, TypeScript and Python clients). Adding one field changed three of
them, one line each. `testGenerate` writes the file *then* asserts — so a run repairs a
stale artifact, and **an unchanged tree afterwards is the real assertion**.

The generator emits `BoolDef` fields as **required** in the client types (unlike `Maybe`,
which becomes `profile?`/`NotRequired`). So the two client libraries that are kept in sync
by hand — `simplex-chat-python/api.py` and `simplex-chat-nodejs/api.ts`, both of which
build a `NewUser` literal listing every bool — stop type-checking until the new field is
added there too. Neither is generated, so no test catches it; the precedent is
`a4e3a1ea1`, which added `clientService` to both. (`simplex-chat-client/typescript` has
its own separate types and is deliberately untouched — it was not updated for
`clientService` either.)

**Before touching any type in `Simplex.Chat.Types`, run the `Bot API docs` tests.** The
Haskell compiles fine without them; only that test catches the drift, and only a full run
does — a targeted selection is not regression coverage.

**Fixed on master since this was written.** `CPTUnknown Text` (`61012d208`) made the
generator `error` on an enum constructor with fields — and since the write precedes the
assert, running the suite **emptied three checked-in files** (~13,500 lines). `31faceef7`
excludes `CPTUnknown`, as `BTUnknown` already was. On older bases expect the truncation;
restore the files, never commit it.

Do **not** commit the regenerated `*_query_plans.txt` — stale on master for unrelated
reasons, and the suite rewrites them on every run (confirmed again on this base).

## 6. Testing

Full suite, re-run after rebasing onto `34b74d1c4`: **986 examples, 6 failures, 41
pending** (it was 981 at base `cbae9c5e8`; master's `DirectoryTests.hs` added the 5). The
six failures are the *same six* both times — multi-user TTL ×3, broadcast bot, multicast
discovery, query plans — so the branch adds **no regressions**. The TTL ones fail
reproducibly in isolation, not only under load; only a master control settled that. That
control was run on the older master; it still stands, since the set is unchanged and the
branch touches none of those paths.

`Bot API docs` passes 17/17 leaving the tree unchanged, which is what proves the three
regenerated artifacts match the current generator. Both new `keepActiveUser` tests pass.

Manual matrix (Android + desktop) — the part that actually finds bugs:

| Case | Expect |
|---|---|
| Prepared contact/group → Add profile → connect | connects as the new profile; **lands in the chat**, not the chat list |
| Chosen avatar | becomes the **profile picture**, not the description |
| Row position | top of the expanded list (screenshot it; reversed layout) |
| Dividers, with **one** profile | row follows the divider-above-each-row convention: a line at the top edge, none below it — same gap master has above the selected row. Confirm it reads as intentional |
| Scroll row out of view and back, then tap | still works |
| Duplicate name | typed name and avatar survive; form stays open |
| Hidden current profile | works, no password prompt |
| Back/Esc during creation | no profile switch; modals unwind cleanly — and the screen *under* the form is still there |
| Desktop: cancel the form | the invitation chat is still open, with the typed draft intact |
| Desktop: tap a row while the form is open | not possible — the form is in the same pane as the picker |
| One-time link → Share profile → Add profile | link is regenerated |
| Airplane mode | profile is created; **the connection change fails**, so the picker stays open and no switch happens (creation is local, `recreateConn` is not) |
| Switch profile from a notification mid-create | no reassignment; reported, profile left created |
| Second tap while the switch is in flight | row disabled, spinner shown, no second profile |
| Older remote host | detected via `newUser.activeUser`; resyncs and reports |

**Every user-visible bug in this feature was found by running the app** — none by
compilation, the test suite, or ten rounds of review. A later adversarial review pass
did find the rest of §4 statically, but only after the flow was written down.

**Known, not fixed.** `withBGApi` builds a detached scope that nothing cancels and no
Android lifecycle callback stops, so backgrounding right after Create lets the flow
finish unattended — the active profile switches while the app is invisible. Process
death between creation and the reassignment leaves a complete, non-active, empty
profile (with its preset cards and note folder) and the invitation still on the old
one, with nothing to reconcile it. Deleting it in Settings is the only recovery. This
failure mode is new: before `keepActiveUser`, creation always activated immediately, so
a half-finished create was self-evident. Fixing it needs either a cancellation-aware
scope or a reconciliation pass, neither of which belongs in this branch.

## 7. iOS — done, but **never compiled**

Three commits mirroring 1/5/7. Commit 2 has no iOS counterpart (already correct: the API
throws, so a failed reassign skips the switch — and the neither-direct-nor-group
fall-through is unreachable, since `nextConnectPrepared` gates the picker and is false for
every other case), and the positional-argument bug cannot occur — Swift's `Profile.init`
requires labels.

Differences from Kotlin, each deliberate:

- **Surface 1's row goes first**, not last: that list is not reverse-laid-out, so
  emitting first is what puts it at the top. Surface 2's row stays **last** on both
  platforms — that list is not reversed on either, so last means bottom on both.
- **The two create flows are *not* extracted into one**, unlike Kotlin. The 12 identical
  lines include `defer { creating = false }`, which must stay in the function that owns
  the whole flow — inside a helper it would fire on the helper's return, clearing the
  in-flight flag before the caller has reassigned and letting a second submit through.
  That leaves ~5 safely extractable lines, and the rest differs anyway (final action; an
  extra `profiles` refresh). Kotlin's extraction was worth it because the shared flow was
  35 lines and compile-checked; this one is neither.
- **`changeProfile` keeps its `Task`**, so the compose picker needs a second flag where
  Kotlin's suspending `onCreated` needs none. Making it `async` and awaiting it removes
  the flag but lifts the body out of the `Task` and re-indents the whole function — more
  diff than it saves.
- Reuses `"Add profile"` (16 locales) and `"Error changing chat profile"` (10) — again
  zero new translation entries.

Swift-specific traps found while reviewing, all fixed:

- **The sheet's owner must outlive the sheet.** Rows live in a `LazyVStack`/`List` and can
  be disposed, taking the presented sheet with them — but so can `profilePicker()`, which
  is swapped for `currentSelection()` the moment `listExpanded` flips, which
  `changeProfile` does while the sheet is still dismissing, and which an incoming event
  can do at any time via `profileChangeProhibited`. It goes on the `Group` in `viewBody()`,
  which survives both. Two `.sheet` modifiers on one chain conflicted only before iOS
  14.5; the app targets 15, so stacking it with the root's `IncognitoHelp` is fine.
- **Surface 1's picker height is computed from the row count** —
  `USER_ROW_SIZE * min(MAX_VISIBLE_USER_ROWS, users.count + 1)` — unlike Kotlin's
  content-sized `heightIn(max = ...)`. Adding a row without making that `+ 2` clips one,
  and since the list scrolls to `BOTTOM_ANCHOR` on appear, the clipped one is "Add
  profile" at the top: invisible to exactly the single-profile user this is for.
- **Trailing-closure syntax binds to the last init parameter**, which for a memberwise
  init is not necessarily `onSubmit`; both call sites pass `onSubmit:` explicitly.
- `if creatingProfile { … }` then setting it is a **non-atomic check-and-set**, and reads
  `@State` off the main actor; both are done inside one `MainActor.run`.
- **A SwiftUI `.alert(item:)` on the view presenting the sheet never appears.** UIKit
  refuses an alert on a controller that already has a presentation, and the form is still
  up on every failure path. Use the global `showAlert`, which targets the top view
  controller and therefore draws over the sheet — surface 2 originally used its own
  `alert` state here and the "core ignored keepActiveUser" report was silently dropped.
- **Only the switch failed, not the creation.** `changeActiveUserAsync_` throws, and on
  the resync path that propagated into the form's catch, which reports "Error creating
  profile!" for a profile that exists. Catch it there.
- **The submit `Task` is unstructured**, so SwiftUI does not cancel it on dismissal:
  swipe-to-dismiss mid-create still created the profile and switched to it while every
  `MainActor.run` write, including the one clearing the in-flight flag, landed on a view
  that was gone. `.interactiveDismissDisabled(creatingProfile)` on both sheets.
- **`@State` snapshots taken in `onAppear` go stale.** Surface 1's `users` is filled once
  and is what the row list *and* the `frame(maxHeight:)` row count read, so refresh it
  wherever `chatModel.users` is refreshed — and do not name the `listUsers` result
  `users`, which is what hid this.
- The `onChange(of: selectedProfile)` handler that surface 2 reuses **returns early unless
  `profileSwitchStatus == .switchingUser`**, so both must be assigned in the same
  `MainActor.run` before SwiftUI's next update.

⚠ **No Swift toolchain on the machine this was written on**, so none of it is compiled or
run — treat it as unverified until it builds in Xcode, for the reason at the end of §6.
First thing to check: open a prepared chat with **one** profile and confirm "Add profile"
is visible without scrolling.
