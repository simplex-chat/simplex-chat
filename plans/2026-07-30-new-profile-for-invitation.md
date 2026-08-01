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

Response stays `CRActiveUser` — it carries the created user, which on this path is not
the active one. Documented at the field; no client decoder changes.

## 3. Commits

In branch order.

1. **parameterise the create-profile form's submit action** — `CreateProfile` chose
   between two submit paths internally, invisible at its two call sites. The callback
   passes *raw fields*, not a `Profile`: the two paths build different ones
   (no-profile-setup drops `shortDescr`), so a `Profile`-shaped callback would silently
   change behaviour. The branch itself moves to a named `createProfileFromForm` that both
   existing call sites delegate to — copying it into each would have left two copies to
   drift. Net +12 lines; the `chatModel` parameter was redundant anyway.
2. **fix: don't switch profile when the reassignment failed** — pre-existing bug.
   `changeActiveUser_` sat outside the null guards, so a failed reassign still switched
   profile and stranded the invitation. Reachable today. iOS unaffected (its API throws,
   so control flow skips the switch).
3. **core: `keepActiveUser`** — §2. Also regenerates three client artifacts and
   hand-syncs two more that no test covers (§5).
4. **core: tests** — `keepActiveUser` works; and omitting it still activates (guards an
   iOS-breaking regression, since iOS never sends the field).
5. **feature: surface 1** — see gotchas. The create-then-hand-off flow lives in a single
   `createProfileForInvitation(rhId, creating) { newUser -> … }` beside the other
   profile-creation helpers; both surfaces call it, differing only in what they do with
   the created profile. Written out twice it was ~35 duplicated lines free to drift.
6. **extract `selectProfile`** — pure move of the row handler's body out of the lambda,
   so the next commit can take the same path instead of duplicating it. No behaviour
   change; review it by checking the body is unchanged apart from indentation.
7. **feature: surface 2** — 32 added lines, nothing removed, now that the move is its own
   commit.
8. **fix: profile image lost when creating the first chat profile** — pre-existing on
   master, unrelated, safe to drop. Desktop-only: reachable just from the user picker's
   "Create chat profile" row, shown when there is no local profile. Onboarding never hits
   it — those screens take a name only.

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
- **`keepingChatId` does not open the chat**, it only preserves its place in the reloaded
  list. Set `chatModel.chatId.value = chat.id` after switching, or you land on the new
  profile's chat list instead of the invitation. The *old* `chat.id` stays valid because
  the reassignment updates in place — `updatePreparedContactUser` runs
  `UPDATE contacts SET user_id = ? WHERE contact_id = ?` and re-reads the same id, and the
  group path does the same for `group_id`. Do not "fix" this to use the returned chat.
- **In-flight flags must not be `rememberSaveable`, nor scoped to a lazy item** — either
  strands them `true` (process death skips the resetting `finally`; scrolling disposes
  the item) and the row dies permanently.
- **`listUsers` throws and `withBGApi` does not catch** (`wrapWithLogging` has no catch),
  so an exception after creation aborts silently. Use `runCatching` for the cosmetic
  refresh, and the safe `changeActiveUser` wrapper rather than `changeActiveUser_`.
- **Create first, close only on success** — the shared `createUser` helper behind both
  wrappers shows its own alert and returns null, so dismissing first discards everything
  typed on a duplicate name. Guard dismissal-during-creation with a `ModalViewId` +
  `isLastModalOpen`, or a back-tap still switches profile.
- **Stale remote host**: an older core ignores the unknown field and activates anyway.
  There is no version to gate on — but the response carries `activeUser`, so check it and
  resync rather than issuing a reassign that must fail.
- Reassignment can **rename** the contact (`alice` → `alice_1`), likelier here because a
  new profile always has the SimpleX Team/Status cards. Core returns the updated contact;
  do not pre-check.

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
| Back/Esc during creation | no profile switch; modals unwind cleanly |
| Desktop: tap a row while the form is open | no concurrent switch |
| One-time link → Share profile → Add profile | link is regenerated |
| Airplane mode | profile is created; connect fails as today |
| Older remote host | detected via `newUser.activeUser`; resyncs and reports |

**Every user-visible bug in this feature was found by running the app** — none by
compilation, the test suite, or ten rounds of review.

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
- **`onSubmit` is optional**, not required — it touches no existing call site, which
  matters more when the change cannot be compiled.
- **The two create flows are *not* extracted into one**, unlike Kotlin. The 12 identical
  lines include `defer { creating = false }`, which must stay in the function that owns
  the whole flow — inside a helper it would fire on the helper's return, clearing the
  in-flight flag before the caller has reassigned and letting a second submit through.
  That leaves ~5 safely extractable lines, and the rest differs anyway (alert mechanism:
  global `showAlert` vs the view's `alert` state; final action; an extra `profiles`
  refresh). Kotlin's extraction was worth it because the shared flow was 35 lines and
  compile-checked; this one is neither.
- Reuses `"Add profile"` (16 locales) and `"Error changing chat profile"` (10) — again
  zero new translation entries.

Swift-specific traps found while reviewing, all fixed:

- **Two `.sheet` modifiers on one view conflict** in SwiftUI, and both pickers already
  present `IncognitoHelp` from the root — so the new sheet goes on the picker itself: a
  descendant of the root, but *not* the row. Rows live in a `LazyVStack`/`List`, which
  may dispose them and take the presented sheet with them.
- **Surface 1's picker height is computed from the row count** —
  `USER_ROW_SIZE * min(MAX_VISIBLE_USER_ROWS, users.count + 1)` — unlike Kotlin's
  content-sized `heightIn(max = ...)`. Adding a row without making that `+ 2` clips one,
  and since the list scrolls to `BOTTOM_ANCHOR` on appear, the clipped one is "Add
  profile" at the top: invisible to exactly the single-profile user this is for.
- **Trailing-closure syntax binds to the last init parameter**, which for a memberwise
  init is not necessarily `onSubmit`; both call sites pass `onSubmit:` explicitly.
- `if creatingProfile { … }` then setting it is a **non-atomic check-and-set**, and reads
  `@State` off the main actor; both are done inside one `MainActor.run`.
- **Presenting an alert while a sheet is dismissing swallows it.** The form is dismissed
  only on the success path, so failures leave it open with the alert over it — which is
  also what any other failure does.
- The `onChange(of: selectedProfile)` handler that surface 2 reuses **returns early unless
  `profileSwitchStatus == .switchingUser`**, so both must be assigned in the same
  `MainActor.run` before SwiftUI's next update.

⚠ **No Swift toolchain on the machine this was written on**, so none of it is compiled or
run — treat it as unverified until it builds in Xcode, for the reason at the end of §6.
First thing to check: open a prepared chat with **one** profile and confirm "Add profile"
is visible without scrolling.
