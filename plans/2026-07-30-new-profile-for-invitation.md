# Feature: create a new profile when choosing a profile for an invitation

Branch `nd/new-profile-for-invitation`, PR #7329. Core + Android/desktop; iOS follows.

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

- **Creating a user activates it** — `createUserRecordAt` runs
  `UPDATE users SET active_user = 0` and the handler sets the `currentUser` TVar.
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

`BoolDef` gives `omittedField = False`, so absent = today's behaviour and iOS, the CLI
and older callers are untouched. `createUserRecordAt` already took an `activeUser :: Bool`;
the handler was hardcoding `True`. The flag is ignored when there is no active user to
keep, which would otherwise leave none at all.

Response stays `CRActiveUser` — it carries the created user, which on this path is not
the active one. Documented at the field; no client decoder changes.

## 3. Commits

1. **parameterise the create-profile form's submit action** — `CreateProfile` chose
   between two submit paths internally, invisible at its two call sites. The callback
   passes *raw fields*, not a `Profile`: the two paths build different ones
   (no-profile-setup drops `shortDescr`), so a `Profile`-shaped callback would silently
   change behaviour. Net −6 lines; the `chatModel` parameter was redundant anyway.
2. **core: `keepActiveUser`** — §2. Also regenerates three client artifacts (§5).
3. **core: tests** — `keepActiveUser` works; and omitting it still activates (guards an
   iOS-breaking regression, since iOS never sends the field).
4. **fix: don't switch profile when the reassignment failed** — pre-existing bug.
   `changeActiveUser_` sat outside the null guards, so a failed reassign still switched
   profile and stranded the invitation. Reachable today. iOS unaffected (its API throws,
   so control flow skips the switch).
5. **feature: surface 1** — see gotchas.
6. **feature: surface 2** — extracts `selectProfile` so the new row takes exactly the
   same path as picking an existing profile.
7. **fix: profile image lost when creating the first profile** — pre-existing on master,
   unrelated, safe to drop.

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
  profile's chat list instead of the invitation.
- **In-flight flags must not be `rememberSaveable`, nor scoped to a lazy item** — either
  strands them `true` (process death skips the resetting `finally`; scrolling disposes
  the item) and the row dies permanently.
- **`listUsers` throws and `withBGApi` does not catch** (`wrapWithLogging` has no catch),
  so an exception after creation aborts silently. Use `runCatching` for the cosmetic
  refresh, and the safe `changeActiveUser` wrapper rather than `changeActiveUser_`.
- **Create first, close only on success** — `apiCreateActiveUser` shows its own alert and
  returns null, so dismissing first discards everything typed on a duplicate name. Guard
  dismissal-during-creation with a `ModalViewId` + `isLastModalOpen`, or a back-tap still
  switches profile.
- **Stale remote host**: an older core ignores the unknown field and activates anyway.
  There is no version to gate on — but the response carries `activeUser`, so check it and
  resync rather than issuing a reassign that must fail.
- Reassignment can **rename** the contact (`alice` → `alice_1`), likelier here because a
  new profile always has the SimpleX Team/Status cards. Core returns the updated contact;
  do not pre-check.

## 5. Generated artifacts — easy to miss

`NewUser` is a documented API type, and `apiDocsTest` generates **11 files** from those
definitions (markdown, TypeScript and Python clients). Adding one field changed three of
them, one line each. `testGenerate` writes the file *then* asserts it matched, so a stale
artifact fails the test and the run repairs it.

**Before touching any type in `Simplex.Chat.Types`, run the `Bot API docs` tests.** The
Haskell compiles fine without them; only that test catches the drift. This was missed for
four review rounds because earlier runs were *targeted* — a narrow selection is not
regression coverage.

⚠ **Master is currently broken here**: `CPTUnknown Text` (commit `61012d208`) makes the
generator `error` on an enum constructor with fields. Worse, `testGenerate` truncates the
file before the exception fires, so running the suite on master **empties three
checked-in files** (~13,500 lines). Restore them; never commit the truncation.

Do **not** commit the regenerated `*_query_plans.txt` — stale on master for unrelated
reasons, and the suite rewrites them on every run.

## 6. Testing

Full suite: **981 examples, 6 failures, 41 pending** — all six reproduce on a master
control (multi-user TTL ×3, broadcast bot, multicast discovery, query plans), so the
branch adds **no regressions**. The TTL ones fail reproducibly in isolation, not only
under load; only the control settled that.

Manual matrix (Android + desktop) — the part that actually finds bugs:

| Case | Expect |
|---|---|
| Prepared contact/group → Add profile → connect | connects as the new profile; **lands in the chat**, not the chat list |
| Chosen avatar | becomes the **profile picture**, not the description |
| Row position | top of the expanded list (screenshot it; reversed layout) |
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

## 7. iOS

Structure mirrors Kotlin. Commit 4 has **no** iOS counterpart (already correct), and the
positional-argument bug cannot occur. Needs: `NewUser` and `createActiveUser` gain the
field, an `apiCreateProfileKeepingActive`, the row in `ContextProfilePickerView` and
`ActiveProfilePicker`, and the same `chatId` auto-open. iOS's picker is **not**
reverse-laid-out — the row goes first to appear at the top.
