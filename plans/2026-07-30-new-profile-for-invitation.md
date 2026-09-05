# Feature: create a new profile when choosing a profile for an invitation

Branch `nd/new-profile-for-invitation`, PR #7329.

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
| 3 | Legacy connect alerts, links without short-link data | follow-up |
| 4 | Choose profile to share content into (`ShareListView`) | no — a new profile has no chats to share into |
| 5 | Accept an incoming contact request | **not possible**, see below |
| 6 | Connect via a contact card's address | deferred — needs re-preparing an existing contact |
| 7 | Accept a group invitation | no choice exists; identity fixed by `membership.memberIncognito` |
| 8 | App-wide profile switcher | already has "Add profile" |

Coverage follows the link format: links **with** short-link data prepare a chat
(surface 1); those without fall to the legacy alert (surface 3).

Accepting a contact request (5) with a new *chat profile* is impossible, and the reason
is protocol rather than a missing command: the requester already sent their confirmation
to a queue owned by that profile's address, and an in-flight queue cannot move across
agent users, only be recreated (which changes the link). Any future feature here is
"reject and re-invite from another profile".

## 2. Why `keepActiveUser` is needed

Two facts collide: creating a user activates it, and the reassignment resolves the chat
under the *active* user (`APIChangePreparedContactUser` is
`withUser $ \user -> getContact db cxt user contactId`). So create-then-reassign is dead —
the profile that owns the invitation is no longer active, and the chat cannot be found.

Rejected alternatives:

- **Create, switch back, reassign, switch forward.** No core change, two defects: it
  fails for a **hidden** current profile (`validateUserPassword_` accepts a missing
  password only when the *current* user matches, and by then it doesn't — the UI has no
  password to offer); and it opens a window where core's active user disagrees with
  `chatModel.currentUser`, so any concurrent `withUser` command runs as the wrong profile.
- **Re-prepare the link under the new profile.** Dead on a fact: `PreparedContact` keeps
  only `connLinkToConnect`, not the `contactShortLinkData` that `apiPrepareContact`
  requires — so it needs a fresh network fetch and **fails offline**.

With the flag: create (active user untouched) → reassign (old profile still active, still
owns it) → switch once. `BoolDef` gives `omittedField = False`, so absent is today's
behaviour and older callers are untouched. The flag is ignored when there is no active
user to keep, which would otherwise leave none at all.

## 3. Implementation constraints

Each of these cost a bug or a review round.

- **Positional arguments cost a bug.** `Profile` is
  `(displayName, fullName, shortDescr, description, image, …)`, so passing `image`
  positionally puts it in **`description`** — it type-checks, both are `String?`, and the
  avatar silently vanishes. (Swift cannot hit this; its init requires labels.)
- **The Kotlin form must open in `ModalManager.fullscreen`.** On desktop the four managers
  are four panes: `center` sets `ChatModel.chatId = null` and the chat view *is* the centre
  pane, so the form closes the very invitation it was for, taking the compose draft with
  it. `end` leaves the picker live beside the form, and `apiChangeConnectionUser`
  recreates the connection, so a row tapped meanwhile invalidates the `pccConnId` the form
  is about to use. `start` disposes surface 2's picker, losing its search text. On Android
  all four are the same manager regardless.
- **Row placement is layout-dependent.** Surface 1's Kotlin list is `reverseLayout = true`,
  so the row is emitted **last** to render at the top; iOS's is not reversed, so it is
  emitted **first**. Surface 2's row must sit **outside** the `activeProfile != null`
  branch or it disappears whenever search text filters the active profile out.
- **Surface 1's iOS picker height is computed from the row count** (`users.count + 2`),
  unlike Kotlin's content-sized `heightIn(max = …)`. Without the `+ 2` a row is clipped,
  and since the list scrolls to `BOTTOM_ANCHOR` on appear the clipped one is "Add profile"
  at the top: invisible to exactly the single-profile user this is for.
- **The iOS sheet's owner must outlive the sheet.** Rows live in a `LazyVStack`/`List` and
  can be disposed, and so can `profilePicker()`, which is swapped for `currentSelection()`
  the moment `listExpanded` flips — which `changeProfile` does. Both `.sheet` modifiers go
  on the outer `body` chain, which survives.
- **Create first, close only on success.** `apiCreateActiveUser` shows its own alert and
  returns null, so dismissing first discards everything typed on a duplicate name.
- **`keepingChatId` does not open the chat**, it only preserves its place in the reloaded
  list. The old `chat.id` stays valid because the reassignment updates in place. Do not
  "fix" this to use the returned chat.
- Reassignment can **rename** the contact (`alice` → `alice_1`), likelier here because a
  new profile always has the SimpleX Team/Status cards. Core returns the updated contact.

## 4. Generated and hand-synced artifacts — easy to miss

`NewUser` is a documented API type, and `apiDocsTest` generates 11 files from those
definitions. Adding one field changed three, one line each. `testGenerate` writes the file
*then* asserts — so a run repairs a stale artifact, and **an unchanged tree afterwards is
the real assertion**.

The generator emits `BoolDef` fields as **required** in the client types (unlike `Maybe`),
so the two hand-synced clients — `simplex-chat-python/api.py` and
`simplex-chat-nodejs/api.ts`, both of which build a `NewUser` literal listing every bool —
stop type-checking until the field is added there too. No test catches it; the precedent
is `a4e3a1ea1`, which added `clientService` to both.

`simplex-chat-client/typescript` is deliberately untouched, and **not** because it has its
own types — it imports from the same package this branch regenerates, but its `^0.3.0` pin
resolves to the published `0.3.0`, which has no such field. Adding `keepActiveUser: false`
there would *break* it on excess-property checking. It was not updated for `clientService`
either.

**Before touching any type in `Simplex.Chat.Types`, run the `Bot API docs` tests** — only
that test catches the drift, and only a full run does. Do not commit the regenerated
`*_query_plans.txt`.

## 5. Testing

Both new `keepActiveUser` tests pass, and `Bot API docs` passes 17/17 leaving the tree
unchanged. Manual matrix (Android + desktop) — the part that actually finds bugs:

| Case | Expect |
|---|---|
| Prepared contact/group → Add profile → connect | connects as the new profile; **lands in the chat**, not the chat list |
| Chosen avatar | becomes the profile picture |
| Row position | top of the expanded list (screenshot it; reversed layout) |
| Dividers with **one** profile | a line at the top edge, none below it |
| Duplicate name | typed name and avatar survive; form stays open |
| Hidden current profile | works, no password prompt |
| Back/Esc during creation | no profile switch; the screen *under* the form is still there |
| Desktop: cancel the form | the invitation chat is still open, draft intact |
| One-time link → Share profile → Add profile | link is regenerated |
| Airplane mode | profile is created **and listed in Settings**; the connection change fails (creation is local, `recreateConn` is not) |
| Older remote host | the new profile is activated anyway, and the reassignment then fails |

⚠ **The iOS half has not been compiled** — no Swift toolchain on the machine this was
written on. First thing to check: open a prepared chat with **one** profile and confirm
"Add profile" is visible without scrolling.

## 6. Deliberately out of scope

Pre-existing master bugs, adjacent to this code, each needing its own PR rather than a
workaround here:

- **A failed reassignment still switches profile and dismisses the picker** (surface 2,
  both platforms): `selectProfileAsync` calls `close()` unconditionally, so when
  `apiChangeConnectionUser` returns null the connection stays on the old profile while the
  picker closes. Reachable today by tapping any row offline — creating a profile first
  makes it worse, because the new profile is then stranded with nothing pointing at it.
- **`IncognitoUserOption` writes `appPreferences.incognito` before the call that can
  fail** and never rolls it back, so a failed `apiSetConnectionIncognito` leaves the
  app-wide preference on. It surfaces on the *next* connection, which silently defaults to
  incognito.
- **The first chat profile's avatar is stored as its description** — the positional
  `Profile` argument above, in `createProfileInNoProfileSetup`.

- **No progress indicator on Android, surface 2 only.** `ModalManager.showInView` renders
  only the top modal, and on Android all four managers are the same one — so pushing the
  create form disposes the picker, and the picker that comes back has fresh `remember`
  state. `selectProfile` then writes `switchingProfile` on the disposed composition's
  object, so no spinner shows and the rows stay tappable for the one round trip the switch
  takes; a second tap in that window moves the connection twice (the switches themselves
  serialize on `changingActiveUserMutex`). Desktop is unaffected — `fullscreen` is a
  different manager from the `start` pane holding the picker, which is covered but not
  disposed. Surface 1's picker is not a modal and is unaffected on both. Closing this needs
  the flag hoisted out of the composition or a suspending `onCreated`, both of which cost
  more than the window is worth.

- **iOS surface 1: a fast reassignment failure can swallow its alert.** `createProfileForChat`
  dismisses the sheet and then calls `changeProfile`, whose errors go through the global
  `showAlert`, which targets `getTopViewController()` — and that walks
  `presentedViewController`, which stays set for the ~350ms dismiss animation. So an alert
  raised in that window is presented on the disappearing sheet and lost.
  `apiChangePreparedContactUser` is a local DB update and can return well inside it;
  `changeActiveUserAsync_` takes several round trips and is safely outside. The common
  error (duplicate name) is unaffected - it comes from `apiCreateActiveUser`, is caught in
  `CreateProfile` and shown on the form, which stays open. Fixing this needs either a fixed
  delay after dismissal or the presenting controller's completion handler, which is not
  reachable from where these alerts are raised.
- **Both `isLastModalOpen` guards have a ~250ms residual.** `closeModal` leaves a dismissed
  modal in `modalViews` until its close animation ends, so the check still reports it open
  for that window: backing out and having the create land inside it still reassigns, and
  re-opening the form within it is swallowed. Closing that needs a `toRemove`-aware variant
  of the check, which is more machinery than the remaining windows are worth.
- **Surface 2's `filteredProfiles` is `remember`ed on the search text only**, so on desktop
  (where the picker is not disposed under the form) a profile created while it is open is
  not listed. Invisible today because `selectProfile`
  closes the picker unconditionally - the very bug split out to its own PR. When that lands
  and the picker stays open on failure, this becomes visible; the fix is adding
  `chatModel.users.size` to the `remember` key, which preserves the "do not reorder after a
  user was selected" intent because selecting does not change the count.

Also not addressed, and not bugs in master:

- **A stale remote host** ignores the unknown field and activates the new profile anyway;
  the reassignment then fails with an alert. Detecting it via the response's `activeUser`
  costs more than the case is worth.
- **Backgrounding right after Create.** `withBGApi` builds a detached scope that nothing
  cancels. Process death between creation and the reassignment leaves a complete,
  non-active, empty profile and the invitation still on the old one; deleting it in
  Settings is the only recovery.
- **`Terminal/Input.hs` and `Terminal/Output.hs` cache any `CRActiveUser`** as the remote
  host's current user, so a CLI acting as a controller would cache a profile that is
  deliberately not active. Only reachable by typing `/_create user` with the raw JSON flag:
  both `/create user` and `/create bot` parse to `keepActiveUser = BoolDef False`, and
  `CRActiveUser` is a command response rather than an event, so it only reaches the client
  that issued it. Display-only either way.
