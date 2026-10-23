# Writing UI tests

Rules for the Compose UI tests in `common/src/uiTestShared` (both platforms), `common/src/desktopTest/.../ui/desktop`
(desktop only) and `android/src/androidTest/.../ui` (Android only). Every rule below comes from a mistake that was made
and found in review (`plans/ui-tests/reviews/`, plan §15.4). Read it before writing or changing a test; check the
list at the end before opening a PR.

How the harness works is in plan §0.5 (`plans/ui-tests/2026-10-10-desktop-ui-test-dsl-plan.md`); how to run the
tests is in `scripts/uitest.sh` and `scripts/uitest-android.sh`.

## 1. What a test is for

A UI test exists to fail when a feature a user relies on breaks. Before writing one, write down (in the PR, not in
the code):

- **the user scenario**: the steps a person takes and what they expect to see (a catalogue row in
  `plans/ui-tests/catalogue/`; add or correct the row if it is missing or wrong);
- **the regression it catches**: "this test fails if ___". Name a realistic change to app code that breaks the
  feature. If you cannot name one, the test is not worth its run time (every test costs an app reset and usually a
  peer connection).

Then, before the PR, **break the feature on purpose** (revert or invert the line that implements it, locally) and
run the test: it must fail, with a message that points at the feature. A test that stays green is wrong, however
plausible it looks. Mention the mutation you tried in the PR.

## 2. Assertions

### Assert the outcome, through the strongest source of truth

Check what the user or the other side observes, in this order of preference:

1. **The peer** (`CliPeer`): what the other client received or printed. It is independent of the app's UI and model.
2. **The screen**: visible text (`seesText`, `textsOf`), a keyed node (`exists`, `editableText`), a content
   description (`seesDescription`, `descriptionsOf`).
3. **Persisted state** the app reads back at start: `storedSetting(...)`, then `restart()` and check the screen again.
4. **The model** (`chatModel`): only when the state is not visible any other way and adding a key is not allowed
   (section 4).

A test whose name claims a visual (an icon, a colour, a thumbnail, an alert) must check that visual. If the visual
is not observable (an icon with no content description, a colour), do not name the test after it: name it after the
state you can check (`...InModel` is a smell; prefer moving the check into a test that does something the user sees),
or make the element observable by acting on it (tap the thumbnail, open the alert). Examples that were wrong:
`verifiedIconOnRow` and `incognitoIconOnRow` checked only the model; `unreadCounterGrayWhenMuted` never checked the
colour; `groupInvitation_tapRowShowsJoinAlert` never checked the alert.

### Every assertion must be able to fail

- **Prove the opposite first.** Before asserting the new state, assert the old one (the row is listed before the
  filter hides it; the badge is absent before the message arrives). A check that is already true before the action
  tests nothing.
- **Never assert what the harness or the app sets for you.** The DB template sets `storeDBPassphrase`,
  `initialRandomDBPassphrase` and the keystore passphrase (`DbTemplate`); preset operators start enabled; some model
  properties are constants (`ChatInfo.Local.incognito` is always `false`). Read the code behind a value before
  asserting it.
- **Use a negative control.** A filter, search or menu test needs one item that must disappear and one that must stay,
  and the item that must disappear should be of the kind a plausible bug would wrongly include (a group for the
  Contacts filter, not just the notes folder).
- **Cover both branches of conditional UI.** If an option is shown in one case and hidden in another ("Accept
  incognito"), test both; testing only the hidden case passes when the option is deleted.
- **Absence needs presence.** `!exists(x)` and `!seesText(t)` pass when the screen never rendered. Wait for the screen
  (a keyed container or a known text on it) before asserting that something is absent. `isEnabled(key)` is `false`
  for a missing node, so `waitFor(key)` before `!isEnabled(key)`. `isEnabled` is always `true` for `SectionItemView`
  keys (a disabled item has no Disabled semantics): check the effect of clicking instead.
- **Know the app's exemptions.** The open chat is always listed whatever the search text or filter
  (`ChatListView.kt` `filteredChats`), so make sure the chat under test is not the open chat before asserting list
  contents. On desktop, a panel that stays in the center pane can turn into the open chat (a pending connection's
  info becomes the contact's chat when the peer connects).

### Peers

- `expects` checks the very next line: use it when nothing else may come.
- `eventually` / `eventuallyMessage` skip lines until the one you want. Skipped lines are lost, so after an action
  whose side effects matter (delete for me only, reject, block), prove the peer received nothing else: send a later
  sentinel message and `expects` it as the next line (see `delete_received_onlyForMe`).
- Peer output that looks like an error and that the test did not consume explicitly fails the test (section 6).
- The CLI inserts the open chat's name after a leading `> `, `! ` or `\ `. Write these commands without the space (`>@alice (quoted) reply`, `!@alice new text`), or the typed line will not match.

### Servers and the SOCKS stub

- `TestServers.stop(name)`, `start(name)` (returns the address) and `reset(name)` (restart without the store) control
  `smp1`, `xftp1`, `smp2` and `xftp2` (plan §0.5.6). Each test JVM has its own servers, and every reset restores the
  baseline (`smp1` and `xftp1` running, `smp2` and `xftp2` stopped), so start what the test needs and never rely on
  what another test left.
- A peer on the second server: `TestServers.start("smp2")`, then `cli("bob", PeerFlags(smpServer = "smp2"))`.
- Keep outages to a few seconds and use `CONNECT_TIMEOUT_MS` for waits after a start: the agents back off.
- Around an outage CLI peers print `disconnected N connections on server …` and `subscribed N connections on server …`,
  so use `eventually*` for the next line after a stop or start, not `expects`.
- `SocksProxy.start()`, `SocksProxy.port` (host `SOCKS_HOST`) and `SocksProxy.connections()` (each CONNECT the stub
  received, with the target host, port and username); the harness stops it at the end of the test.

### Persistence

"Persists across restart" means: change the setting through the UI, check `storedSetting(...)` (what is on disk),
`restart()`, then check the screen. `restart()` re-reads the stored settings; a check that only re-reads in-memory
state does not test persistence.

## 3. Interacting with the app

- **Find elements by key, text or model, never by coordinates, offsets or pixel sizes.** `click` finds a point inside
  the node that no nested button covers (48 dp touch targets included). Rows in lazy lists need `scrollTo(key)` first.
- **One user action is one input.** Helpers must not retry a tap, a key press or typing because the first one "did not
  take". If the app drops the first input, that is an app bug: record it (plan §15.5) and let the test show it
  (section 6), do not hide it.
- **Wait for state, never for time.** Use `poll(what = "...") { condition }` with a `what` that says what the user is
  waiting for. No sleeps. `settle()` only lets animations finish; it is not a wait for app work.
- **Wait for database work, do not race it.** Many actions return before their database call finishes (mute waits for
  the database). Poll the model or the screen for the new state before the next action. `quiesce()` (no API call in
  flight, no event for 1 s) is for setup only, after joining or connecting. If a test only passes after `quiesce()`
  because the app misbehaves when the user acts while events arrive, that is an app finding: record it in plan §15.5
  and keep a test that exposes it, marked as a known issue (section 6).
- **Timeouts.** The default `poll` timeout is for local UI. Anything that crosses the network (connecting, joining,
  delivery to a peer) uses `CONNECT_TIMEOUT_MS`; Android runs arm64 code under translation and is slower. Never raise a
  timeout to make a flaky test pass; find what it is waiting for.
- **Opening a chat** with `ChatListScreen.open()` first waits for app events to settle, because a message that arrives while a chat is being opened can go missing from it (plan §15.5 item 25). A test that is about acting while events arrive clicks the row key directly.
- **Group sends**: wait for each member's `#g: member X is connected` before sending, or messages arrive forwarded.

### Platform differences (shared tests run on both)

| Situation | Desktop | Android | Use |
|---|---|---|---|
| Back | Esc invokes the window back stack; screens without a `BackHandler` ignore it (New chat sheet, pending-connection info) | system Back; the first Back only hides the soft keyboard | `goBack()` (clicks the "Back" arrow on desktop, system Back on Android), or the screen's own close control |
| Opening a chat | list stays visible next to the chat | chat covers the list | `backToChatList()` before acting on the list |
| Soft keyboard after typing | none | stays open, covers the bottom of the chat, and the first Back only hides it | `hideKeyboard()` before tapping something near the bottom or pressing Back (`backToChatList()` does it) |
| Context menu | right click | long press | `rightClick(key)` |
| Pending-connection info | stays in the center pane, no close control | closable | `closePendingConnectionInfo()` |
| Camera / QR scanner | none | Waydroid has no camera | `assumeCamera()` (skips, never passes) |
| A received file that was tapped before it arrived | the save dialog opens | an Open / Save menu opens | on Android tap `described("Save")` inside `saveFileAs` |

Use `appPlatform.isDesktop/isAndroid` in a shared test only for one of these real UI differences, and say which in
the PR. Behaviour that exists on one platform only belongs in a `desktop/` or `android/` test.

## 4. App code changes

- **Keys only on elements a test acts on** (click, type, scroll to) **or must read text from** where no other way
  exists. No observation-only helpers: no marker modifiers, no keys attached only while some state holds, no new
  parameters just to see an icon. Reuse an existing key when one fits (`Ui.kt`).
- A new `uiKey: UiKey? = null` parameter goes **before** the last function-typed parameter, so existing calls with a
  trailing lambda keep compiling. Then build **both** platforms: the desktop suite does not compile `androidMain`, and
  an Android-only caller broke this way once.
- Keys and `UiIdle` hooks must be no-ops unless `uiTestHooksEnabled`; a release build must behave exactly as before.
- Never fix an app bug inside a test PR; report it (plan §15.5) and get it fixed in its own PR.

## 5. Structure, naming, cost

- **One scenario per test, no prefixes of other tests.** If your test's steps are the first half of an existing test,
  extend that test or assert there instead. Before adding a test, search for tests that already perform the same
  steps. Each test costs an app reset (and a peer connection when it uses one), so three tests that each toggle one
  menu item should be one test that toggles all three.
- **Names**: exactly the catalogue test name; the name states the observable behaviour.
- **Placement**: shared unless the behaviour is platform-specific (section 3). Class names follow the catalogue class
  map (`plans/ui-tests/catalogue/00-legend.md`).
- **"Compat" tests** must run against the stable released CLI peer as well as the current one. Do not name a test
  `...Compat` unless it really does.
- **No explanatory comments** in tests or helpers: a comment that restates the code means the structure or a name is
  wrong. Short bodies built from screen helpers (`alice.chatList.rowMenu(...)`, `chat.send(...)`).

## 6. The harness must not hide failures

When you change the harness (`harness/*.kt`, both `UiTest.kt`), keep these properties:

- **Any app crash fails the test**: an uncaught exception on any thread or coroutine, on both platforms, including
  ones the app's own crash handler recovers from (on Android it closes the open screen or chat, which can look like
  the screen change a test waits for). There is no allowlist by message text.
- **A known app bug is a visible result, not a pass.** Mark the test `@KnownAppIssue("<plan §15.5 item>", "<exact
  error text>")`: when the test fails with that error it is reported as skipped with the finding; when it fails any
  other way it fails; when it passes, it fails with "known issue no longer reproduces; remove the annotation".
  `intermittent = true` lets the test pass when the bug does not show. A test can carry several annotations; a failure
  matching any of them is skipped, and a pass still fails while any of them is not intermittent.
- **No retries of user input**, no repeated `replaceText` until the text sticks, no second Enter.
- **App work still running when the test ends fails the test** (it is a hang or a leaked job), with the names of the
  stuck calls.
- **Helpers set up state through the UI** when that state is what the test is about. Setting state through the API or
  the model is allowed only for preconditions outside the feature under test, and the helper's name says so.
- **No debug helpers, prints or temporary logging** in committed code.

## 7. Before you open a PR

- [ ] The PR states the scenario and "this test fails if ___" for each new test, and the mutation you tried made it
      fail.
- [ ] Every assertion can fail: the opposite state was asserted first; nothing asserted is set by the harness or is a
      constant; negative controls are present; both branches of conditional UI are covered.
- [ ] The name matches what is checked; visual claims are checked visually or the name does not make them.
- [ ] No coordinates, no sleeps, no raised timeouts, no retries of user input, no new `quiesce()` hiding an app race.
- [ ] Shared tests handle the platform differences in section 3; platform-only behaviour is in a platform test.
- [ ] App changes are keys only, on elements the test acts on; `uiKey` before trailing lambdas; both platforms built.
- [ ] No test is a prefix or duplicate of another.
- [ ] Desktop suite and Android suite run green (or the test is `@KnownAppIssue` with a plan §15.5 entry); new tests
      ran three times without a flake.
- [ ] The catalogue row and the plan (§15.4 findings, §15.5 app findings) are updated.
