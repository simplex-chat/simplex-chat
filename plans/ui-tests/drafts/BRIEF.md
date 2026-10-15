# Brief for drafting UI tests (read fully before starting)

## Fences (strict)
- Write ONLY under your assigned draft folder (given in your prompt). Do NOT edit /home/user/wt-uitest or any other repo or file outside your folder.
- Do NOT run gradle, cabal, adb, wd.sh, builds or tests. Do NOT launch agents or create worktrees.
- Stop after writing the deliverables.

## Where things are
- Worktree: /home/user/wt-uitest (Kotlin Multiplatform app in apps/multiplatform). Read-only for you.
- Shared test bodies (compile on desktop AND Android): apps/multiplatform/common/src/uiTestShared/kotlin/chat/simplex/app/ui/** — read harness/Screens.kt, harness/Helpers.kt and several tests (main/ContactsTest.kt, main/ChatListMenuTest.kt, chat/MessagingTest.kt, groups/MemberInfoTest.kt, main/ChatListMenusPendingTest.kt).
- Desktop scope: apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/ui/harness/UiTest.kt, CliPeer.kt. Android scope: apps/multiplatform/android/src/androidTest/java/chat/simplex/app/ui/harness/UiTest.kt, CliPeer.kt. Shared tests may use ONLY methods present in BOTH scopes.
- Keys: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/ui/Ui.kt. Attached in app code with `Modifier.ui(Ui.X)` or `uiKey = ...` params (grep `Ui\.` in common/src/commonMain, desktopMain, androidMain).
- Catalogue: plans/ui-tests/catalogue/0*.md (00-legend.md explains columns and Needs codes). Rows give entry path, code file:line, test names and verification.
- English strings: apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml.
- CLI behaviour: Haskell tests tests/ChatTests/*.hs and view code src/Simplex/Chat/View.hs. The GUI user is "alice".

## Harness API (both platforms)
UiScope: app(user: String?) (null = onboarding, no user), restart(), click(key), rightClick(key) (desktop right-click / Android long press), escape() (desktop window Back stack / Android system Back), typeInto(key, text), replaceText(key, text), pressEnter(key), poll(timeoutMs, what, screenshot){cond}, waitFor(key), exists(key), scrollTo(key) (reveals rows in lazy lists), settle() (~0.5 s of app clock), textsOf/textOf/descriptionsOf(key), isEnabled(key), editableText(key), seesText(text, substring), texts(), storedSetting(prefKey), sliderValue/setSlider(key, v), clipboardText(), cli(name, PeerFlags) -> CliPeer.
CliPeer: `sends` (types a command and waits for its echo), `types` (no echo wait), `expects` (next line exactly), `expectsMessage` (next message line without the time prefix), `eventually(line, timeoutMs)`, `eventuallyMessage`, `eventuallyMatching(regex)`, `nextLine()`, `contactId(name)`.
Helpers: connect(peer), connectPeers(a, b), hostGroup(host, group, members...), ownGroup(app, group, peers...) (alice creates + peers join), quiesce() (no API in flight and no app events for 1 s), CONNECT_TIMEOUT_MS.
Screens (harness/Screens.kt): App (chatList, userPicker(), profile(), appearance(), settings(), newChat()), ChatListScreen (chatId, row, request, rowMenu(name, action, label?), gone, open, rowTexts, rowDescriptions, chat, search, shown), ChatScreen (info, contactInfo, groupInfo, itemId, item, itemTexts, menu, react, reactionCount, send, replaceAndSend, forwardTo, openSearch, search), and others. Add new screen helpers in your screens.kt (as additions), reusing existing ones.

## Rules learned the hard way
- Locate elements ONLY by keys or model state (chatModel). NEVER coordinates, offsets, fractions or pixel sizes. Clicks already find a point not covered by nested buttons.
- No explanatory comments in code. Match the style of existing tests: short bodies, helper calls, `poll(what = ...) { condition }`.
- Before acting on state that the app updates after a database call or from incoming events (mute/favorite/settings changes, right after joining a group), call `quiesce()` or poll the model for the new state first; row menus are opened through `chatList.rowMenu(...)` which already waits for in-flight API calls.
- Rows inside lazy lists (group members, long lists) need `scrollTo(key)` before `click`.
- CLI input: typing "!" then space auto-inserts the current chat (`! @alice `). Use `!@alice new text` for edits. Commands with message text are echoed as the sent message line ("@alice hi"), so `sends` handles both.
- Group sends: wait for each member's "#g: member X is connected" before sending (otherwise messages arrive forwarded with " [>>]").
- Waits on connections use CONNECT_TIMEOUT_MS (Android runs translated arm64 code and is slower).
- Shared tests must behave the same on desktop and Android. If a row is desktop-only behaviour (right-click specifics, zoom, window, file dialogs) put the test in a `desktop/` file and say so; Android-only behaviour goes in an `android/` file. Use `appPlatform.isDesktop/isAndroid` branches only for unavoidable UI differences, and justify them in NOTES.md.
- Verify every asserted UI string against strings.xml and every behaviour against the code (file:line). Verify every CLI line against Haskell tests/View.hs.

## Deliverables (in your folder)
1. Test files mirroring the shared layout: `<area>/<CatalogueClass>.kt` (package chat.simplex.app.ui.<area>, class extends chat.simplex.app.ui.harness.UiTest, test names EXACTLY as in the catalogue). Use the class names from the catalogue class map (`python3 plans/ui-tests/catalogue/tools/classmap.py` if helpful, or 00-legend.md).
2. `keys.diff`: unified diff (paths relative to /home/user/wt-uitest, `a/...` `b/...`) adding new keys to Ui.kt and the `Modifier.ui(...)`/`uiKey = ...` at the composables, for commonMain and any desktopMain/androidMain actuals so both platforms get the key. Minimal app-code changes; reuse existing keys whenever one fits (check Ui.kt first).
3. `screens.kt`: additions to harness/Screens.kt (new classes/funs only; do not repeat existing ones).
4. `NOTES.md`: per test — catalogue file:line, what it asserts, code/Haskell references, platform notes, uncertainties. Then a section "Catalogue corrections": for each row whose text is wrong or incomplete versus the code, give file:line, the problem, and exact replacement text. Then rows skipped with the reason.

## Where platform-specific tests go
- Desktop-only tests: put them in `desktop/<Class>.kt` in your folder, package `chat.simplex.app.ui.desktop`; they will live in apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/ui/desktop/ and may use the desktop UiScope (e.g. `ui`, TestClipboard via `clipboardText()`).
- Android-only tests: `android/<Class>.kt`, package `chat.simplex.app.ui.android`; they will live in apps/multiplatform/android/src/androidTest/java/chat/simplex/app/ui/android/.
- Catalogues 01-04 describe the desktop app; most rows are the same on Android — keep those shared. Onboarding, window, zoom, file dialogs and "Link a mobile" rows are desktop-only.

## App-code additions: keep them minimal (user rule)
- Add keys ONLY to elements a test must act on (click, type, scroll to) or must read text from where no other reliable way exists.
- Do NOT add app-side observation helpers (marker modifiers, keys attached only while some state holds, new uiKey params just to see an icon). Assert visual states through the model (chatModel: contact.verified, chatInfo.incognito, draftChatId, chatSettings, chat items), through visible texts (`textsOf`, `seesText`) or through existing content descriptions (`descriptionsOf`, `seesDescription(text)`).
- If a state is truly not observable without an app change, skip that assertion and explain in NOTES.md instead of adding a helper.
