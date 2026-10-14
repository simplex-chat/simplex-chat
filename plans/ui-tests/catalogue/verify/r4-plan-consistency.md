I found 18 real problems in the plan and legend, so the result is not CLEAN. Findings 1–3 would make P1 fail or break an approval boundary. Findings 4–11 are rules two implementers would read differently. Findings 12–18 are stale numbers and wording.

**What checks out:** `catcheck.py` and `classmap.py summary` both pass: 1,161 tests, 38 ignored, 72 sections (70 with runnable tests), 82 class bases, at most 29 tests per class, no duplicate names. The class-map rows match the regenerated table exactly. The plan's numbers agree with the four `## Count` tables and the file headers: lines 18, 669–673, 679 (82), 692 (70), 702 (NAMES 15), 703 (RELAY 87), 724–726 (8 / 5 / 1), 752–756 and 778. Every Needs cell uses only legend codes, `S` is never combined with `RELAY`, and every `S` row also has `P`. Every "plan §x.y" reference in the legend and catalogues points to a section that exists and says what is claimed. The three §9.3 test names exist with `P, S` in the classes the map gives them. The §9.3 code matches the §9.1/§9.2 API: method names, return types, `peerVersion`, `@Category(StableCompat::class)`, package names and class names.

## Most severe

**1. `restart()` gives no way to apply a fixture while the core is stopped, but about 25 FIXTURE+RESTART tests need one.**
- Plan 598: `fun restart()  // RESTART: restartCore() (7.3)`. Plan 396: restartCore runs "`chatCloseStore`; clear in-memory state …; then start", with no hook in between.
- Legend 51: FIXTURE is "prepared before boot via `app(fixture = …)`, or between `restart()` steps for data the test produced itself (then also `RESTART`)". Plan 711 says the same.
- Examples: 01:256 "copy her database, exchange messages, restore the copy"; 02:249 "with the core stopped, the GUI database …". Implementers will each invent a different API (`stop()`/`start()`, reflection, copying a live database).
- **Fix, plan 598:** `fun restart(fixture: ((dbPrefix: String) -> Unit)? = null)   // RESTART: restartCore(fixture) (7.3)`
- **Fix, plan 396:** after "`chatCloseStore`;" insert "`fixture?.invoke(dbPrefix)` (FIXTURE rows that copy, restore or edit data the test produced);".

**2. The P1 stubs fail P1's own traceability check.**
- Plan 751: "every catalogue name generated as an `@Ignore("not written")` stub". Plan 682, which applies in non-strict mode: the check fails if "an `IGNORE` name's method lacks `@Ignore`, or its reason differs from the row's".
- All 38 IGNORE names would fail from day one.
- **Fix, plan 751:** "every catalogue name generated as a stub in its mapped class: `@Ignore("not written")` for runnable rows, `@Ignore("<the row's IGNORE reason>")` for `IGNORE` rows".

**3. `UiIdle` is new app code, but the plan says no app code is added.**
- Plan 257: "`UiIdle` … A commonMain object, always on". It hooks `sendCmd` (SimpleXAPI.kt:936-958), `startReceiver` and `withBGApi`/`withApi` (Utils.kt:34-49).
- That contradicts plan 337 ("No app code is added for the reset"), even though the reset calls `UiIdle.drain()` at 353.
- It is also missing from plan 720 ("Besides keys and typed semantics (…), the catalogue needs three seams"), from the approved list at 768, and from K0 at 240.
- **Fix, plan 720:** "Besides keys and typed semantics (which include the `PlatformTextField` key parameter of 3.3 and making `keyForItem` internal) and the `UiIdle` busy tracker of section 6 (hooks in `ChatController.sendCmd`, `startReceiver`, `withBGApi`/`withLongRunningApi`/`withApi`), the catalogue needs three seams."
- Add `UiIdle.kt` and its hooks to the K0 row. Add a §13 entry marking UiIdle as approved or pending.
- **Fix, plan 337:** "The reset adds no app code beyond `UiIdle` (6): it uses existing functions, public state and (for a few private fields) reflection from test code."

**4. Starting a fixture that already has a user skips server setup.**
- Plan 364: `if (chatModel.chatDbStatus.value != DBMigrationResult.OK || chatModel.currentUser.value != null) return  // fixture DB error or fixture user`. That return comes before `TestServers.configure(u)` (369), `setLastVersionDefault` and `clearUiState`.
- Tests that boot a fixture user and need a reachable server would then use the fixture's own servers, which can't be reached in the namespace:
  - 01:92, 94, 139, 228 (`P, FIXTURE …`);
  - 03:131, whose upgrade needs SMP, on a database "created by a pre-short-link CLI release".
- **Fix, plan 364–366:** `if (chatModel.chatDbStatus.value != DBMigrationResult.OK) return` / `val fixtureUser = chatModel.currentUser.value` / `if (fixtureUser == null && user == null) return` / `val u = fixtureUser ?: runBlocking { chatController.apiCreateActiveUser(null, Profile(user!!, "", null)) }!!`.
- Then run `configure` for both cases, and call `startChat` only when `fixtureUser == null` (initChatController already started the chat for a fixture user).

## Rules implementers would read differently

**5. The category rule clashes with `Smoke` and `StableCompat`, and the smoke run can't include the traceability check as written.**
- Plan 683: the check fails if "a method's JUnit categories don't match its Needs". But `Smoke` (688) and `StableCompat` (679) don't come from any Needs code, so a strict equality check fails every Smoke method.
- Nothing checks that the `<Base>CompatStableTest` subclass exists.
- Plan 688/692 says the Smoke run includes "the traceability check". With `includeCategories(Smoke)` an uncategorised test is excluded, so it won't run.
- Plan 331 excludes Multicast from pass 1, so a Smoke test chosen with `MCAST` would never run.
- **Fix, plan 683:** "a method's Needs-derived categories (method and class annotations) differ from those implied by its row (`RELAY` → `NeedsRelay`, `NAMES` → `NeedsNames`, `FIXTURE` → `NeedsFixture`, `MCAST` → `Multicast`, `SLOW` → `Slow`); `Smoke` and `StableCompat` are not compared; an `S` base lacks `<Base>CompatStableTest` with `@Category(StableCompat::class)` overriding `peerVersion`;"
- **Fix, plan 688:** append "`CatalogueTraceabilityTest` carries `@Category(Smoke::class)`. A sub-area's Smoke test has no `MCAST`."

**6. The legend names the Compat classes two ways.**
- Legend 39: "the sub-area's separate `open class <SubArea>CompatTest`, whose subclass `<SubArea>CompatStableTest`".
- Legend 111 and plan 679 use `<Base>CompatTest`, and the class map has `ChatListRequestsCompatTest`, a base inside sub-area 01 §3 "Chat list".
- **Fix, legend 39:** "These tests live in their class base's separate `open class <Base>CompatTest` (class map), whose subclass `<Base>CompatStableTest` overrides the peer version;".

**7. It is unclear what exact string an IGNORE reason must be.**
- Legend 23: "the reason taken from plan §11.3". Plan 682: reason must not differ "from the row's". Plan 757: "`@Ignore` with an 11.3 reason".
- The cells paraphrase §11.3, and some end with "(plan §11.3)" while others don't. Compare 04:183 "known app bug: the import error alert is never shown (plan §11.3)" with 02:111, which has no suffix.
- **Fix, legend 23:** "…and the `Needs` cell says `IGNORE: <reason>`; the reason must fall under a plan §11.3 bullet. The `@Ignore` string is the cell text after `IGNORE: `, verbatim, including any parenthetical."
- **Fix, plan 757:** "…or `@Ignore` with its row's reason".

**8. `relay()` always registers the relay, but relay-settings tests need one that isn't registered.**
- Plan 595: `fun relay(…): RelayPeer  // headless relay, registered in the GUI's servers`.
- Plan 479 allows "(or the settings test does it through the UI)", and 04:135 says "the harness does not pre-register this".
- **Fix, plan 595:** `fun relay(name: String, web: Boolean = false, register: Boolean = true): RelayPeer  // headless relay; registered in the GUI's servers unless register = false (relay-settings tests add it through the UI)`

**9. The runner's two passes break the documented `--tests` usage.**
- Plan 331: pass 2 runs `-PuiTags=Multicast` "with the user's exclusions and `--tests`". Plan 333 shows usage `scripts/uitest.sh --tests '*OneTimeLink*Test'`.
- When a `--tests` filter ends up running zero tests, Gradle fails with "No tests found for given includes", because `failOnNoMatchingTests` defaults to true and JUnit 4 category exclusion leaves nothing to count. This is from Gradle's documented behaviour; I did not run it.
- Likewise, `-PuiTags=Multicast` makes pass 1 empty.
- **Fix, plan 331:** append "Pass 1 is skipped when the user's `uiTags` is exactly `Multicast`. `desktopUiTest` sets `filter.isFailOnNoMatchingTests = false` when `-PuiPass2` is given, and pass 2 passes it."

**10. §11.1 lacks rows for several Needs codes, and labels the stubs with the wrong code.**
- No §11 row covers `P`, `S`, `MCAST` or `DT`. `DT` (22 tests) has no setup described anywhere in the plan.
- The Stubs row (713) says "Used by (Needs code)" is `—`. But `—` is itself a Needs code meaning "nothing beyond the local servers", while those stubs are `FD`, `CB`, `URI`, `NTF` and `TOAST`.
- **Fix, row 713:** change `—` to "`FD`, `CB`, `URI`, `NTF`, `TOAST`".
- **Add rows:**
  - `| CLI peers | `P`, `S` | `CliPeer` (8.1): current build from `dist-uitest`; for `S`, the release binary in `…CompatStableTest` (10.1) |`
  - `| Multicast | `MCAST` | loopback multicast route (8.3); `Multicast` category, serial second pass (7.1.1) |`
  - `| Developer tools | `DT` | `appPrefs.developerTools.set(true)` after `app()` (the toggle itself is tested through the UI in 04 §J) |`

**11. The stable CLI tag is described two ways.**
- Plan 478: "`-PuiStableTags=v7.0.3`, defaulting to the release GitHub marks "Latest"". That is plural and decided at run time.
- Plan 770 fixes the default at v7.0.3, plan 457 has a single `UiEnv.defaultStableTag`, and plan 679 says "JUnit 4 runs the Compat tests once per peer version".
- One implementer will query the GitHub API, another will hard-code the tag. With several tags, only one would ever run.
- **Fix, plan 478:** "The tag is a Gradle property, `-PuiStableTag=<tag>` (one per run), default `v7.0.3` pinned in `build.gradle.kts` and bumped by hand; to test another release, run again with another tag."

## Lower severity

**12. `-PuiFixtureTags` has no default.** Plan 290/711 mention it, but neither gives a default or says how a fixture picks its CLI. 03:131 needs "a pre-short-link CLI release". **Fix, plan 711:** add "`-PuiFixtureTags` defaults to the last release without short-link data (pinned in P1); a fixture gets that CLI's path from `UiEnv.fixtureCli(tag)`."

**13. `SMP` and `SRVCTL` overlap.**
- Legend 40: `SMP` is "listed only when the test controls it". Legend 48: `SRVCTL` means "stop/start a local server mid-test".
- The catalogues use both styles: 02:232 has `P, SMP, SRVCTL`; 01:156 has only `SRVCTL`.
- **Fix, legend 40:** "| `SMP` | local SMP server (always available; never needed in a cell — stopping or starting it is `SRVCTL`) |". Then drop `SMP` from 02:232.

**14. The legend's count rule covers only one plan table.** Legend 201 says only "The plan's §10 table is regenerated". The plan also repeats derived counts at lines 18, 679, 692, 702, 703, 724–726, 752–756 and 778. **Fix:** "The plan's numbers derived from the catalogue (lines 18, §10 table, §10.1, §10.2, §11.1 NAMES/RELAY, §11.2, §12, §14) are regenerated from the four `## Count` tables, the class map and the Needs counts."

**15. Stale counts in 03:302.** It says "2 need a `FIXTURE`", "`SRVCTL` ×1" and "10 need a peer, relay or server stopped". The rows give FIXTURE = 3 (131, 132, 183) and SRVCTL = 2, so the stopped total is 11. **Fix:** "3 need a `FIXTURE`; 11 need a peer, relay or server stopped or restored (`PEERCTL` ×9, `SRVCTL` ×2)".

**16. "One test per sub-area" doesn't match 70.** Plan 688 says Smoke runs "one test per sub-area" (there are 72), while 692 and 752 say 70. Two sub-areas (02 §Y, 04 §N) are all-IGNORE. **Fix, plan 688:** "one test per sub-area with runnable tests (70)".

**17. Plan and catalogue disagree on which notices need a restart.** Plan 715 lists two notices "checked once at first composition (SimpleX Lock notice …; app-update notice …)". Catalogue 01 line 7 lists three, adding `whatsNewVersion`, and its row `whatsNewShownAfterVersionUpgrade` is marked RESTART. **Fix, plan 715:** "(SimpleX Lock notice, App.kt:94-103; app-update notice, desktop Main.kt:79-87; What's New, ChatListView.kt:219-228)".

**18. Two misleading labels.**
- The 01 §8 heading, "Paste link and connection plans (every link type)", which the class map repeats, contradicts legend 96. That row gives group, channel, relay-address and business plans to 03, with "01: none". **Fix:** retitle it "Paste link and connection plans (contact, invitation, address, bot, name, signature, URI and link-card links)" and regenerate the class map.
- The plan 59 diagram shows `cli("bob", CliVersion.Stable())` inside a test. Under §10.1, stable peers come only from overriding `peerVersion` in `…CompatStableTest`. Copying the diagram would pin peers to stable even in the current-build class. **Fix:** `uiTest { val alice = app("alice"); val bob = cli("bob") ... }   // peer version from peerVersion (10.1)`.