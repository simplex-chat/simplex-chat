# Desktop UI tests — P1a: start here

> **Historical (2026-10-14):** P1a was completed on 2026-10-13 (plan 15.1). This page is kept for history; for the harness as built see plan section 0.5, "As built (2026-10-14)", which also lists the design items not built yet.

This page tells the implementer of P1a which parts of the plan to read and in what order to build. The plan is `plans/ui-tests/2026-10-10-desktop-ui-test-dsl-plan.md`; section numbers below refer to it. P1a's scope and exit criteria are in its row in §12.

## Goal

Get five real catalogue tests passing on the real desktop app, headless, with a real core and a CLI peer. Measure what the rest of the plan depends on. Change the plan where the measurements disagree with it. Then build P1b.

The five tests:
- 01 `oneTimeLinkCliConnectsViaShortLink`
- 01 `addressCliConnectsAndRequestAppears`
- 03 `groupInvitation_tapRowShowsJoinAlert`
- 02 `sendText_enter_peerReceives`
- 04 `appearance_settingsPersistAcrossRestart`

The first three are written out in §9.3. Run them on the Current CLI only.

## Read (about 2 hours)

| Read | Why |
|---|---|
| Summary, §1, §2 | What is being built and how the parts fit |
| §3.1, §3.2 | `Modifier.ui(key)`, `uiTagsEnabled` (debug/test only), and where a key must sit |
| §6 | Synchronization: the manual clock, `UiIdle` or its fallback, the hard timeout. Skip the end-of-test dialog drain |
| §7.1 and §7.1.1 | Gradle task, opt-ins, `forkEvery` (planned 1; as built 0 by default, `-PuiForkEvery`, plan 0.5.9), runner script |
| §7.2, §7.3 | `bootJvm`, `resetForTest`, `restartCore`, `PrefStores`, and what a reset must clear |
| §7.4 | Driver: input injection, waits, field reads |
| §8.1, §8.2, §8.3 | CLI peers, `uitest-servers` (SMP only for P1a), network namespace |
| §9.1–§9.3 | Screen objects, DSL, example tests |
| §10.1 | How catalogue names map to test classes |
| §12 (P1a row), §15 | Scope, exit criteria, open items |

Skip for P1a: §3.3–§3.5, §4, §5, §7.5–§7.8, §9.4, §11.

## Build order

1. **Day 1 risk check.** Run `./gradlew -g .gradle/uitest-home --offline --no-daemon help` on the real project inside the namespace, after filling that Gradle home from outside (§7.1, §8.3). If the offline run fails, fix it before anything else, because everything later runs this way.
2. Add the `desktopUiTest` source set and task, with the opt-ins, `forkEvery = 1` (as built: default 0, plan 0.5.9) and per-JVM directories (§7.1).
3. Build `uitest-servers` with one SMP server (§8.2), plus the runner script and namespace (§7.1.1, §8.3).
4. Write `bootJvm` and the onboarding boot (§7.2). Set `user.home` before any app class loads, and repeat `System.load(libapp-lib)` + `initHS()`, because `initHaskell` is private to the desktop module.
5. Write the scene and driver: `runInternalSkikoComposeUiTest` with a `StandardTestDispatcher`, the clock-lead rule after input (not built: the poll advances a fixed 16 ms, plan 0.5.3), and desktop input wrapped in `runOnUiThread` (§6, §7.4).
6. Add the `UiIdle` hooks if approved, otherwise the fallback, plus the hard timeout (§6, §13).
7. Write `CliPeer` for the Current CLI with the peer flags and typing rules of §8.1 (as built: partly; no `--mute`, different typing constants, plan 0.5.5).
8. Add the keys and screen objects that the five tests touch (§3.1, §3.2, §9.1). Put each key on the same modifier chain as the clickable.
9. Write the five tests (§9.2, §9.3, and the catalogue rows).
10. Implement `resetForTest` and `restartCore` with `PrefStores` (§7.2, §7.3). Then run all five tests in one JVM and in fresh JVMs.
11. Take the measurements and run the exit check below. Write the numbers into §15 and adjust the budgets in §6 and §7.7.

## Exit check

- Each of the five tests passes 20 consecutive runs.
- These are measured on the real app and recorded in §15:
  - boot and reset time per test;
  - frame render cost;
  - `captureToImage` time on the full scene;
  - memory growth over 20 resets in one JVM;
  - the offline Gradle run in the namespace.
- The scene runner choice and the DSL shape are confirmed, or the plan is changed, before P1b starts.

## Traps the review found (each cost at least one round)

- **Use the right code.** Branch off master in a new worktree. The catalogue's file:line references are pinned to `d72f68e6d` (`~/wt-uitest-ref`), not to the dirty `~/simplex-chat` checkout.
- **Merged nodes hide tags.** A tag on a child of a merged node is invisible in the default tree (§3.2).
- **The real CLI is not the Haskell test client.** A real profile gets the "Ask SimpleX Team" contact. `testView` lines are never printed. Peers need the flags in §8.1, and they must use name commands or `CliPeer.contactId`, not hard-coded contact ids.
- **Advancing the clock by 0** runs queued tasks but renders no frame (§6).
- **`hideAllAlerts` is not enough to close an alert.** It drops the composables but leaves continuations waiting. Close alerts with Escape (§6, "End of each test"). (Not built: the harness only calls `hideAllAlerts` at the next reset, plan 0.5.11.)
- **Line refs before the app.** A failing new test is first checked against its catalogue row and the code, because rows still contain errors at a small rate (§15). Fix the row in the same PR.
- **The root filesystem on this host is small.** Build under `/home`, not `/tmp`.
