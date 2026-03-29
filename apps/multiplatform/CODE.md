# Coding and building

You are an expert developer for SimpleX Chat, a privacy-first decentralized messaging platform. You MUST navigate and develop this codebase using the three-layer documentation architecture described below. You MUST NOT write code without first loading the relevant product and spec context.

## Three-Layer Documentation Architecture

### Why this structure exists

LLMs start each session with no persistent understanding of the codebase. Navigating thousands of lines of flat source code to reconstruct behavior, constraints, and intent wastes context window and produces unreliable results.

The `product/`, `spec/`, and source layers form a persistent, structured representation of the system that survives across sessions. Each layer is connected to the next by bidirectional cross-references. This structure enables you to load only the context relevant to a specific change, understand all affected concepts, and maintain coherence as the system evolves.

### The layers

| Layer | Contains | Question it answers |
|-------|----------|-------------------|
| `product/` | Capabilities, user flows, views, business rules, glossary | **What** does the system do and why? |
| `spec/` | Technical design, API contracts, database schema, service internals | **How** is it organized technically? |
| `common/src/commonMain/` | Shared Kotlin/Compose code (Android + Desktop) | What does it **execute** on both platforms? |
| `common/src/androidMain/` | Android-specific Kotlin (platform implementations) | What does it execute on **Android**? |
| `common/src/desktopMain/` | Desktop-specific Kotlin (platform implementations) | What does it execute on **Desktop**? |
| `android/src/main/` | Android app module (Application, Activity, Services) | What is the **Android entry point**? |
| `desktop/src/jvmMain/` | Desktop app module (main function) | What is the **Desktop entry point**? |
| `../../src/Simplex/Chat/` | Haskell core (chat logic, protocol, database) | What does the **core** execute? |

Each layer links to the next:
- `product/concepts.md` links every concept to its spec docs, source files, and tests in a single table — this is the primary navigation entry point
- `product/views/*.md` and `product/flows/*.md` each have a **Related spec:** line linking to their most relevant spec documents
- `product/glossary.md` uses *See: [spec/...]* references and `product/rules.md` uses **Spec:** [spec/...] references to link individual terms and rules down to spec
- `spec/` documents contain **Source:** headers and inline function links pointing down to source. Line references MUST be clickable by embedding the `#Lxx-Lyy` fragment in the link URL: [`functionName()`](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#Lxx-Lyy). You MUST NOT duplicate line numbers in the display text — the URL fragment is sufficient. Why: redundant line numbers in display text create maintenance burden on every line shift.
- Reverse direction: the Document Map (end of this file) maps source → spec → product

### Navigation workflow

When the user requests any change, you MUST follow these steps before writing any code:

1. **Identify scope.** You MUST read `product/concepts.md` and find which product concepts are affected by the requested change. Each row links to the relevant product docs, spec docs, source files, and tests. Why: concepts.md is the fastest path to identify all affected documents — skipping it risks missing impacted areas.

2. **Load product context.** You MUST read the relevant `product/views/*.md` or `product/flows/*.md` to understand current user-facing behavior. For business constraints, you MUST read `product/rules.md`. Why: product documents define the intended behavior — changing code without understanding current behavior risks breaking the user contract.

3. **Load spec context.** You MUST follow the product → spec links to read the relevant `spec/*.md` or `spec/services/*.md`. You MUST understand the technical design, function signatures, and data flows. Why: spec documents reveal technical constraints and invariants that product docs omit — ignoring them leads to implementations that violate existing guarantees.

4. **Load source context.** You MUST follow the spec → source links (with line numbers) to read the relevant source files. Why: source code is the ground truth — product and spec may lag behind actual behavior.

5. **Identify full impact.** You MUST read `spec/impact.md` to find all product concepts affected by the source files you plan to change. This determines which documents you MUST update after the code change. Why: without impact analysis, documentation updates will be incomplete, and future sessions will navigate using stale information.

For internal-only changes that do not map to a product concept (infrastructure, refactoring, non-user-facing fixes), you MUST start at step 3 using the Document Map to find the relevant spec document, then proceed to steps 4–6.

6. **Implement.** Make the code change in source, then you MUST update all affected documentation as described in the Change Protocol below.

### Key navigation documents

| Document | Purpose | When to read |
|----------|---------|-------------|
| `product/concepts.md` | Concept → doc → code → test cross-reference | Starting point for every change |
| `product/rules.md` | Business invariants with enforcement locations and tests | Before modifying any behavior |
| `product/glossary.md` | Domain term definitions | When encountering unfamiliar terms |
| `product/gaps.md` | Known issues and recommendations | Before designing a fix or feature |
| `spec/impact.md` | Source file → affected product concepts | After identifying which files to change |
| Document Map (below) | Source ↔ spec ↔ product mapping | When updating documentation |

---

## Code Security

When designing code and planning implementations, you MUST:
- Apply adversarial thinking, and consider what may happen if one of the communicating parties is malicious. Why: security vulnerabilities arise from untested assumptions about trust boundaries.
- Formulate an explicit threat model for each change — who can do which undesirable things and under which circumstances. Why: explicit threat models catch attack vectors that implicit reasoning misses.

---

## Code Style

**Follow existing code patterns — you MUST:**
- Match the style of surrounding code. Why: consistent style reduces cognitive load and prevents unnecessary diff noise.
- Use Kotlin data classes for value types, regular classes for reference types, and sealed classes/interfaces for variants. Why: correct type choices leverage the type system for compile-time correctness.
- Prefer exhaustive `when` expressions over `else` branches. Why: `else` branches bypass compiler checks for new sealed subclasses and hide bugs.

**Comments policy — you MUST:**
- Only comment on non-obvious design decisions or tricky implementation details. Why: redundant comments create maintenance burden and drift from code.
- Keep function names and type signatures self-documenting. Why: good names eliminate the need for most comments.
- Assume a competent Kotlin reader. Why: over-explaining trivial Kotlin adds noise without value.

**Diff and refactoring — you MUST:**
- Avoid unnecessary changes and code movements. Why: unnecessary changes increase review burden and hide the meaningful diff.
- Never do refactoring unless it substantially reduces cost of solving the current problem, including the cost of refactoring itself. Why: speculative refactoring has guaranteed present cost with uncertain future benefit.
- Minimize the code changes — do what is minimally required to solve users' problems. Why: smaller diffs are easier to review, less likely to introduce bugs, and faster to revert.

**Document and code structure — you MUST:**
- **Never move existing code or sections around** — add new content at appropriate locations without reorganizing existing structure. Why: moving code creates large diffs that obscure the actual change and break git blame.
- When adding new sections to documents, continue the existing numbering scheme. Why: consistent numbering preserves document navigability.
- Minimize diff size — prefer small, targeted changes over reorganization. Why: large diffs compound review errors and make rollback difficult.

**Code analysis and review — you MUST:**
- Trace data flows end-to-end: from origin, through storage/parameters, to consumption. Flag values that are discarded and reconstructed from partial data (e.g. extracted from a URI missing original fields) — this is usually a bug. Why: broken data flows are the most common source of security and correctness bugs.
- Read implementations of called functions, not just signatures — if duplication involves a called function, check whether decomposing it resolves the duplication. Why: function signatures can be misleading about actual behavior.
- Read every function in the data flow even when the interface seems clear. Why: wrong assumptions about internals are the main source of missed bugs.

---

## Plans

When developing via plans (non-trivial features, multi-step changes, architectural decisions), you MUST store the plan in the `plans/` folder before implementing. Why: plans are the persistent record of design decisions and rationale — without them, future sessions cannot understand why the system was built the way it was.

### Plan requirements

1. **File naming.** You MUST use the format `YYYYMMDD_NN.md` (e.g., `20260211_01.md`). Why: chronological ordering makes it easy to trace the evolution of design decisions.

2. **Plan structure.** Every plan MUST include: (1) Problem statement, (2) Solution summary, (3) Detailed technical design, (4) Detailed implementation steps. Why: incomplete plans lead to ad-hoc implementation that drifts from intent.

3. **Consistency with product/ and spec/.** The plan MUST be consistent with the current state of `product/` and `spec/`. If the plan introduces new behavior, it MUST describe which product and spec documents will be affected. Why: plans that contradict existing documentation create conflicting sources of truth.

4. **Adversarial self-review.** After writing the plan, you MUST run the same adversarial self-review as for code changes: verify the plan is internally consistent, consistent with product/ and spec/, and does not introduce contradictions. You MUST repeat until two consecutive passes find zero issues. Why: an incoherent plan produces incoherent implementation.

---

## Change Protocol

### The rule

Every code change MUST include corresponding updates to `spec/` and `product/`. A task is NOT complete until all three layers are coherent with each other. Why: these layers are the persistent memory that enables coherent development across sessions — stale documentation creates false confidence and compounds errors in every future change.

### What to update

1. **spec/ — on every code change.** You MUST update the corresponding spec document to reflect the change. You MUST add new functions, update changed signatures, and remove deleted ones. Why: spec documents map 1:1 to source files — divergence defeats specification.

2. **product/ — when user-visible behavior changes.** You MUST update the relevant `product/views/*.md` and any affected `product/flows/*.md`. You MUST update `product/rules.md` when business invariants change. Why: product documents are the contract with users — silent changes create confusion.

3. **Line number references — on every code change.** You MUST verify and update all `#Lxx-Lyy` references in affected spec documents. Why: stale line numbers make spec documents misleading and destroy navigational value.

4. **Cross-references — when adding or removing files.** You MUST add corresponding spec documents and update `spec/README.md` document index and reverse index. When adding pages, you MUST add `product/views/` and `spec/client/` documents. You MUST update the Document Map at the end of this file. Why: every source file must be covered for the navigation system to work.

5. **Impact graph — when adding files or changing what a file affects.** You MUST update `spec/impact.md` to reflect the source file → product concept mapping. Why: the impact graph drives documentation updates for all future changes — an incomplete graph causes future changes to miss required updates.

6. **Concept index — when adding or changing product concepts.** You MUST add or update the relevant row in `product/concepts.md` with links to product docs, spec docs, source files, and tests. Why: the concept index is the entry point for all future navigation — a missing row means future changes to that concept will miss context.

7. **[GAP] annotations — when discovering issues.** When encountering missing error handling, dead code, inconsistencies, or incomplete features, you MUST add a `[GAP]` annotation in the relevant spec or product document and add a summary to `product/gaps.md`. Why: this builds institutional knowledge about technical debt.

8. **[REC] annotations — when identifying improvements.** You MUST add a `[REC]` annotation in the relevant document. Why: capturing improvement ideas at discovery time preserves context that is lost later.

9. **Preserve document structure.** You MUST follow existing format conventions: spec documents use function-anchored links with line numbers, product documents use interaction descriptions, flow documents use Mermaid diagrams. Why: consistent structure makes documents predictable and navigable.

### Adversarial self-review

After completing all changes (code + documentation), you MUST run an adversarial self-review. You MUST check coherence both within each layer and across layers.

**Within-layer coherence — you MUST verify:**
- spec/ is internally consistent — no contradictory descriptions, state machines have no unreachable states, data model is referentially intact
- product/ is internally consistent — flows match views, rules match behavior descriptions

**Across-layer coherence — you MUST verify:**
- Every new or changed function in source appears in the corresponding spec/ document
- Every user-visible behavior change in source appears in the relevant product/ document
- All `#Lxx-Lyy` line references in affected spec documents point to the correct lines
- All cross-references resolve — product → spec links, spec → source links
- `spec/impact.md` covers all affected product concepts for the changed source files
- `product/concepts.md` rows are current for any affected concepts

**Convergence:** You MUST repeat the review-and-fix cycle until two consecutive passes find zero issues. You MUST fix all issues discovered between passes. Why: LLM non-determinism means a single review pass may miss violations — two consecutive clean passes provide confidence that the layers are coherent.

---

## Multiplatform Architecture Notes

### Kotlin Multiplatform (KMP) + Compose Multiplatform

The app uses Kotlin Multiplatform with Compose Multiplatform for shared UI. The project has three Gradle modules:

- **common/** — Shared library containing all models, views, platform abstractions, and theme system
- **android/** — Android app module (Application, Activity, Services)
- **desktop/** — Desktop JVM app module (main entry point)

### expect/actual Pattern

Platform-specific code uses Kotlin's `expect`/`actual` mechanism. The `commonMain` source set declares `expect` functions/classes, and `androidMain`/`desktopMain` provide `actual` implementations. Files follow the naming convention:
- `commonMain`: `FileName.kt` (contains `expect` declarations)
- `androidMain`: `FileName.android.kt` (contains `actual` implementations)
- `desktopMain`: `FileName.desktop.kt` (contains `actual` implementations)

When modifying platform abstractions, you MUST update both `actual` implementations.

### Source Set Structure

```
common/src/
├── commonMain/kotlin/chat/simplex/common/    -- Shared code (195 files)
│   ├── model/          -- ChatModel, SimpleXAPI, CryptoFile
│   ├── platform/       -- expect/actual platform abstractions
│   ├── ui/theme/       -- Theme system (ThemeManager, colors, types)
│   └── views/          -- Compose UI (chat, chatlist, call, settings, etc.)
├── androidMain/kotlin/chat/simplex/common/   -- Android actuals (55 files)
│   ├── platform/       -- actual implementations
│   └── views/          -- Android-specific view variants
├── desktopMain/kotlin/chat/simplex/common/   -- Desktop actuals (56 files)
│   ├── platform/       -- actual implementations
│   └── views/          -- Desktop-specific view variants
android/src/main/java/chat/simplex/app/       -- Android app (8 files)
desktop/src/jvmMain/kotlin/chat/simplex/desktop/ -- Desktop app (1 file)
```

### Platform Differences

| Aspect | Android | Desktop |
|--------|---------|---------|
| Layout | 2-column (chat list → chat) | 3-column (sidebar → chat list → details) |
| Background messaging | SimplexService (foreground service) + MessagesFetcherWorker (WorkManager) | Continuous (always-on process) |
| Notifications | Android NotificationManager with channels | Desktop system notifications |
| Calls | CallActivity (separate Activity) + CallService | In-window call view |
| Video playback | ExoPlayer | VLC (VLCJ) |
| Authentication | Android BiometricPrompt | Passcode only |
| Auto-update | Play Store / manual APK | Built-in AppUpdater |
| Window management | Standard Activity lifecycle | StoreWindowState persistence |
| Entry point | SimplexApp (Application) + MainActivity | Main.kt → initHaskell() → showApp() |

---

## Document Map

### Shared Sources (commonMain)

| Source Location | Spec Document | Product Document |
|----------------|---------------|-----------------|
| common/.../common/App.kt | spec/architecture.md | product/views/chat-list.md |
| common/.../common/AppLock.kt | spec/architecture.md | product/views/settings.md |
| common/.../common/model/ChatModel.kt | spec/state.md | product/concepts.md |
| common/.../common/model/SimpleXAPI.kt | spec/api.md, spec/architecture.md | product/concepts.md |
| common/.../common/model/CryptoFile.kt | spec/services/files.md | product/flows/file-transfer.md |
| common/.../common/platform/Core.kt | spec/architecture.md | product/concepts.md |
| common/.../common/platform/AppCommon.kt | spec/architecture.md | product/flows/onboarding.md |
| common/.../common/platform/Notifications.kt | spec/services/notifications.md | product/flows/messaging.md |
| common/.../common/platform/NtfManager.kt | spec/services/notifications.md | product/flows/messaging.md |
| common/.../common/platform/Files.kt | spec/services/files.md | product/flows/file-transfer.md |
| common/.../common/platform/SimplexService.kt | spec/services/notifications.md | product/flows/messaging.md |
| common/.../common/platform/Share.kt | spec/architecture.md | product/concepts.md |
| common/.../common/platform/VideoPlayer.kt | spec/services/files.md | product/views/chat.md |
| common/.../common/platform/RecAndPlay.kt | spec/services/files.md | product/views/chat.md |
| common/.../common/platform/UI.kt | spec/architecture.md | product/views/chat.md |
| common/.../common/platform/Platform.kt | spec/architecture.md | product/concepts.md |
| common/.../common/ui/theme/ThemeManager.kt | spec/services/theme.md | product/views/settings.md |
| common/.../common/ui/theme/Theme.kt | spec/services/theme.md | product/views/settings.md |
| common/.../common/ui/theme/Color.kt | spec/services/theme.md | product/views/settings.md |
| common/.../common/views/chatlist/ChatListView.kt | spec/client/chat-list.md | product/views/chat-list.md |
| common/.../common/views/chatlist/ChatListNavLinkView.kt | spec/client/chat-list.md | product/views/chat-list.md |
| common/.../common/views/chatlist/ChatPreviewView.kt | spec/client/chat-list.md | product/views/chat-list.md |
| common/.../common/views/chatlist/UserPicker.kt | spec/client/chat-list.md | product/views/chat-list.md |
| common/.../common/views/chatlist/TagListView.kt | spec/client/chat-list.md | product/views/chat-list.md |
| common/.../common/views/chat/ChatView.kt | spec/client/chat-view.md | product/views/chat.md |
| common/.../common/views/chat/ComposeView.kt | spec/client/compose.md | product/views/chat.md |
| common/.../common/views/chat/SendMsgView.kt | spec/client/compose.md | product/views/chat.md |
| common/.../common/views/chat/ChatInfoView.kt | spec/client/chat-view.md | product/views/contact-info.md |
| common/.../common/views/chat/group/ | spec/client/chat-view.md | product/views/group-info.md |
| common/.../common/views/chat/item/ | spec/client/chat-view.md | product/views/chat.md |
| common/.../common/views/call/CallView.kt | spec/services/calls.md | product/views/call.md |
| common/.../common/views/call/IncomingCallAlertView.kt | spec/services/calls.md | product/views/call.md |
| common/.../common/views/call/WebRTC.kt | spec/services/calls.md | product/flows/calling.md |
| common/.../common/views/newchat/NewChatView.kt | spec/client/navigation.md | product/views/new-chat.md |
| common/.../common/views/newchat/AddGroupView.kt | spec/client/navigation.md | product/views/new-chat.md |
| common/.../common/views/usersettings/SettingsView.kt | spec/client/navigation.md | product/views/settings.md |
| common/.../common/views/usersettings/Appearance.kt | spec/services/theme.md | product/views/settings.md |
| common/.../common/views/usersettings/PrivacySettings.kt | spec/client/navigation.md | product/views/settings.md |
| common/.../common/views/usersettings/networkAndServers/ | spec/architecture.md | product/views/settings.md |
| common/.../common/views/usersettings/UserProfilesView.kt | spec/client/navigation.md | product/views/user-profiles.md |
| common/.../common/views/onboarding/ | spec/client/navigation.md | product/views/onboarding.md |
| common/.../common/views/localauth/ | spec/architecture.md | product/views/settings.md |
| common/.../common/views/database/ | spec/database.md | product/views/settings.md |
| common/.../common/views/migration/ | spec/database.md | product/flows/onboarding.md |
| common/.../common/views/remote/ | spec/architecture.md | product/views/settings.md |
| common/.../common/views/contacts/ | spec/client/chat-view.md | product/views/contact-info.md |
| common/.../common/views/helpers/ | spec/architecture.md | product/concepts.md |

### Android-Specific Sources

| Source Location | Spec Document | Product Document |
|----------------|---------------|-----------------|
| android/.../app/SimplexApp.kt | spec/architecture.md | product/flows/onboarding.md |
| android/.../app/MainActivity.kt | spec/architecture.md | product/views/chat-list.md |
| android/.../app/SimplexService.kt | spec/services/notifications.md | product/flows/messaging.md |
| android/.../app/CallService.kt | spec/services/calls.md | product/flows/calling.md |
| android/.../app/MessagesFetcherWorker.kt | spec/services/notifications.md | product/flows/messaging.md |
| android/.../app/model/NtfManager.android.kt | spec/services/notifications.md | product/flows/messaging.md |
| android/.../app/views/call/CallActivity.kt | spec/services/calls.md | product/views/call.md |

### Desktop-Specific Sources

| Source Location | Spec Document | Product Document |
|----------------|---------------|-----------------|
| desktop/.../desktop/Main.kt | spec/architecture.md | product/flows/onboarding.md |
| common/.../common/DesktopApp.kt (desktopMain) | spec/architecture.md | product/views/chat-list.md |
| common/.../common/StoreWindowState.kt (desktopMain) | spec/architecture.md | product/views/settings.md |
| common/.../common/model/NtfManager.desktop.kt (desktopMain) | spec/services/notifications.md | product/flows/messaging.md |
| common/.../common/views/helpers/AppUpdater.kt (desktopMain) | spec/architecture.md | product/views/settings.md |

### Haskell Core Sources (at `../../src/Simplex/Chat/` relative to `apps/multiplatform/`)

| Source Location | Spec Document | Product Document |
|----------------|---------------|-----------------|
| ../../src/Simplex/Chat/Controller.hs | spec/api.md | product/concepts.md |
| ../../src/Simplex/Chat/Types.hs | spec/api.md | product/glossary.md |
| ../../src/Simplex/Chat/Core.hs | spec/architecture.md | product/concepts.md |
| ../../src/Simplex/Chat/Protocol.hs | spec/architecture.md | product/concepts.md |
| ../../src/Simplex/Chat/Messages.hs | spec/api.md | product/flows/messaging.md |
| ../../src/Simplex/Chat/Messages/CIContent.hs | spec/api.md | product/flows/messaging.md |
| ../../src/Simplex/Chat/Call.hs | spec/services/calls.md | product/flows/calling.md |
| ../../src/Simplex/Chat/Files.hs | spec/services/files.md | product/flows/file-transfer.md |
| ../../src/Simplex/Chat/Store/Messages.hs | spec/database.md | product/flows/messaging.md |
| ../../src/Simplex/Chat/Store/Groups.hs | spec/database.md | product/flows/group-lifecycle.md |
| ../../src/Simplex/Chat/Store/Direct.hs | spec/database.md | product/flows/connection.md |
| ../../src/Simplex/Chat/Store/Files.hs | spec/database.md | product/flows/file-transfer.md |
| ../../src/Simplex/Chat/Store/Profiles.hs | spec/database.md | product/views/user-profiles.md |
