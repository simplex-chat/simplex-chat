# Desktop tray icon — implementation plan

Companion to the design at `plans/2026-05-09-desktop-tray.md`. Read that first.

## What

Eight small commits that build the feature incrementally. After each commit the build is green and the app still runs; only the last commit makes the feature visible to the user end-to-end.

## Why

We split the work this way so each commit is reviewable on its own and revertable without unwinding others. The order keeps the build green throughout (no commit introduces a reference to something the next commit will define).

## How

### Pre-flight

- Pull the branch `sh/tray` (current branch). It is at `stable`.
- Confirm dev environment can build desktop: `cd apps/multiplatform && ./gradlew :common:desktopMainClasses` — should succeed before any change.
- Read `plans/2026-05-09-desktop-tray.md` end to end. The implementation steps below assume that design is settled.

---

### Task 1 — `CloseBehavior` enum + preference

**Files**
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`

**What to add.** The enum lives next to other small enums in this file (search for `enum class LAMode` for placement convention). The preference goes in `class AppPreferences` next to `notificationsMode`.

Match the existing pattern (use `values().firstOrNull { it.name == this }`, not `entries`, to stay consistent with `LAMode` and others in this file):

```kotlin
enum class CloseBehavior {
  Ask, Quit, MinimizeToTray;
  companion object { val default = Ask }
}

// In AppPreferences:
val closeBehavior = mkEnumPreference(
  SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR,
  CloseBehavior.default
) { CloseBehavior.values().firstOrNull { it.name == this } }
```

Add the constant at the bottom of `AppPreferences` next to other `SHARED_PREFS_*` constants:

```kotlin
private const val SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR = "DesktopCloseBehavior"
```

**Verify.** Build: `./gradlew :common:desktopMainClasses` — succeeds. No behavior change yet.

**Commit.** `desktop: add CloseBehavior preference`

---

### Task 2 — Add `ComposeNativeTray` dependency

**Files**
- `apps/multiplatform/common/build.gradle.kts`

**What to add.** In the `desktopMain` dependencies block (around line 140), add:

```kotlin
implementation("io.github.kdroidfilter:composenativetray-jvm:1.3.0") {
  exclude(group = "net.java.dev.jna", module = "jna-jpms")
}
```

**Why this exclusion direction.** Existing build pulls `net.java.dev.jna:jna:5.14.0` and `net.java.dev.jna:jna-platform:5.14.0` directly (lines ~149–151). `jna-platform` requires classic `jna`. ComposeNativeTray brings `jna-jpms`, which collides because both ship the same `com.sun.jna.*` classes. We keep classic `jna` (existing) and drop `jna-jpms` (transitive from the new dep).

**Verify.**

```
./gradlew :common:desktopMainClasses
./gradlew :common:dependencies --configuration desktopRuntimeClasspath | grep -i jna
```

Expect: exactly one of `jna` and `jna-jpms` (the classic one) on the desktop classpath; build succeeds.

**Commit.** `desktop: add ComposeNativeTray dependency`

---

### Task 3 — Window-visibility state + branching close handler (no dialog, no tray yet)

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt`

**What to change.**

1. Add `windowVisible` to `SimplexWindowState` (the class at line 227):

```kotlin
class SimplexWindowState {
  // ...existing fields...
  val windowVisible = mutableStateOf(true)
}
```

2. In `AppWindow`, pass it to `Window`:

```kotlin
Window(
  state = windowState,
  visible = simplexWindowState.windowVisible.value,
  icon = painterResource(MR.images.ic_simplex),
  onCloseRequest = { handleCloseRequest(closedByError) },
  // ...rest unchanged...
)
```

3. Add the handler at file scope (or near `showApp`). Temporarily make `Ask` fall through to `Quit` — the dialog comes in Task 4:

```kotlin
private fun ApplicationScope.handleCloseRequest(closedByError: MutableState<Boolean>) {
  if (closedByError.value) { closedByError.value = false; exitApplication(); return }
  when (appPrefs.closeBehavior.get()) {
    CloseBehavior.Quit, CloseBehavior.Ask -> {
      closedByError.value = false
      exitApplication()
    }
    CloseBehavior.MinimizeToTray -> {
      simplexWindowState.windowVisible.value = false
    }
  }
}
```

(Imports: `chat.simplex.common.model.CloseBehavior`, `chat.simplex.common.model.ChatController.appPrefs`.)

**Verify.** Build + run desktop:

```
./gradlew :desktop:run
```

Click X — app exits exactly as today. No dialog, no tray. (Internal preference is `Ask`, branch falls through to Quit.)

**Commit.** `desktop: branch close handler on CloseBehavior preference`

---

### Task 4 — First-close dialog

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt` *(new)*
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt`
- `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`

**Strings.** Add to `strings.xml`:

```xml
<string name="close_behavior_dialog_title">Minimize to tray?</string>
<string name="close_behavior_dialog_text">If you choose Close, messages won\'t be received.\nYou can change it later in Appearance settings.</string>
<string name="close_behavior_dialog_close">Close the app</string>
<string name="close_behavior_dialog_minimize">Minimize to tray</string>
```

**`DesktopTray.kt` — dialog only.** A `mutableStateOf<Pair<onClose, onMinimize>?>` global, and a Composable that, when set, renders a non-dismissible `Dialog` with the two buttons. Skeleton:

```kotlin
package chat.simplex.common

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

private val pendingCloseChoice = mutableStateOf<CloseChoice?>(null)

private data class CloseChoice(val onClose: () -> Unit, val onMinimize: () -> Unit)

fun requestCloseBehavior(onClose: () -> Unit, onMinimize: () -> Unit) {
  pendingCloseChoice.value = CloseChoice(onClose, onMinimize)
}

@Composable
fun CloseBehaviorDialog() {
  val choice = pendingCloseChoice.value ?: return
  Dialog(
    onCloseRequest = { /* swallow — non-dismissible */ },
    state = rememberDialogState(width = 420.dp, height = 220.dp),
    title = stringResource(MR.strings.close_behavior_dialog_title),
    resizable = false,
  ) {
    Column(Modifier.padding(24.dp)) {
      Text(stringResource(MR.strings.close_behavior_dialog_text))
      Spacer(Modifier.height(24.dp))
      Row(horizontalArrangement = Arrangement.spacedBy(12.dp)) {
        Button(
          onClick = { pendingCloseChoice.value = null; choice.onClose() },
          colors = ButtonDefaults.buttonColors(backgroundColor = MaterialTheme.colors.error),
        ) { Text(stringResource(MR.strings.close_behavior_dialog_close)) }
        Button(
          onClick = { pendingCloseChoice.value = null; choice.onMinimize() },
          colors = ButtonDefaults.buttonColors(backgroundColor = MaterialTheme.colors.primary),
        ) { Text(stringResource(MR.strings.close_behavior_dialog_minimize)) }
      }
    }
  }
}
```

**Wire it up in `DesktopApp.kt`.** Inside `application(exitProcessOnExit = false) { … }`, render `CloseBehaviorDialog()` alongside `AppWindow`. Update `handleCloseRequest`'s `Ask` branch:

```kotlin
CloseBehavior.Ask -> requestCloseBehavior(
  onClose = {
    appPrefs.closeBehavior.set(CloseBehavior.Quit)
    closedByError.value = false
    exitApplication()
  },
  onMinimize = {
    appPrefs.closeBehavior.set(CloseBehavior.MinimizeToTray)
    simplexWindowState.windowVisible.value = false
  }
)
```

**Verify.** Run, click X — dialog appears with the exact text and button colors. Click "Close the app" → exits. Reopen, click X — exits without dialog (preference is `Quit`).

To reset the preference for re-testing, delete the SimpleX Chat desktop preferences file:
- Linux: `~/.config/simplex/SimpleXChatDesktop.properties`
- macOS: `~/Library/Preferences/SimpleXChatDesktop.properties`
- Windows: `%AppData%\SimpleX\SimpleXChatDesktop.properties`

Click "Minimize to tray" → window hides; the app process keeps running but is invisible (no tray icon yet — that's Task 6). Kill the JVM with Ctrl-C in the terminal to recover.

**Commit.** `desktop: first-close dialog for tray choice`

---

### Task 5 — Tray icon resources

**Files**
- `apps/multiplatform/common/src/commonMain/resources/MR/images/ic_simplex_tray.xml` (or `.png`)
- `apps/multiplatform/common/src/commonMain/resources/MR/images/ic_simplex_tray_dot.xml` (or `.png`)
- `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`

**Icons.** Two assets at 64×64 source size (per spec — single bitmap, OS handles downscale):

- `ic_simplex_tray` — base SimpleX glyph; same silhouette as `ic_simplex` but flat-coloured, no background, suitable for menu-bar/tray panels of various themes.
- `ic_simplex_tray_dot` — `ic_simplex_tray` with a small red filled circle in the bottom-right (~18×18 in the 64×64 frame).

Easiest: drop two PNGs into `MR/images/`. Moko picks them up; refer to as `MR.images.ic_simplex_tray` etc. Run a build to check generation: `./gradlew :common:generateMRcommonMain`.

**Strings.** Tray menu items + tooltip strings:

```xml
<string name="tray_show">Show SimpleX</string>
<string name="tray_quit">Quit SimpleX</string>
<string name="tray_tooltip">SimpleX</string>
<string name="tray_tooltip_unread">SimpleX — %d unread</string>
```

**Verify.** Build succeeds; the generated `MR.images.ic_simplex_tray` and `MR.strings.tray_*` symbols compile when referenced from a temporary scratch file (delete after).

**Commit.** `desktop: tray icon assets and menu strings`

---

### Task 6 — Tray composable (no unread indicator yet)

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt`
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt`

**Add to `DesktopTray.kt`.** Functions to show window and quit; the Tray composable itself.

```kotlin
import com.kdroid.composetray.tray.api.Tray
import com.kdroid.composetray.utils.SingleInstanceManager  // optional; only if we adopt single-instance later

fun showWindow() {
  simplexWindowState.windowVisible.value = true
  simplexWindowState.window?.toFront()
  simplexWindowState.window?.requestFocus()
}

fun quitApp(onQuit: () -> Unit) {
  pendingCloseChoice.value = null
  onQuit()
}

@Composable
fun ApplicationScope.SimplexTray(closedByError: MutableState<Boolean>) {
  if (appPrefs.closeBehavior.state.value != CloseBehavior.MinimizeToTray) return
  Tray(
    iconContent = { Image(painterResource(MR.images.ic_simplex_tray), contentDescription = null) },
    tooltip = stringResource(MR.strings.tray_tooltip),
    primaryAction = ::showWindow,
    menu = {
      Item(stringResource(MR.strings.tray_show), onClick = ::showWindow)
      Divider()
      Item(stringResource(MR.strings.tray_quit), onClick = {
        closedByError.value = false
        exitApplication()
      })
    }
  )
}
```

(Verify exact import paths and DSL element names against `composenativetray-jvm:1.3.0`'s README at https://github.com/kdroidFilter/ComposeNativeTray — the names above match the README at the time of writing but check if the build fails.)

**Wire into `DesktopApp.kt`.** Inside `application(exitProcessOnExit = false) { … }`:

```kotlin
SimplexTray(closedByError)
CloseBehaviorDialog()
AppWindow(closedByError)
```

The order doesn't affect rendering — the tray and dialog are top-level surfaces.

**Verify.** Run; in the dialog pick "Minimize to tray". Window hides; tray icon appears. Left-click tray — window restores. Right-click tray — menu has "Show SimpleX" and "Quit SimpleX". Both work. Quit, restart — preference persists; clicking X hides directly without dialog. Tray icon appears at app startup (because the preference is now `MinimizeToTray`).

**Commit.** `desktop: system tray icon with show/quit menu`

---

### Task 7 — Unread indicator + tooltip count

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt`

**Change `SimplexTray`.** Replace the static icon and tooltip with reactive ones:

```kotlin
val unread by remember {
  derivedStateOf { ChatModel.chats.value.sumOf { it.chatStats.unreadCount } }
}
val iconRes = if (unread > 0) MR.images.ic_simplex_tray_dot else MR.images.ic_simplex_tray
val tooltip =
  if (unread > 0) stringResource(MR.strings.tray_tooltip_unread, unread)
  else stringResource(MR.strings.tray_tooltip)

Tray(
  iconContent = { Image(painterResource(iconRes), contentDescription = null) },
  tooltip = tooltip,
  // primaryAction + menu unchanged
)
```

**Verify.**
1. With "Minimize to tray" enabled, hide the window.
2. Trigger a notification (have another account/contact send you a message; or open a direct chat with notifications enabled and post from another device).
3. Tray icon switches to the red-dot variant; tooltip shows "SimpleX — 1 unread" (or higher).
4. Click tray, view the message in the relevant chat. Icon reverts to the plain variant; tooltip becomes "SimpleX".

**Commit.** `desktop: unread indicator on tray icon`

---

### Task 8 — Appearance settings toggle

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/usersettings/Appearance.desktop.kt`
- `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`

**Strings.**

```xml
<string name="appearance_minimize_to_tray">Minimize to tray when closing window</string>
<string name="appearance_minimize_to_tray_desc">Keep SimpleX running in the background to receive messages.</string>
```

**UI row.** In `AppearanceLayout` (the Composable around line ~38), add a new section row using the existing `SectionItemView` / `SettingsActionItemWithContent` / similar patterns visible in this file. Read the surrounding rows for the exact convention; the snippet below is illustrative:

```kotlin
val pref = remember { appPrefs.closeBehavior.state }
val on = pref.value == CloseBehavior.MinimizeToTray
SectionItemView {
  Row(verticalAlignment = Alignment.CenterVertically) {
    Column(Modifier.weight(1f)) {
      Text(stringResource(MR.strings.appearance_minimize_to_tray))
      Text(
        stringResource(MR.strings.appearance_minimize_to_tray_desc),
        style = MaterialTheme.typography.caption,
        color = MaterialTheme.colors.onSurface.copy(alpha = 0.7f)
      )
    }
    Switch(
      checked = on,
      onCheckedChange = { checked ->
        appPrefs.closeBehavior.set(if (checked) CloseBehavior.MinimizeToTray else CloseBehavior.Quit)
      }
    )
  }
}
```

Place the row in the existing `AppearanceLayout` Composable, after the theme/dark-mode rows and before the language selector — that grouping is for general window-and-display preferences and the new toggle fits there. Match the styling of nearby rows. If a clearer section emerges during implementation, add a new `SectionView` with a "Window" header instead.

**Verify.** Open Appearance settings; toggle the row off — tray icon disappears; click X exits with no dialog. Toggle back on — tray icon reappears (Compose recomposes the gated `Tray` composable). Window-close behavior still depends on the toggle.

**Commit.** `desktop: Appearance toggle for minimize-to-tray`

---

### Final manual test pass

Run the full test plan from the spec on each platform you can reach (Linux KDE, Windows 11, macOS):

1. Fresh install (clear `~/.config/simplex/` or per-OS data dir). Click X → dialog with the right text and button colors. Esc / outside-tap do nothing.
2. Pick Close → exits. Reopen → click X → exits with no dialog.
3. Reset, pick Minimize to tray → window hides, tray icon shows.
4. Receive a message → red-dot variant + tooltip count.
5. Click tray → window restores and focuses (acceptable if focus is best-effort per spec).
6. Right-click tray → Show / Quit both work.
7. Appearance toggle off → tray vanishes, X exits without dialog.
8. Appearance toggle on → tray reappears.

If anything fails, file follow-ups; the spec's "out of scope" list catches the expected omissions (autostart, number-on-icon, etc.).
