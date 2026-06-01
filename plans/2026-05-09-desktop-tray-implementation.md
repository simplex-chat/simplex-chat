# Desktop tray icon — implementation plan

Companion to the design at `plans/2026-05-09-desktop-tray.md`. Read that first.

## What

Seven small commits that build the feature incrementally. After each commit the build is green and the app still runs; only the last commit makes the feature visible to the user end-to-end.

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
val closeBehavior: SharedPreference<CloseBehavior> =
  mkSafeEnumPreference(SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR, CloseBehavior.default)
```

Add the constant at the bottom of `AppPreferences` next to other `SHARED_PREFS_*` constants:

```kotlin
private const val SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR = "DesktopCloseBehavior"
```

**Verify.** Build: `./gradlew :common:desktopMainClasses` — succeeds. No behavior change yet.

**Commit.** `desktop: add CloseBehavior preference`

---

### Task 2 — Window-visibility state + branching close handler (no dialog, no tray yet)

**(Note: a `Task 2 — Add ComposeNativeTray dependency` is removed. We now use Compose Multiplatform's built-in `androidx.compose.ui.window.Tray`, already on the classpath via the `org.jetbrains.compose` plugin. No new dep.)**

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

3. Add the handler at file scope (or near `showApp`). Temporarily make `Ask` fall through to `Quit` — the dialog comes in Task 3:

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

The `MinimizeToTray` branch will get a tray-availability guard in Task 5 (defensive: a user could have set the pref on a different machine where tray works).

(Imports: `chat.simplex.common.model.CloseBehavior`, `chat.simplex.common.model.ChatController.appPrefs`.)

**Verify.** Build + run desktop:

```
./gradlew :desktop:run
```

Click X — app exits exactly as today. No dialog, no tray. (Internal preference is `Ask`, branch falls through to Quit.)

**Commit.** `desktop: branch close handler on CloseBehavior preference`

---

### Task 3 — First-close dialog

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
        // Hide the Minimize button when tray isn't supported (stock GNOME).
        // The dialog still asks once so the user gets a definitive Quit answer
        // and doesn't see the dialog again. trayIsAvailable is defined in Task 5;
        // until then, the button is always shown.
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

### Task 4 — Tray icon resources

**Files**
- `apps/multiplatform/common/src/commonMain/resources/MR/images/ic_simplex_tray_dot.svg`
- `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`

**Icons.** Reuse the existing `MR.images.ic_simplex` for the no-unread case and add a single new asset for the unread case:

- `ic_simplex_tray_dot` — copy of `ic_simplex.svg` with a small red filled circle added in the bottom-right (~6px radius in the 40×40 viewBox).

Drop the SVG into `MR/images/`. Moko picks it up; refer to as `MR.images.ic_simplex_tray_dot`. Run a build to check generation: `./gradlew :common:generateMRcommonMain`.

**Strings.** Tray menu items + tooltip strings:

```xml
<string name="tray_show">Show SimpleX</string>
<string name="tray_quit">Quit SimpleX</string>
<string name="tray_tooltip">SimpleX</string>
<string name="tray_tooltip_unread">SimpleX — %d unread</string>
```

**Verify.** Build succeeds; the generated `MR.images.ic_simplex_tray_dot` and `MR.strings.tray_*` symbols compile when referenced from a temporary scratch file (delete after).

**Commit.** `desktop: tray icon assets and menu strings`

---

### Task 5 — Tray composable (no unread indicator yet)

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt`
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt`

**Add to `DesktopTray.kt`.** Tray-availability probe, functions to show window and quit, the Tray composable itself.

```kotlin
import androidx.compose.ui.window.ApplicationScope
import androidx.compose.ui.window.Tray
import androidx.compose.ui.window.MenuBar
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.awt.SystemTray

// Probed once at startup. Performs a real add/remove of a transparent TrayIcon
// because SystemTray.isSupported() can return true while add() throws (JDK-8322750).
val trayIsAvailable: Boolean by lazy {
  if (!SystemTray.isSupported()) return@lazy false
  try {
    val tray = SystemTray.getSystemTray()
    val probe = TrayIcon(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB))
    tray.add(probe); tray.remove(probe); true
  } catch (e: AWTException) { false } catch (e: SecurityException) { false }
}

fun showWindow() {
  simplexWindowState.windowVisible.value = true
  simplexWindowState.window?.toFront()
  simplexWindowState.window?.requestFocus()
}

@Composable
fun ApplicationScope.SimplexTray(closedByError: MutableState<Boolean>) {
  if (!trayIsAvailable) return
  if (appPrefs.closeBehavior.state.value != CloseBehavior.MinimizeToTray) return
  Tray(
    icon = painterResource(MR.images.ic_simplex_tray),
    tooltip = stringResource(MR.strings.tray_tooltip),
    onAction = ::showWindow,
    menu = {
      Item(stringResource(MR.strings.tray_show), onClick = ::showWindow)
      Separator()
      Item(stringResource(MR.strings.tray_quit), onClick = {
        closedByError.value = false
        exitApplication()
      })
    }
  )
}
```

(Note: this uses Compose Multiplatform's built-in `androidx.compose.ui.window.Tray`. The API is `icon: Painter`, `onAction` (not `primaryAction`), menu DSL uses `Separator()` (not `Divider()`).)

**Update `DesktopApp.kt`'s close handler** to add the defensive tray-availability check from Task 2's TODO:

```kotlin
CloseBehavior.MinimizeToTray -> {
  if (trayIsAvailable) {
    simplexWindowState.windowVisible.value = false
  } else {
    closedByError.value = false
    exitApplication()
  }
}
```

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

### Task 6 — Unread indicator + tooltip count

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt`

**Change `SimplexTray`.** Replace the static icon and tooltip with reactive ones:

```kotlin
// UserInfo.unreadCount is incremented only when ntfsEnabled(item) — see SimpleXAPI.kt:2781-2783.
val unread by remember {
  derivedStateOf { ChatModel.users.sumOf { it.unreadCount } }
}
val iconRes = if (unread > 0) MR.images.ic_simplex_tray_dot else MR.images.ic_simplex
val tooltip =
  if (unread > 0) stringResource(MR.strings.tray_tooltip_unread, unread)
  else stringResource(MR.strings.tray_tooltip)

Tray(
  icon = painterResource(iconRes),
  tooltip = tooltip,
  // onAction + menu unchanged
)
```

**Verify.**
1. With "Minimize to tray" enabled, hide the window.
2. Trigger a notification (have another account/contact send you a message; or open a direct chat with notifications enabled and post from another device).
3. Tray icon switches to the red-dot variant; tooltip shows "SimpleX — 1 unread" (or higher).
4. Click tray, view the message in the relevant chat. Icon reverts to the plain variant; tooltip becomes "SimpleX".

**Commit.** `desktop: unread indicator on tray icon`

---

### Task 7 — Appearance settings toggle

**Files**
- `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/usersettings/Appearance.desktop.kt`
- `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`

**Strings.**

```xml
<string name="appearance_minimize_to_tray">Minimize to tray when closing window</string>
<string name="appearance_minimize_to_tray_desc">Keep SimpleX running in the background to receive messages.</string>
```

**UI row.** In `AppearanceLayout` (the Composable around line ~38), add a new section row using the existing `SectionItemView` / `SettingsActionItemWithContent` / similar patterns visible in this file. The entire row is gated on `trayIsAvailable` — if the OS has no tray host, the toggle is omitted. Read the surrounding rows for the exact convention; the snippet below is illustrative:

```kotlin
if (trayIsAvailable) {
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
