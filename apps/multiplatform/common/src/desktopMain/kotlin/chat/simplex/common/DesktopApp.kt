package chat.simplex.common

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.key.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.*
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.defaultLocale
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.helpers.FileDialogChooser
import kotlinx.coroutines.*
import java.awt.event.WindowEvent
import java.awt.event.WindowFocusListener
import java.io.File

val simplexWindowState = SimplexWindowState()

fun showApp() = application {
  // TODO: remove after update to compose 1.5.0+
  // See: https://github.com/JetBrains/compose-multiplatform/issues/3366#issuecomment-1643799976
  System.setProperty("compose.scrolling.smooth.enabled", "false")

  // For some reason on Linux actual width will be 10.dp less after specifying it here. If we specify 1366,
  // it will show 1356. But after that we can still update it to 1366 by changing window state. Just making it +10 now here
  val width = if (desktopPlatform.isLinux()) 1376.dp else 1366.dp
  val windowState = rememberWindowState(placement = WindowPlacement.Floating, width = width, height = 768.dp)
  simplexWindowState.windowState = windowState
  // Reload all strings in all @Composable's after language change at runtime
  if (remember { ChatController.appPrefs.appLanguage.state }.value != "") {
    Window(state = windowState, onCloseRequest = ::exitApplication, onKeyEvent = {
      if (it.key == Key.Escape && it.type == KeyEventType.KeyUp) {
        simplexWindowState.backstack.lastOrNull()?.invoke() != null
      } else {
        false
      }
    }, title = "SimpleX") {
      SimpleXTheme {
        AppScreen()
        if (simplexWindowState.openDialog.isAwaiting) {
          FileDialogChooser(
            title = "SimpleX",
            isLoad = true,
            params = simplexWindowState.openDialog.params,
            onResult = {
              simplexWindowState.openDialog.onResult(it.firstOrNull())
            }
          )
        }

        if (simplexWindowState.openMultipleDialog.isAwaiting) {
          FileDialogChooser(
            title = "SimpleX",
            isLoad = true,
            params = simplexWindowState.openMultipleDialog.params,
            onResult = {
              simplexWindowState.openMultipleDialog.onResult(it)
            }
          )
        }

        if (simplexWindowState.saveDialog.isAwaiting) {
          FileDialogChooser(
            title = "SimpleX",
            isLoad = false,
            params = simplexWindowState.saveDialog.params,
            onResult = { simplexWindowState.saveDialog.onResult(it.firstOrNull()) }
          )
        }
        val toasts = remember { simplexWindowState.toasts }
        val toast = toasts.firstOrNull()
        if (toast != null) {
          Box(Modifier.fillMaxSize().padding(bottom = 20.dp), contentAlignment = Alignment.BottomCenter) {
            Text(
              toast.first,
              Modifier.background(MaterialTheme.colors.primary, RoundedCornerShape(100)).padding(vertical = 5.dp, horizontal = 10.dp),
              color = MaterialTheme.colors.onPrimary,
              style = MaterialTheme.typography.body1
            )
          }
          // Shows toast in insertion order with preferred delay per toast. New one will be shown once previous one expires
          LaunchedEffect(toast, toasts.size) {
            delay(toast.second)
            simplexWindowState.toasts.removeFirst()
          }
        }
      }
      var windowFocused by remember { simplexWindowState.windowFocused }
      LaunchedEffect(windowFocused) {
        val delay = ChatController.appPrefs.laLockDelay.get()
        if (!windowFocused && ChatModel.performLA.value && delay > 0) {
          delay(delay * 1000L)
          // Trigger auth state check when delay ends (and if it ends)
          AppLock.recheckAuthState()
        }
      }
      LaunchedEffect(Unit) {
        window.addWindowFocusListener(object: WindowFocusListener {
          override fun windowGainedFocus(p0: WindowEvent?) {
            windowFocused = true
            AppLock.recheckAuthState()
          }

          override fun windowLostFocus(p0: WindowEvent?) {
            windowFocused = false
            AppLock.appWasHidden()
          }
        })
      }
    }
  }
}

class SimplexWindowState {
  lateinit var windowState: WindowState
  val backstack = mutableStateListOf<() -> Unit>()
  val openDialog = DialogState<File?>()
  val openMultipleDialog = DialogState<List<File>>()
  val saveDialog = DialogState<File?>()
  val toasts = mutableStateListOf<Pair<String, Long>>()
  var windowFocused = mutableStateOf(true)
}

data class DialogParams(
  val filename: String? = null,
  val allowMultiple: Boolean = false,
  val fileFilter: ((File?) -> Boolean)? = null,
  val fileFilterDescription: String = "",
)

class DialogState<T> {
  private var onResult: CompletableDeferred<T>? by mutableStateOf(null)
  var params = DialogParams()
  val isAwaiting get() = onResult != null

  suspend fun awaitResult(params: DialogParams = DialogParams()): T {
    onResult = CompletableDeferred()
    this.params = params
    val result = onResult!!.await()
    onResult = null
    return result
  }

  fun onResult(result: T) = onResult!!.complete(result)
}

@Preview
@Composable
fun AppPreview() {
  SimpleXTheme {
    AppScreen()
  }
}
