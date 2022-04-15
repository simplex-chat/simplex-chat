package chat.simplex.app.views.helpers

import android.content.Context
import android.graphics.Rect
import android.view.ViewTreeObserver
import androidx.compose.runtime.*
import androidx.compose.ui.platform.LocalView
import kotlinx.coroutines.*

fun withApi(action: suspend CoroutineScope.() -> Unit): Job =
  GlobalScope.launch { withContext(Dispatchers.Main, action) }

enum class KeyboardState {
  Opened, Closed
}

@Composable
fun getKeyboardState(): State<KeyboardState> {
  val keyboardState = remember { mutableStateOf(KeyboardState.Closed) }
  val view = LocalView.current
  DisposableEffect(view) {
    val onGlobalListener = ViewTreeObserver.OnGlobalLayoutListener {
      val rect = Rect()
      view.getWindowVisibleDisplayFrame(rect)
      val screenHeight = view.rootView.height
      val keypadHeight = screenHeight - rect.bottom
      keyboardState.value = if (keypadHeight > screenHeight * 0.15) {
        KeyboardState.Opened
      } else {
        KeyboardState.Closed
      }
    }
    view.viewTreeObserver.addOnGlobalLayoutListener(onGlobalListener)

    onDispose {
      view.viewTreeObserver.removeOnGlobalLayoutListener(onGlobalListener)
    }
  }

  return keyboardState
}

fun getFilesDirectory(context: Context): String {
  return context.filesDir.toString()
}

fun getAppFilesDirectory(context: Context): String {
  return getFilesDirectory(context) + "/app_files"
}
