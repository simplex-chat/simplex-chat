package chat.simplex.common.platform

import android.app.Activity
import android.content.Context
import android.content.pm.ActivityInfo
import android.graphics.Rect
import android.os.*
import android.view.*
import android.view.inputmethod.InputMethodManager
import android.widget.Toast
import androidx.activity.compose.setContent
import androidx.compose.runtime.*
import androidx.compose.ui.platform.LocalView
import chat.simplex.common.AppScreen
import chat.simplex.common.views.helpers.*
import androidx.compose.ui.platform.LocalContext as LocalContext1
import chat.simplex.res.MR

actual fun showToast(text: String, timeout: Long) = Toast.makeText(androidAppContext, text, Toast.LENGTH_SHORT).show()

@Composable
actual fun LockToCurrentOrientationUntilDispose() {
  val context = LocalContext1.current
  DisposableEffect(Unit) {
    val activity = (context as Activity?) ?: return@DisposableEffect onDispose {}
    val manager = context.getSystemService(Activity.WINDOW_SERVICE) as WindowManager
    val rotation = if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.Q) manager.defaultDisplay.rotation else activity.display?.rotation
    activity.requestedOrientation = when (rotation) {
      Surface.ROTATION_90 -> ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE
      Surface.ROTATION_180 -> ActivityInfo.SCREEN_ORIENTATION_REVERSE_PORTRAIT
      Surface.ROTATION_270 -> ActivityInfo.SCREEN_ORIENTATION_REVERSE_LANDSCAPE
      else -> ActivityInfo.SCREEN_ORIENTATION_PORTRAIT
    }
    // Unlock orientation
    onDispose { activity.requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED }
  }
}

@Composable
actual fun LocalMultiplatformView(): Any? = LocalView.current

@Composable
actual fun getKeyboardState(): State<KeyboardState> {
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

actual fun hideKeyboard(view: Any?) {
  // LALAL
  //  LocalSoftwareKeyboardController.current?.hide()
  if (view is View) {
    (androidAppContext.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager).hideSoftInputFromWindow(view.windowToken, 0)
  }
}

actual fun androidIsFinishingMainActivity(): Boolean = (mainActivity.get()?.isFinishing == true)

actual class GlobalExceptionsHandler: Thread.UncaughtExceptionHandler {
  actual override fun uncaughtException(thread: Thread, e: Throwable) {
    Log.e(TAG, "App crashed, thread name: " + thread.name + ", exception: " + e.stackTraceToString())
    includeMoreFailedComposables()
    if (ModalManager.start.hasModalsOpen()) {
      ModalManager.start.closeModal()
    } else if (chatModel.chatId.value != null) {
      // Since no modals are open, the problem is probably in ChatView
      chatModel.chatId.value = null
      chatModel.chatItems.clear()
    } else {
      // ChatList, nothing to do. Maybe to show other view except ChatList
    }
    chatModel.activeCall.value?.let {
      withBGApi {
        chatModel.callManager.endCall(it)
      }
    }
    if (thread.name == "main") {
      mainActivity.get()?.recreate()
    } else {
      mainActivity.get()?.apply {
        window
          ?.decorView
          ?.findViewById<ViewGroup>(android.R.id.content)
          ?.removeViewAt(0)
        setContent {
          AppScreen()
        }
      }
    }
    // Wait until activity recreates to prevent showing two alerts (in case `main` was crashed)
    Handler(Looper.getMainLooper()).post {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.app_was_crashed),
        text = e.stackTraceToString()
      )
    }
  }
}
