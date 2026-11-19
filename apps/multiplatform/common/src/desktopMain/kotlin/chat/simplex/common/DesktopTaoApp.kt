package chat.simplex.common

import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowExceptionHandler
import androidx.compose.ui.window.WindowPlacement
import androidx.compose.ui.window.rememberWindowState
import chat.simplex.common.model.ChatController
import chat.simplex.common.ui.theme.DEFAULT_START_MODAL_WIDTH
import chat.simplex.common.views.helpers.fontSizeSqrtMultiplier
import chat.simplex.res.MR
import dev.nucleusframework.core.runtime.ActivationToken
import dev.nucleusframework.window.tao.ApplicationScope
import dev.nucleusframework.window.tao.DecoratedWindow
import dev.nucleusframework.window.tao.LocalWindowExceptionHandlerFactory
import dev.nucleusframework.window.tao.WindowExceptionHandlerFactory
import dev.nucleusframework.window.tao.taoApplication
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

private const val NUCLEUS_LIBRARY_PATH = "nucleus.native.libraryPath"
private const val COMPOSE_RESOURCES_DIR = "compose.application.resources.dir"

fun useAppNativeLibraries() {
  System.getProperty(COMPOSE_RESOURCES_DIR)?.let { System.setProperty(NUCLEUS_LIBRARY_PATH, it) }
}

@OptIn(ExperimentalComposeUiApi::class)
fun showTaoApp() {
  taoApplication {
    var crashes by remember { mutableIntStateOf(0) }
    CompositionLocalProvider(
      LocalWindowExceptionHandlerFactory provides WindowExceptionHandlerFactory {
        WindowExceptionHandler { e ->
          showCrashAlert(e)
          resetAfterCrash()
          crashes++
        }
      }
    ) {
      key(crashes) {
        TaoAppWindow()
      }
    }
  }
}

@Composable
private fun ApplicationScope.TaoAppWindow() {
  val windowState = rememberStoredWindowState()
  if (remember { ChatController.appPrefs.appLanguage.state }.value != "") {
    DecoratedWindow(
      onCloseRequest = { handleCloseRequest(false, ::exitApplication) },
      state = windowState,
      title = "SimpleX",
      icon = painterResource(MR.images.ic_simplex),
      visible = simplexWindowState.windowVisible.value,
      onKeyEvent = ::closeOnEscape,
      systemDecorations = true,
    ) {
      val taoWindow = window
      DisposableEffect(taoWindow) {
        simplexWindowState.raise = { activationToken ->
          activationToken?.let(ActivationToken::offer)
          taoWindow.show()
          taoWindow.focus()
        }
        taoWindow.onFocusChanged(::onWindowFocusChanged)
        onDispose { simplexWindowState.raise = {} }
      }
      AppScreen()
      AppWindowOverlays()
    }
  }
  if (terminalWindowShown()) {
    var hiddenUntilRestart by remember { mutableStateOf(false) }
    if (!hiddenUntilRestart) {
      val terminalWindowState = rememberWindowState(placement = WindowPlacement.Floating, width = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier, height = 768.dp)
      DecoratedWindow(
        onCloseRequest = { hiddenUntilRestart = true },
        state = terminalWindowState,
        title = stringResource(MR.strings.chat_console),
        systemDecorations = true,
      ) {
        TerminalWindowContent { hiddenUntilRestart = true }
      }
    }
  }
}
