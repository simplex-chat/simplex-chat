package chat.simplex.common

import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowExceptionHandler
import androidx.compose.ui.window.WindowPlacement
import androidx.compose.ui.window.rememberWindowState
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.NtfManager
import chat.simplex.common.platform.FileChooserAction
import chat.simplex.common.platform.LinuxNative
import chat.simplex.common.platform.adjustToLinuxRenderer
import chat.simplex.common.platform.fileChooserPatterns
import chat.simplex.common.platform.fileFromNativePath
import chat.simplex.common.ui.theme.DEFAULT_START_MODAL_WIDTH
import chat.simplex.common.views.helpers.fontSizeSqrtMultiplier
import chat.simplex.res.MR
import dev.nucleusframework.core.runtime.ActivationToken
import dev.nucleusframework.window.tao.ApplicationScope
import dev.nucleusframework.window.tao.DecoratedWindow
import dev.nucleusframework.window.tao.LocalWindowExceptionHandlerFactory
import dev.nucleusframework.window.tao.TaoWindow
import dev.nucleusframework.window.tao.WindowExceptionHandlerFactory
import dev.nucleusframework.window.tao.taoApplication
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.suspendCancellableCoroutine
import kotlinx.coroutines.withContext
import java.io.File
import kotlin.coroutines.resume

private const val FILE_DIALOG_TITLE = "SimpleX"

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
    LinuxTray()
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
        taoWindow.onFocusChanged { focused ->
          onWindowFocusChanged(focused)
          if (focused) NtfManager.clearUrgency()
        }
        onDispose { simplexWindowState.raise = {} }
      }
      LaunchedEffect(Unit) {
        withFrameNanos {}
        withFrameNanos {}
        adjustToLinuxRenderer(ChatController.appPrefs)
      }
      AppScreen()
      GtkFileDialogs(taoWindow)
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

@Composable
private fun GtkFileDialogs(window: TaoWindow) {
  val openDialog = simplexWindowState.openDialog
  if (openDialog.isAwaiting) {
    LaunchedEffect(openDialog.params) {
      openDialog.onResult(chooseFiles(window, FileChooserAction.Open, openDialog.params).firstOrNull())
    }
  }
  val openMultipleDialog = simplexWindowState.openMultipleDialog
  if (openMultipleDialog.isAwaiting) {
    LaunchedEffect(openMultipleDialog.params) {
      openMultipleDialog.onResult(chooseFiles(window, FileChooserAction.Open, openMultipleDialog.params))
    }
  }
  val saveDialog = simplexWindowState.saveDialog
  if (saveDialog.isAwaiting) {
    val action = if (saveDialog.params.filename != null) FileChooserAction.Save else FileChooserAction.SelectFolder
    LaunchedEffect(saveDialog.params) {
      saveDialog.onResult(chooseFiles(window, action, saveDialog.params).firstOrNull())
    }
  }
}

private suspend fun chooseFiles(window: TaoWindow, action: FileChooserAction, params: DialogParams): List<File> {
  if (!LinuxNative.isLoaded) return emptyList()
  val files = withContext(Dispatchers.Main) {
    suspendCancellableCoroutine { continuation ->
      LinuxNative.nativeChooseFiles(
        window.linuxGtkWindowHandle,
        FILE_DIALOG_TITLE,
        action.gtkAction,
        params.allowMultiple,
        params.filename,
        params.fileFilterDescription,
        params.fileExtensions?.let(::fileChooserPatterns),
      ) { paths -> continuation.resume(paths?.map(::fileFromNativePath) ?: emptyList()) }
    }
  }
  return if (action == FileChooserAction.Open) files.filter { it.canRead() } else files
}
