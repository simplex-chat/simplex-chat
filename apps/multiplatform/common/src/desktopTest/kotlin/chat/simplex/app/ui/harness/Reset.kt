package chat.simplex.app.ui.harness

import androidx.compose.runtime.MutableState
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.CompletableDeferred
import java.io.File
import java.util.Properties
import javax.swing.SwingUtilities

object PrefStores {
  private val resources = Class.forName("chat.simplex.common.platform.Resources_desktopKt")
  private val settingsProps = staticField(resources, "settingsProps") as Properties
  private val themesProps = staticField(resources, "settingsThemesProps") as Properties
  private val settingsFile get() = File(desktopPlatform.configPath, "settings.properties")
  private val themesFile get() = File(desktopPlatform.configPath, "themes.properties")
  private val themesYaml get() = File(getPreferenceFilePath("themes.yaml"))

  private lateinit var settingsCapture: Properties
  private lateinit var themesCapture: Properties
  private var yamlCapture: ByteArray? = null

  fun capture() {
    settingsCapture = settingsProps.clone() as Properties
    themesCapture = themesProps.clone() as Properties
    yamlCapture = themesYaml.takeIf { it.exists() }?.readBytes()
  }

  fun restore() {
    restoreProps(settingsProps, settingsCapture, settingsFile)
    restoreProps(themesProps, themesCapture, themesFile)
    val yaml = yamlCapture
    if (yaml == null) themesYaml.delete() else themesYaml.apply { parentFile.mkdirs() }.writeBytes(yaml)
    CommonPrefs.reload()
  }

  fun reloadFromDisk() {
    loadProps(settingsProps, settingsFile)
    loadProps(themesProps, themesFile)
    CommonPrefs.reload()
  }

  private fun loadProps(live: Properties, file: File) {
    synchronized(live) {
      live.clear()
      if (file.exists()) file.reader().use { live.load(it) }
    }
  }

  private fun restoreProps(live: Properties, captured: Properties, file: File) {
    synchronized(live) {
      live.clear()
      live.putAll(captured)
      file.parentFile.mkdirs()
      file.writer().use { live.store(it, "") }
    }
  }

}

object UiState {
  fun clearUiState() {
    CommonUiState.clear(::onEdt)
    onEdt {
      simplexWindowState.windowState = WindowState(size = DpSize(1600.dp, 900.dp))
      simplexWindowState.windowFocused.value = true
      simplexWindowState.windowVisible.value = true
    }
    clearTransientUi()
  }

  fun clearTransientUi() {
    CommonUiState.clearTransient(::onEdt)
    onEdt {
      completePendingDialogs()
      simplexWindowState.toasts.clear()
    }
  }

  fun completePendingDialogs() {
    val onResult = chat.simplex.common.DialogState::class.java.getDeclaredField("onResult\$delegate").apply { isAccessible = true }
    with(simplexWindowState) {
      for ((ds, empty) in listOf(openDialog to null, saveDialog to null, openMultipleDialog to emptyList<File>())) {
        @Suppress("UNCHECKED_CAST")
        val d = (onResult.get(ds) as MutableState<CompletableDeferred<Any?>?>).value
        if (d != null && !d.isCompleted) d.complete(empty)
      }
    }
  }
}

private fun staticField(cls: Class<*>, name: String): Any? = cls.getDeclaredField(name).apply { isAccessible = true }.get(null)

internal fun onEdt(action: () -> Unit) {
  if (SwingUtilities.isEventDispatchThread()) action() else SwingUtilities.invokeAndWait(action)
}

