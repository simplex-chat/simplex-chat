package chat.simplex.common

import chat.simplex.common.model.json
import chat.simplex.common.platform.appPreferences
import chat.simplex.common.ui.theme.DEFAULT_WINDOW_WIDTH
import kotlinx.serialization.*

@Serializable
data class WindowPositionSize(
  val width: Int = DEFAULT_WINDOW_WIDTH.value.toInt(),
  val height: Int = 768,
  val x: Int = 0,
  val y: Int = 0,
)

fun getStoredWindowState(): WindowPositionSize =
  try {
    val str = appPreferences.desktopWindowState.get()
    if (str == null) {
      WindowPositionSize()
    } else {
      json.decodeFromString(str)
    }
  } catch (e: Throwable) {
    WindowPositionSize()
  }

fun storeWindowState(state: WindowPositionSize) =
  appPreferences.desktopWindowState.set(json.encodeToString(state))
