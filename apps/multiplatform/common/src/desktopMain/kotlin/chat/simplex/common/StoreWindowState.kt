package chat.simplex.common

import chat.simplex.common.platform.appPreferences
import chat.simplex.common.platform.desktopPlatform
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import java.io.File

@Serializable
data class WindowPositionSize(
  var width: Int = if (desktopPlatform.isLinux()) 1376 else 1366,
  var height: Int = 768,
  var x: Int = 0,
  var y: Int = 0,
)

fun getStoredWindowState() : WindowPositionSize {
  return Json.decodeFromString<WindowPositionSize>(appPreferences.windowState.get() ?: "{}")
}

fun storeWindowState(data: WindowPositionSize) {
  appPreferences.windowState.set(Json.encodeToString<WindowPositionSize>(data))
}
