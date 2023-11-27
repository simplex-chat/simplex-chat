package chat.simplex.common

import chat.simplex.common.platform.appPreferences
import chat.simplex.common.platform.desktopPlatform
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import java.io.File

val defaultWindowStateJson = """{"width"\:1370,"height"\:780,"x"\:0,"y"\:0}"""

@Serializable
data class WindowPositionSize(
  val width: Int = if (desktopPlatform.isLinux()) 1376 else 1366,
  val height: Int = 768,
  val x: Int = 0,
  val y: Int = 0,
)

fun getStoredWindowState() : WindowPositionSize {
  return try {
    Json.decodeFromString<WindowPositionSize>(appPreferences.windowState.get() ?: defaultWindowStateJson)
  } catch (e: Throwable) {
    WindowPositionSize()
  }
}

fun storeWindowState(data: WindowPositionSize) {
  appPreferences.windowState.set(
    try {
      Json.encodeToString<WindowPositionSize>(data)
    } catch (e: Throwable) {
      defaultWindowStateJson
    }
  )
}
