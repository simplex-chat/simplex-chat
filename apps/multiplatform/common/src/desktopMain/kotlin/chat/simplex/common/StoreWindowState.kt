package chat.simplex.common

import chat.simplex.common.model.json
import chat.simplex.common.platform.appPreferences
import chat.simplex.common.platform.desktopPlatform
import kotlinx.serialization.*

@Serializable
data class WindowPositionSize(
  val width: Int = 1366,
  val height: Int = 768,
  val x: Int = 0,
  val y: Int = 0,
)

fun getStoredWindowState(): WindowPositionSize =
  try {
    val str = appPreferences.desktopWindowState.get()
    var state = if (str == null) {
      WindowPositionSize()
    } else {
      json.decodeFromString(str)
    }

    // For some reason on Linux actual width will be 10.dp less after specifying it here. If we specify 1366,
    // it will show 1356. But after that we can still update it to 1366 by changing window state. Just making it +10 now here
    if (desktopPlatform.isLinux() && state.width == 1366) {
      state = state.copy(width = 1376)
    }
    state
  } catch (e: Throwable) {
    WindowPositionSize()
  }

fun storeWindowState(state: WindowPositionSize) =
  appPreferences.desktopWindowState.set(json.encodeToString(state))
