package chat.simplex.common

import chat.simplex.common.platform.desktopPlatform
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import java.io.File

var windowPositionSizeSavePath = desktopPlatform.dataPath + File.separator + "/window.json"

@Serializable
data class WindowPositionSize(
  var width: Int = if (desktopPlatform.isLinux()) 1376 else 1366,
  var height: Int = 768,
  var x: Int = 0,
  var y: Int = 0,
)

fun getStoredWindowPositionSize() : WindowPositionSize {
  // check if window position/size state file exists
  val saveFile = File(windowPositionSizeSavePath)

  var data = WindowPositionSize()

  if (!saveFile.exists()) {
    storeWindowPositionSize(data)
  } else {
    // load stored data into the WindowPositionSize instance
    data = Json.decodeFromString<WindowPositionSize>(saveFile.readText())
  }

  return data
}

fun storeWindowPositionSize(data: WindowPositionSize) {
  val saveFile = File(windowPositionSizeSavePath)
  val json = Json.encodeToString(data)
  saveFile.writeText(json)
}