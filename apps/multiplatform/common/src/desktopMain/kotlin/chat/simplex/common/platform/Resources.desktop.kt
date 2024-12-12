package chat.simplex.common.platform

import androidx.compose.runtime.*
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toComposeImageBitmap
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.*
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import com.jthemedetecor.OsThemeDetector
import com.russhwolf.settings.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.desc.desc
import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.*

@Composable
actual fun font(name: String, res: String, weight: FontWeight, style: FontStyle): Font =
  androidx.compose.ui.text.platform.Font("MR/fonts/$res.ttf", weight, style)

actual fun StringResource.localized(): String = desc().toString()

private val detector: OsThemeDetector = OsThemeDetector.getDetector()
actual fun isInNightMode() = try {
  detector.isDark
}
catch (e: Exception) {
  Log.e(TAG, e.stackTraceToString())
  /* On Mac this code can produce exception */
  false
}

private val settingsFile =
  File(desktopPlatform.configPath + File.separator + "settings.properties")
private val settingsThemesFile =
  File(desktopPlatform.configPath + File.separator + "themes.properties")

private val settingsProps =
  Properties()
    .also { props ->
      if (!settingsFile.exists()) return@also

      try {
        settingsFile.reader().use {
          // Force exception to happen
          //it.close()
          props.load(it)
        }
      } catch (e: Exception) {
        Log.e(TAG, "Error reading settings file: ${e.stackTraceToString()}")
      }
    }
private val settingsThemesProps =
  Properties()
    .also { props -> try { settingsThemesFile.reader().use { props.load(it) } } catch (e: Exception) { /**/ } }

private const val lock = "settingsSaver"
actual val settings: Settings = PropertiesSettings(settingsProps) {
  synchronized(lock) {
    try {
      createTmpFileAndDelete(preferencesTmpDir) { tmpFile ->
        tmpFile.writer().use { settingsProps.store(it, "") }
        settingsFile.parentFile.mkdirs()
        Files.move(tmpFile.toPath(), settingsFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
      }
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.prefs_error_saving_settings), e.stackTraceToString())
      throw e
    }
  }
}
actual val settingsThemes: Settings = PropertiesSettings(settingsThemesProps) {
  synchronized(lock) {
    try {
      createTmpFileAndDelete(preferencesTmpDir) { tmpFile ->
        tmpFile.writer().use { settingsThemesProps.store(it, "") }
        settingsThemesFile.parentFile.mkdirs()
        Files.move(tmpFile.toPath(), settingsThemesFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
      }
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.prefs_error_saving_settings), e.stackTraceToString())
      throw e
    }
  }
}

actual fun windowOrientation(): WindowOrientation =
  if (simplexWindowState.windowState.size.width > simplexWindowState.windowState.size.height) {
    WindowOrientation.LANDSCAPE
  } else {
    WindowOrientation.PORTRAIT
  }

@Composable
actual fun windowWidth(): Dp = simplexWindowState.windowState.size.width

@Composable
actual fun windowHeight(): Dp = simplexWindowState.windowState.size.height

actual fun desktopExpandWindowToWidth(width: Dp) {
  if (simplexWindowState.windowState.size.width >= width) return
  simplexWindowState.windowState.size = simplexWindowState.windowState.size.copy(width = width)
}

actual fun isRtl(text: CharSequence): Boolean {
  if (text.isEmpty()) return false
  return text.any { char ->
    val dir = Character.getDirectionality(char)
    dir == Character.DIRECTIONALITY_RIGHT_TO_LEFT || dir == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC
  }
}

actual fun ImageResource.toComposeImageBitmap(): ImageBitmap? =
  image.toComposeImageBitmap()
