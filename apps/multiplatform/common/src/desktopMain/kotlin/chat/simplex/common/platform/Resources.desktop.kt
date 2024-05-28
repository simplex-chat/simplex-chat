package chat.simplex.common.platform

import SectionItemView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toComposeImageBitmap
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.connectViaUri
import chat.simplex.common.views.newchat.openKnownGroup
import chat.simplex.res.MR
import com.jthemedetecor.OsThemeDetector
import com.russhwolf.settings.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.stringResource
import dev.icerock.moko.resources.desc.desc
import java.io.File
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
    .also { it.parentFile.mkdirs() }
private val settingsThemesFile =
  File(desktopPlatform.configPath + File.separator + "themes.properties")
    .also { it.parentFile.mkdirs() }

private var readOnlySettings = false
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
        // Making backup just in case
        try {
          settingsFile.copyTo(File(settingsFile.absolutePath + ".bak"), overwrite = false)
        } catch (e: Exception) {
          if (e !is FileAlreadyExistsException) {
            Log.e(TAG, "Error making a backup of settings file: ${e.stackTraceToString()}")
          }
        }
        readOnlySettings = true
        Log.e(TAG, "Error reading settings file: ${e.stackTraceToString()}")
        AlertManager.shared.showAlertDialogButtonsColumn(
          title = generalGetString(MR.strings.error_reading_settings_title),
          text = generalGetString(MR.strings.error_reading_settings_desc).format(settingsFile.absolutePath, e.stackTraceToString()),
          buttons = {
            Column {
              if (appPlatform.isDesktop) {
                // Open directory
                SectionItemView({
                  desktopOpenDir(settingsFile.parentFile)
                }) {
                  Text(generalGetString(MR.strings.error_reading_settings_open_directory), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                }
              }
              // Store settings in memory-only
              SectionItemView({
                AlertManager.shared.hideAlert()
              }) {
                Text(
                  generalGetString(MR.strings.error_reading_settings_store_in_memory),
                  Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary
                )
              }
              // Overwrite
              SectionItemView({
                AlertManager.shared.hideAlert()
                readOnlySettings = false
              }) {
                Text(stringResource(MR.strings.error_reading_settings_overwrite), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.error)
              }
            }
          },
        )
        Properties()
      }
    }
private val settingsThemesProps =
  Properties()
    .also { props -> try { settingsThemesFile.reader().use { props.load(it) } } catch (e: Exception) { Properties() } }

actual val settings: Settings = PropertiesSettings(settingsProps) { withApi { if (!readOnlySettings) settingsFile.writer().use { settingsProps.store(it, "") } } }
actual val settingsThemes: Settings = PropertiesSettings(settingsThemesProps) { withApi { settingsThemesFile.writer().use { settingsThemesProps.store(it, "") } } }

actual fun windowOrientation(): WindowOrientation =
  if (simplexWindowState.windowState.size.width > simplexWindowState.windowState.size.height) {
    WindowOrientation.LANDSCAPE
  } else {
    WindowOrientation.PORTRAIT
  }

@Composable
actual fun windowWidth(): Dp = simplexWindowState.windowState.size.width

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
