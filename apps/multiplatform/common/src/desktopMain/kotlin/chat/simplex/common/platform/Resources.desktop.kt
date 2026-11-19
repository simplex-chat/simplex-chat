package chat.simplex.common.platform

import androidx.compose.runtime.*
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.toComposeImageBitmap
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.LineHeightStyle
import androidx.compose.ui.unit.*
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.theme.Inter
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import com.jthemedetecor.OsThemeDetector
import com.russhwolf.settings.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.desc.desc
import org.jetbrains.skia.FontMgr
import org.jetbrains.skia.Typeface
import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.*
import kotlin.math.roundToInt
import org.jetbrains.skia.Font as SkFont

@Composable
actual fun font(name: String, res: String, weight: FontWeight, style: FontStyle): Font =
  androidx.compose.ui.text.platform.Font("MR/fonts/$res.ttf", weight, style)

actual fun StringResource.localized(): String = desc().toString()

internal val systemThemeDetector: OsThemeDetector? = try {
  OsThemeDetector.getDetector()
} catch (_: LinkageError) {
  null
}
actual fun isInNightMode() = try {
  systemThemeDetector?.isDark ?: false
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

@Composable
actual fun desktopLineHeight(fontSize: TextUnit, lineHeight: TextUnit, fontFamily: FontFamily?, lineHeightStyle: LineHeightStyle?): TextUnit {
  val typeface = when (fontFamily) {
    null, Inter -> interTypeface
    FontFamily.Monospace -> monospaceTypeface
    FontFamily.Default, FontFamily.SansSerif -> sansSerifTypeface
    else -> null
  }
  if (!fontSize.isSp || !lineHeight.isSp || typeface == null) return lineHeight
  val density = LocalDensity.current
  val lineHeightPx = with(density) { lineHeight.toPx() }
  val metrics = SkFont(typeface, with(density) { fontSize.toPx() }).use { it.metrics }
  val fontHeight = metrics.descent - metrics.ascent + metrics.leading
  val roundedFontHeight = fontHeight.roundToInt()
  if (roundedFontHeight >= fontHeight) return lineHeight
  val topRatio = when ((lineHeightStyle ?: LineHeightStyle.Default).alignment) {
    LineHeightStyle.Alignment.Top -> 0f
    LineHeightStyle.Alignment.Center -> 0.5f
    LineHeightStyle.Alignment.Bottom -> 1f
    else -> -1f
  }
  val adjustedPx = if (topRatio >= 0f) {
    lineHeightPx - fontHeight + roundedFontHeight
  } else {
    (lineHeightPx - metrics.leading) * roundedFontHeight / (metrics.descent - metrics.ascent)
  }
  return with(density) { adjustedPx.toSp() }
}

private val interTypeface: Typeface? by lazy { FontMgr.default.makeFromFile(MR.fonts.inter_regular.file.absolutePath) }

private val monospaceTypeface: Typeface? by lazy {
  firstInstalledTypeface(
    linux = listOf("Noto Sans Mono", "DejaVu Sans Mono", "Consolas"),
    windows = listOf("Consolas"),
    mac = listOf(".AppleSystemUIFontMonospaced", "Menlo", "Courier")
  )
}

private val sansSerifTypeface: Typeface? by lazy {
  firstInstalledTypeface(
    linux = listOf("Noto Sans", "DejaVu Sans", "Arial"),
    windows = listOf("Segoe UI", "Arial"),
    mac = listOf(".AppleSystemUIFont", "Helvetica Neue", "Helvetica")
  )
}

private fun firstInstalledTypeface(linux: List<String>, windows: List<String>, mac: List<String>): Typeface? {
  val families = when {
    desktopPlatform.isLinux() -> linux
    desktopPlatform.isWindows() -> windows
    else -> mac
  }
  return families.firstNotNullOfOrNull { family ->
    FontMgr.default.matchFamily(family).use { set -> if (set.count() > 0) set.matchStyle(org.jetbrains.skia.FontStyle.NORMAL) else null }
  }
}
