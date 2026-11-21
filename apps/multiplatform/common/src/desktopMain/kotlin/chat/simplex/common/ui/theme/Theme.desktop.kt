package chat.simplex.common.ui.theme

import androidx.compose.runtime.Composable
import chat.simplex.common.platform.LinuxTheme
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.common.platform.systemThemeDetector
import com.jthemedetecor.OsThemeDetector

private val detector: OsThemeDetector? = systemThemeDetector
  ?.apply { registerListener(::reactOnDarkThemeChanges) }

private val linuxTheme: LinuxTheme? =
  if (desktopPlatform.isLinux()) LinuxTheme.apply { observe(::reactOnDarkThemeChanges) } else null

// TODO: explore possibility to use
//@Composable
//actual fun isSystemInDarkTheme(): Boolean = androidx.compose.foundation.isSystemInDarkTheme()

@Composable
actual fun isSystemInDarkTheme(): Boolean = linuxTheme?.isDark ?: try {
  detector?.isDark ?: false
}
catch (e: Exception) {
  Log.e(TAG, e.stackTraceToString())
  /* On Mac this code can produce exception */
  false
}
