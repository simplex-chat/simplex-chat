package chat.simplex.common.ui.theme

import androidx.compose.runtime.Composable
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.desktopPlatform
import com.jthemedetecor.OsThemeDetector

private val detector: OsThemeDetector = OsThemeDetector.getDetector()
  .apply {
    registerListener(::reactOnDarkThemeChanges)
    if (desktopPlatform.isLinux()) Runtime.getRuntime().addShutdownHook(Thread(::stopGsettingsMonitor))
  }

private fun stopGsettingsMonitor() =
  ProcessHandle.current().children()
    .filter { it.info().command().orElse("").contains("gsettings") }
    .forEach { it.destroy() }

// TODO: explore possibility to use
//@Composable
//actual fun isSystemInDarkTheme(): Boolean = androidx.compose.foundation.isSystemInDarkTheme()

@Composable
actual fun isSystemInDarkTheme(): Boolean = try {
  detector.isDark
}
catch (e: Exception) {
  Log.e(TAG, e.stackTraceToString())
  /* On Mac this code can produce exception */
  false
}
