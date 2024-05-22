package chat.simplex.common.ui.theme

import androidx.compose.runtime.Composable
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import com.jthemedetecor.OsThemeDetector

private val detector: OsThemeDetector = OsThemeDetector.getDetector()
  .apply {
    registerListener(::reactOnDarkThemeChanges)
  }

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
