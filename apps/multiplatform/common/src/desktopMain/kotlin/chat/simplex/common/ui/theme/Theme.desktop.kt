package chat.simplex.common.ui.theme

import com.jthemedetecor.OsThemeDetector

private val detector: OsThemeDetector = OsThemeDetector.getDetector()
  .apply {
    registerListener(::reactOnDarkThemeChanges)
  }

actual fun isSystemInDarkTheme(): Boolean = detector.isDark
