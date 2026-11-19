package chat.simplex.common.platform

import dev.nucleusframework.darkmodedetector.IDarkModeDetector
import dev.nucleusframework.darkmodedetector.getPlatformDarkModeDetector
import kotlinx.coroutines.Dispatchers
import java.util.function.Consumer
import kotlin.coroutines.EmptyCoroutineContext

private const val DARK_THEME_MARKER = "dark"

internal fun isDarkThemeName(name: String): Boolean = name.contains(DARK_THEME_MARKER, ignoreCase = true)

internal object LinuxTheme {
  private val portal: IDarkModeDetector? = try {
    getPlatformDarkModeDetector()
  } catch (e: LinkageError) {
    Log.e(TAG, "Desktop portal theme is unavailable: ${e.message}")
    null
  }

  private val gtkThemeIsDark: Boolean by lazy {
    LinuxNative.isLoaded && LinuxNative.nativeGtkThemeName()?.let(::isDarkThemeName) == true
  }

  val isDark: Boolean
    get() = portal?.isDark() == true || gtkThemeIsDark

  fun observe(onChange: (Boolean) -> Unit) {
    portal?.registerListener(Consumer { Dispatchers.Main.dispatch(EmptyCoroutineContext) { onChange(isDark) } })
  }
}
