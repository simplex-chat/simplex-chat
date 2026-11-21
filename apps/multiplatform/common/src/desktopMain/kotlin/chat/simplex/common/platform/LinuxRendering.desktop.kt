package chat.simplex.common.platform

import chat.simplex.common.model.AppPreferences
import chat.simplex.common.model.DEFAULT_BARS_BLUR_RADIUS

private val SOFTWARE_RENDERERS = listOf("llvmpipe", "softpipe", "swrast")
private const val SOFTWARE_BARS_BLUR_RADIUS = 0
private const val SOFTWARE_BARS_ALPHA = 1f

internal fun isSoftwareRenderer(renderer: String): Boolean = SOFTWARE_RENDERERS.any { renderer.contains(it, ignoreCase = true) }

internal fun applySoftwareRenderingDefaults(prefs: AppPreferences) {
  val defaultBlur = if (prefs.deviceSupportsBlur) DEFAULT_BARS_BLUR_RADIUS else 0
  if (prefs.appearanceBarsBlurRadius.get() != defaultBlur || prefs.inAppBarsAlpha.get() != prefs.inAppBarsDefaultAlpha) return
  prefs.appearanceBarsBlurRadius.set(SOFTWARE_BARS_BLUR_RADIUS)
  prefs.inAppBarsAlpha.set(SOFTWARE_BARS_ALPHA)
}

fun adjustToLinuxRenderer(prefs: AppPreferences) {
  if (!LinuxNative.isLoaded) return
  val renderer = LinuxNative.nativeGlRenderer() ?: return
  Log.i(TAG, "OpenGL renderer: $renderer")
  if (isSoftwareRenderer(renderer)) applySoftwareRenderingDefaults(prefs)
}
