package chat.simplex.common

import androidx.compose.ui.awt.ComposeWindow
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.setMacOSWindowVibrancyAvailable
import org.jetbrains.skiko.SkiaLayer
import java.awt.Component
import java.awt.Container
import java.awt.SystemColor

private external fun macOSConfigureWindow(windowHandle: Long): Boolean

fun configureMacOSWindow(window: ComposeWindow) {
  val skiaLayers = findSkiaLayers(window)
  try {
    skiaLayers.forEach { it.transparency = true }
    val configured = macOSConfigureWindow(window.windowHandle)
    setMacOSWindowVibrancyAvailable(configured)
    if (!configured) {
      useOpaqueFallback(window, skiaLayers)
      Log.w(TAG, "macOS window vibrancy was unavailable; using the opaque fallback")
    }
  } catch (e: Throwable) {
    setMacOSWindowVibrancyAvailable(false)
    useOpaqueFallback(window, skiaLayers)
    Log.w(TAG, "Unable to configure macOS window vibrancy: ${e.message}")
  }
}

private fun useOpaqueFallback(window: ComposeWindow, skiaLayers: List<SkiaLayer>) {
  skiaLayers.forEach { it.transparency = false }
  window.background = SystemColor.window
}

private fun findSkiaLayers(component: Component): List<SkiaLayer> {
  val own = if (component is SkiaLayer) listOf(component) else emptyList()
  val children = if (component is Container) component.components.flatMap(::findSkiaLayers) else emptyList()
  return own + children
}
