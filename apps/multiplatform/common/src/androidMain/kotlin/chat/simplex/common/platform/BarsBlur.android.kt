package chat.simplex.common.platform

import androidx.compose.ui.graphics.drawscope.DrawScope

// Android blurs the bars with a RenderEffect on the layer itself, so this is never called there.
actual fun DrawScope.drawBarsBlurred(radiusPx: Float, barWidth: Float, barHeight: Float, drawBar: DrawScope.() -> Unit) = drawBar()
