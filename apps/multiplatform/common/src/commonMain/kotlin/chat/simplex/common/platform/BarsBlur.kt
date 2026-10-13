package chat.simplex.common.platform

import androidx.compose.ui.graphics.drawscope.DrawScope

// Draws the bars' background blurred. The drawing to blur is given in bar coordinates, whatever size the blur runs at.
expect fun DrawScope.drawBarsBlurred(radiusPx: Float, barWidth: Float, barHeight: Float, drawBar: DrawScope.() -> Unit)
