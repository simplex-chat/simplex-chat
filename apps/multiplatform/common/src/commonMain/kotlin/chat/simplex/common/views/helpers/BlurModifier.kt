package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.runtime.State
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.clipRect
import androidx.compose.ui.graphics.drawscope.translate
import androidx.compose.ui.graphics.layer.GraphicsLayer
import androidx.compose.ui.graphics.layer.drawLayer
import androidx.compose.ui.unit.*
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.drawBarsBlurred
import chat.simplex.common.ui.theme.CurrentColors

fun Modifier.blurredBackgroundModifier(
  keyboardInset: WindowInsets,
  handler: AppBarHandler?,
  blurRadius: State<Int>,
  prefAlpha: State<Float>,
  keyboardCoversBar: Boolean,
  onTop: Boolean,
  density: Density
): Modifier {
  val graphicsLayer = handler?.graphicsLayer
  val backgroundGraphicsLayer = handler?.backgroundGraphicsLayer
  val backgroundGraphicsLayerSize = handler?.backgroundGraphicsLayerSize
  if (handler == null || graphicsLayer == null || backgroundGraphicsLayer == null || blurRadius.value == 0 || prefAlpha.value == 1f || backgroundGraphicsLayerSize === null)
    return this

  return if (appPlatform.isAndroid) {
    this.androidBlurredModifier(keyboardInset, blurRadius.value, keyboardCoversBar, onTop, graphicsLayer, backgroundGraphicsLayer, backgroundGraphicsLayerSize, density)
  } else {
    this.desktopBlurredModifier(keyboardInset, blurRadius, keyboardCoversBar, onTop, handler, graphicsLayer, backgroundGraphicsLayer, backgroundGraphicsLayerSize, density)
  }
}

// this is more performant version than for Android but can't be used on desktop because on first frame it shows transparent view
// which is very noticeable on desktop and unnoticeable on Android
private fun Modifier.androidBlurredModifier(
  keyboardInset: WindowInsets,
  blurRadius: Int,
  keyboardCoversBar: Boolean,
  onTop: Boolean,
  graphicsLayer: GraphicsLayer,
  backgroundGraphicsLayer: GraphicsLayer,
  backgroundGraphicsLayerSize: State<IntSize>,
  density: Density
): Modifier = this
  .graphicsLayer {
    renderEffect = if (blurRadius > 0) BlurEffect(blurRadius.dp.toPx(), blurRadius.dp.toPx()) else null
    clip = blurRadius > 0
  }
  .graphicsLayer {
    if (!onTop) {
      val bgSize = when {
        backgroundGraphicsLayerSize.value.height == 0 && backgroundGraphicsLayer.size.height != 0 -> backgroundGraphicsLayer.size.height
        backgroundGraphicsLayerSize.value.height == 0 -> graphicsLayer.size.height
        else -> backgroundGraphicsLayerSize.value.height
      }
      val keyboardHeightCovered = if (!keyboardCoversBar) keyboardInset.getBottom(density) else 0
      translationY = -bgSize + size.height + keyboardHeightCovered
    }
  }
  .drawBehind {
    drawRect(CurrentColors.value.colors.background)
    if (onTop) {
      clipRect {
        if (backgroundGraphicsLayer.size != IntSize.Zero) {
          drawLayer(backgroundGraphicsLayer)
        } else {
          drawRect(CurrentColors.value.colors.background, size = Size(graphicsLayer.size.width.toFloat(), graphicsLayer.size.height.toFloat()))
        }
        drawLayer(graphicsLayer)
      }
    } else {
      if (backgroundGraphicsLayer.size != IntSize.Zero) {
        drawLayer(backgroundGraphicsLayer)
      } else {
        drawRect(CurrentColors.value.colors.background, size = Size(graphicsLayer.size.width.toFloat(), graphicsLayer.size.height.toFloat()))
      }
      drawLayer(graphicsLayer)
    }
  }
  .graphicsLayer {
    if (!onTop) {
      val bgSize = when {
        backgroundGraphicsLayerSize.value.height == 0 && backgroundGraphicsLayer.size.height != 0 -> backgroundGraphicsLayer.size.height
        backgroundGraphicsLayerSize.value.height == 0 -> graphicsLayer.size.height
        else -> backgroundGraphicsLayerSize.value.height
      }
      val keyboardHeightCovered = if (!keyboardCoversBar) keyboardInset.getBottom(density) else 0
      translationY -= -bgSize + size.height + keyboardHeightCovered
    }
  }

private fun Modifier.desktopBlurredModifier(
  keyboardInset: WindowInsets,
  blurRadius: State<Int>,
  keyboardCoversBar: Boolean,
  onTop: Boolean,
  handler: AppBarHandler,
  graphicsLayer: GraphicsLayer,
  backgroundGraphicsLayer: GraphicsLayer,
  backgroundGraphicsLayerSize: State<IntSize>,
  density: Density
): Modifier = this
  .drawBehind {
    val barWidth = size.width
    val barHeight = size.height
    // The blur is taken from a copy of the scrolled content rather than drawn from that layer, so unlike a layer's own
    // filter it does not follow the content on its own: the bar is redrawn when the container reports the copy moved.
    handler.contentVersion.value
    drawBarsBlurred(blurRadius.value.dp.toPx(), barWidth, barHeight) {
      drawRect(CurrentColors.value.colors.background, size = Size(barWidth, barHeight))
      if (onTop) {
        clipRect(0f, 0f, barWidth, barHeight) {
          if (backgroundGraphicsLayer.size != IntSize.Zero) {
            drawLayer(backgroundGraphicsLayer)
          } else {
            drawRect(CurrentColors.value.colors.background, size = Size(graphicsLayer.size.width.toFloat(), graphicsLayer.size.height.toFloat()))
          }
          drawLayer(graphicsLayer)
        }
      } else {
        val bgSize = when {
          backgroundGraphicsLayerSize.value.height == 0 && backgroundGraphicsLayer.size.height != 0 -> backgroundGraphicsLayer.size.height
          backgroundGraphicsLayerSize.value.height == 0 -> graphicsLayer.size.height
          else -> backgroundGraphicsLayerSize.value.height
        }
        val keyboardHeightCovered = if (!keyboardCoversBar) keyboardInset.getBottom(density) else 0
        translate(top = -bgSize + barHeight + keyboardHeightCovered) {
          if (backgroundGraphicsLayer.size != IntSize.Zero) {
            drawLayer(backgroundGraphicsLayer)
          } else {
            drawRect(CurrentColors.value.colors.background, size = Size(graphicsLayer.size.width.toFloat(), graphicsLayer.size.height.toFloat()))
          }
          drawLayer(graphicsLayer)
        }
      }
    }
  }
