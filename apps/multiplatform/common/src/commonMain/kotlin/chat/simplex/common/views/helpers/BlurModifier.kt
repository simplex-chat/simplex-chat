package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.runtime.Composable
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
import chat.simplex.common.ui.theme.CurrentColors

@Composable
fun Modifier.blurredBackgroundModifier(
  keyboardInset: WindowInsets,
  handler: AppBarHandler?,
  blurRadius: State<Int>,
  prefAlpha: Float,
  keyboardCoversBar: Boolean,
  onTop: Boolean,
  density: Density
): Modifier {
  val graphicsLayer = handler?.graphicsLayer
  val backgroundGraphicsLayer = handler?.backgroundGraphicsLayer
  val backgroundGraphicsLayerSize = handler?.backgroundGraphicsLayerSize
  if (!(handler != null && graphicsLayer != null && backgroundGraphicsLayer != null && blurRadius.value > 0 && prefAlpha < 1f && backgroundGraphicsLayerSize != null))
    return this

  return if (appPlatform.isAndroid) {
    this.androidBlurredModifier(keyboardInset, blurRadius, keyboardCoversBar, onTop, graphicsLayer, backgroundGraphicsLayer, backgroundGraphicsLayerSize, density)
  } else {
    this.desktopBlurredModifier(keyboardInset, blurRadius, keyboardCoversBar, onTop, graphicsLayer, backgroundGraphicsLayer, backgroundGraphicsLayerSize, density)
  }
}

@Composable
fun Modifier.androidBlurredModifier(
  keyboardInset: WindowInsets,
  blurRadius: State<Int>,
  keyboardCoversBar: Boolean,
  onTop: Boolean,
  graphicsLayer: GraphicsLayer,
  backgroundGraphicsLayer: GraphicsLayer,
  backgroundGraphicsLayerSize: State<IntSize>,
  density: Density
): Modifier = this
  .graphicsLayer {
    renderEffect = if (blurRadius.value > 0) BlurEffect(blurRadius.value.dp.toPx(), blurRadius.value.dp.toPx()) else null
    clip = blurRadius.value > 0
  }
  .graphicsLayer {
    if (!onTop) {
      val bgSize = when {
        backgroundGraphicsLayerSize.value.height == 0 && backgroundGraphicsLayer.size.height != 0 -> backgroundGraphicsLayer.size.height
        backgroundGraphicsLayerSize.value.height == 0 -> graphicsLayer.size.height
        else -> backgroundGraphicsLayerSize.value.height
      }
      println("LALAL GRAPHICSLAYER ${graphicsLayer.size.height} ${backgroundGraphicsLayer.size.height} ${backgroundGraphicsLayerSize.value.height}")
      val keyboardHeightCovered = if (!keyboardCoversBar) keyboardInset.getBottom(density) else 0
      translationY = -bgSize + size.height + keyboardHeightCovered
    }
  }
  .drawBehind {
    drawRect(Color.Black)
    if (onTop) {
      println("LALAL DRAWBEHIND0")
      clipRect {
        if (backgroundGraphicsLayer.size != IntSize.Zero) {
          drawLayer(backgroundGraphicsLayer)
        } else {
          drawRect(CurrentColors.value.colors.background, size = Size(graphicsLayer.size.width.toFloat(), graphicsLayer.size.height.toFloat()))
        }
        drawLayer(graphicsLayer)
      }
    } else {
      println("LALAL DRAWBEHIND1  ${graphicsLayer.size.height}  ${backgroundGraphicsLayer.size.height} ${backgroundGraphicsLayerSize.value}")
      if (backgroundGraphicsLayer.size != IntSize.Zero) {
        drawLayer(backgroundGraphicsLayer)
      } else {
        drawRect(CurrentColors.value.colors.background, size = Size(graphicsLayer.size.width.toFloat(), graphicsLayer.size.height.toFloat()))
      }
      drawLayer(graphicsLayer)
    }
  }

@Composable
fun Modifier.desktopBlurredModifier(
  keyboardInset: WindowInsets,
  blurRadius: State<Int>,
  keyboardCoversBar: Boolean,
  onTop: Boolean,
  graphicsLayer: GraphicsLayer,
  backgroundGraphicsLayer: GraphicsLayer,
  backgroundGraphicsLayerSize: State<IntSize>,
  density: Density
): Modifier = this
  .graphicsLayer {
    renderEffect = if (blurRadius.value > 0) BlurEffect(blurRadius.value.dp.toPx(), blurRadius.value.dp.toPx()) else null
    clip = blurRadius.value > 0
  }
//  .graphicsLayer {
//    if (!onTop) {
//      val bgSize = when {
//        backgroundGraphicsLayerSize.value.height == 0 && backgroundGraphicsLayer.size.height != 0 -> backgroundGraphicsLayer.size.height
//        backgroundGraphicsLayerSize.value.height == 0 -> graphicsLayer.size.height
//        else -> backgroundGraphicsLayerSize.value.height
//      }
//      println("LALAL GRAPHICSLAYER ${graphicsLayer.size.height} ${backgroundGraphicsLayer.size.height} ${backgroundGraphicsLayerSize.value.height}")
//      val keyboardHeightCovered = if (keyboardCoversBar) keyboardInset.getBottom(density) else 0
//      translationY = -bgSize + size.height + keyboardHeightCovered
//    }
//  }
  .drawBehind {
    drawRect(Color.Black)
    if (onTop) {
      println("LALAL DRAWBEHIND0")
      clipRect {
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
      println("LALAL DRAWBEHIND1  ${graphicsLayer.size.height}  ${backgroundGraphicsLayer.size.height} ${backgroundGraphicsLayerSize.value}")
      val keyboardHeightCovered = if (!keyboardCoversBar) keyboardInset.getBottom(density) else 0
      translate(top = -bgSize + size.height + keyboardHeightCovered) {
        if (backgroundGraphicsLayer.size != IntSize.Zero) {
          drawLayer(backgroundGraphicsLayer)
        } else {
          drawRect(CurrentColors.value.colors.background, size = Size(graphicsLayer.size.width.toFloat(), graphicsLayer.size.height.toFloat()))
        }
        drawLayer(graphicsLayer)
      }
    }
  }
