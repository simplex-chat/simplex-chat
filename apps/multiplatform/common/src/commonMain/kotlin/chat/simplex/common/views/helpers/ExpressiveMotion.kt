package chat.simplex.common.views.helpers

import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.LocalIndication
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsPressedAsState
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.composed
import androidx.compose.ui.graphics.graphicsLayer
import chat.simplex.common.platform.*

/**
 * Пружинная физика в стиле Material 3 Expressive.
 */
object ExpressiveSprings {
  // Быстрый пружинный отклик для микро-взаимодействий (кнопки, чипы, иконки)
  val SpatialFast = spring<Float>(
    dampingRatio = 0.72f,
    stiffness = 650f
  )

  // Плавная пружина для изменения размеров капсул, карточек и панелей
  val SpatialDefault = spring<Float>(
    dampingRatio = 0.78f,
    stiffness = 420f
  )

  // Выразительный отскок (M3 Expressive Bounce) для реакций, FAB и переключателей
  val Bouncy = spring<Float>(
    dampingRatio = Spring.DampingRatioMediumBouncy,
    stiffness = Spring.StiffnessMediumLow
  )
}

/**
 * Анимирует масштаб элемента при касании (микро-сжатие + пружинный возврат)
 * и запускает RichHaptics.
 */
fun Modifier.expressivePress(
  interactionSource: MutableInteractionSource,
  targetScale: Float = 0.94f,
  hapticOnPress: HapticPattern? = null
): Modifier = composed {
  val isPressed by interactionSource.collectIsPressedAsState()

  LaunchedEffect(isPressed) {
    if (isPressed && hapticOnPress != null) {
      RichHaptics.perform(hapticOnPress)
    }
  }

  val scale by animateFloatAsState(
    targetValue = if (isPressed) targetScale else 1f,
    animationSpec = ExpressiveSprings.SpatialFast
  )

  this.graphicsLayer {
    scaleX = scale
    scaleY = scale
  }
}

/**
 * Готовый кликабельный модификатор M3 Expressive с пружинным сжатием и RichHaptics.
 */
fun Modifier.expressiveClickable(
  haptic: HapticPattern = HapticPattern.CLICK,
  targetScale: Float = 0.94f,
  enabled: Boolean = true,
  onClick: () -> Unit
): Modifier = composed {
  val interactionSource = remember { MutableInteractionSource() }
  this
    .expressivePress(
      interactionSource = interactionSource,
      targetScale = if (enabled) targetScale else 1f,
      hapticOnPress = if (enabled) HapticPattern.TICK else null
    )
    .clickable(
      interactionSource = interactionSource,
      indication = LocalIndication.current,
      enabled = enabled,
      onClick = {
        RichHaptics.perform(haptic)
        onClick()
      }
    )
}

/**
 * Плавный морфинг размеров капсул (поле ввода, превью вложений, пузыри сообщений).
 */
fun Modifier.expressiveContainerSize(): Modifier =
  this.animateContentSize(
    animationSpec = spring(
      dampingRatio = 0.78f,
      stiffness = 420f
    )
  )

/**
 * Пружинное появление и скрытие плавающих кнопок (FAB) и бейджей.
 */
@Composable
fun ExpressiveFabVisibility(
  visible: Boolean,
  modifier: Modifier = Modifier,
  content: @Composable AnimatedVisibilityScope.() -> Unit
) {
  AnimatedVisibility(
    visible = visible,
    modifier = modifier,
    enter = scaleIn(
      initialScale = 0.72f,
      animationSpec = spring(dampingRatio = 0.68f, stiffness = 480f)
    ) + fadeIn(animationSpec = tween(180)),
    exit = scaleOut(
      targetScale = 0.72f,
      animationSpec = spring(dampingRatio = 0.85f, stiffness = 550f)
    ) + fadeOut(animationSpec = tween(140)),
    content = content
  )
}
