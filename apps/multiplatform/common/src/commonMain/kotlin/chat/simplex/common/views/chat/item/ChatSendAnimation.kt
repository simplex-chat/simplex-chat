package chat.simplex.common.views.chat.item

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.CubicBezierEasing
import androidx.compose.animation.core.LinearEasing
import androidx.compose.animation.core.tween
import androidx.compose.foundation.layout.Box
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.MotionDurationScale
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.graphics.TransformOrigin
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.layout.LayoutCoordinates
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInWindow
import androidx.compose.ui.unit.toSize
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlin.time.TimeSource

val LocalChatSendAnimations = staticCompositionLocalOf<ChatSendAnimations?> { null }

// Spec: spec/client/chat-view.md#outgoing-message-animation
// A mounted chat owns this controller. An API response holding an older controller
// cannot animate a message in a newly opened chat, even if its item ID matches.
class ChatSendAnimations(private val scope: CoroutineScope) {
  private var active = true
  var composerBounds: Rect? = null
  var viewportBounds: Rect? = null
  val transitions = mutableStateMapOf<Long, ChatSendTransition>()

  fun capture(): PendingChatSend? {
    val source = composerBounds?.takeUnless { it.isEmpty } ?: return null
    return if (active) PendingChatSend(this, source) else null
  }

  internal fun register(itemId: Long, pending: PendingChatSend) {
    // A delayed API response should appear normally, not jump from stale input.
    if (!active || pending.created.elapsedNow().inWholeMilliseconds > 2_000) return
    val transition = ChatSendTransition(pending.source)
    transitions[itemId] = transition
    scope.launch {
      delay(1_000)
      if (!transition.running) finish(itemId, transition) // Rows hidden by a filter/search.
    }
  }

  fun finish(itemId: Long, transition: ChatSendTransition) {
    if (transitions[itemId] === transition) transitions.remove(itemId)
  }

  fun dispose() {
    active = false
    composerBounds = null
    transitions.clear()
  }
}

class PendingChatSend internal constructor(
  private val controller: ChatSendAnimations,
  val source: Rect,
) {
  internal val created = TimeSource.Monotonic.markNow()
  fun register(itemId: Long) = controller.register(itemId, this)
}

class ChatSendTransition(val source: Rect) {
  internal var running = false
}

internal fun LayoutCoordinates.chatAnimationBounds(): Rect = Rect(positionInWindow(), size.toSize())

private val sendVerticalEasing = CubicBezierEasing(0.19919473f, 0.010644531f, 0.27920938f, 0.9102539f)
private val sendHorizontalQuint = CubicBezierEasing(0.23f, 1f, 0.32f, 1f)
private val sendHorizontalOut = CubicBezierEasing(0f, 0f, 0.58f, 1f)

@Composable
fun ChatSendAnimatedItem(itemId: Long, content: @Composable (Modifier) -> Unit) {
  val controller = LocalChatSendAnimations.current
  val transition = controller?.transitions?.get(itemId)
  var destination by remember(itemId) { mutableStateOf<Rect?>(null) }
  val animationModifier = if (controller != null && transition != null) {
    val progress = remember(transition) { Animatable(0f) }
    val motionScale = rememberCoroutineScope().coroutineContext[MotionDurationScale]
    LaunchedEffect(transition) {
      transition.running = true
      try {
        if (motionScale?.scaleFactor == 0f) return@LaunchedEffect
        withFrameNanos { }
        val target = destination
        val viewport = controller.viewportBounds
        if (target != null && !target.isEmpty && viewport != null && target.overlaps(viewport) && target.height < viewport.height) {
          // Animatable uses the composition's MotionDurationScale, including zero.
          progress.animateTo(1f, tween(250, easing = LinearEasing))
        }
      } finally {
        controller.finish(itemId, transition)
      }
    }
    DisposableEffect(transition) {
      onDispose { controller.finish(itemId, transition) }
    }
    Modifier.graphicsLayer {
      translationX = 0f
      translationY = 0f
      scaleX = 1f
      scaleY = 1f
      alpha = 1f
      val target = destination ?: return@graphicsLayer
      val viewport = controller.viewportBounds ?: return@graphicsLayer
      if (motionScale?.scaleFactor == 0f || target.isEmpty || !target.overlaps(viewport) || target.height >= viewport.height) return@graphicsLayer
      val t = progress.value
      val vertical = sendVerticalEasing.transform(t)
      val horizontal = sendHorizontalOut.transform(sendHorizontalQuint.transform(t))
      // Re-read the destination during scrolling and when the composer shrinks.
      translationX = (transition.source.left - target.left) * (1f - horizontal)
      translationY = (transition.source.bottom - target.bottom) * (1f - vertical)
      transformOrigin = TransformOrigin(0f, 1f)
      scaleX = 0.96f + 0.04f * vertical
      scaleY = scaleX
      alpha = (t / 0.4f).coerceIn(0f, 1f)
    }
  } else Modifier

  // This must be a separate layout node: measuring the transformed content itself
  // feeds its current translation back into the next scroll/layout calculation.
  // Keep the wrapper/content call site stable when the transition finishes.
  Box(if (transition == null) Modifier else Modifier.onGloballyPositioned { destination = it.chatAnimationBounds() }) {
    content(animationModifier)
  }
}