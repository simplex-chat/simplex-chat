package chat.simplex.common.ui.theme

import androidx.compose.foundation.background
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.composed
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.layout.positionInWindow
import androidx.compose.ui.unit.IntSize
import chat.simplex.common.views.helpers.PresetWallpaper
import chat.simplex.common.views.helpers.SimplexStops
import chat.simplex.common.views.helpers.WallpaperType
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin

// SIMPLEX 20°-tilted "sunrise" axis spans the whole chat surface (top app bar to compose bar).
// Bubble backgrounds and secondary text sample stops along this same axis so colour rises
// from bottom-left dark to top-right warm in one coordinated motion across the screen.

private const val SIMPLEX_TILT_DEG = 20f

// Per-wallpaper gradient stops live in PresetWallpaper._simplexStops (ChatWallpaper.kt).
val LocalSimplexLinkColor = compositionLocalOf<Color?> { null }

class ChatViewportInfo(
  val sizePx: State<IntSize>,
  val originInWindow: State<Offset>,
)

val LocalChatViewportInfo = compositionLocalOf<ChatViewportInfo?> { null }

@Composable
fun rememberChatViewportInfo(): Pair<ChatViewportInfo, Modifier> {
  val sizePx = remember { mutableStateOf(IntSize.Zero) }
  val originInWindow = remember { mutableStateOf(Offset.Zero) }
  val info = remember { ChatViewportInfo(sizePx, originInWindow) }
  val mod = Modifier
    .onSizeChanged { sizePx.value = it }
    .onGloballyPositioned { originInWindow.value = it.positionInWindow() }
  return info to mod
}

private fun axisEndpoints(winW: Float, winH: Float): Pair<Offset, Offset> {
  val theta = SIMPLEX_TILT_DEG * PI.toFloat() / 180f
  val dx = sin(theta); val dy = -cos(theta)
  val cx = winW / 2f; val cy = winH / 2f
  val projs = floatArrayOf(
    (-cx) * dx + (-cy) * dy,
    (winW - cx) * dx + (-cy) * dy,
    (-cx) * dx + (winH - cy) * dy,
    (winW - cx) * dx + (winH - cy) * dy,
  )
  var minP = projs[0]; var maxP = projs[0]
  for (p in projs) { if (p < minP) minP = p; if (p > maxP) maxP = p }
  val start = Offset(cx + dx * minP, cy + dy * minP)
  val end = Offset(cx + dx * maxP, cy + dy * maxP)
  return start to end
}

enum class SimplexBubbleSlot { Sent, SentQuote, Received, ReceivedQuote }

/** SimplexStops for a wallpaper, or null when it has none. */
private fun stopsForWallpaper(wallpaper: AppWallpaper): SimplexStops? {
  val type = wallpaper.type as? WallpaperType.Preset ?: return null
  return PresetWallpaper.from(type.filename)?.simplexStops
}

// The active theme's base is always the global one — ThemeManager.currentColors never overrides
// the base per chat; only wallpaper and colours vary per chat/preview. So the SIMPLEX check stays
// global, while the stops/tints read wallpaper + colours from composition (per-chat/preview aware).
private fun isSimplexActive(): Boolean = CurrentColors.value.base == DefaultTheme.SIMPLEX
@Composable private fun activeSimplexStops(): SimplexStops? = stopsForWallpaper(MaterialTheme.wallpaper)

@Composable private fun stopsFor(slot: SimplexBubbleSlot): Array<Pair<Float, Color>>? {
  val stops = activeSimplexStops() ?: return null
  return when (slot) {
    SimplexBubbleSlot.Sent -> stops.sent
    SimplexBubbleSlot.SentQuote -> stops.sentQuote
    SimplexBubbleSlot.Received -> stops.received
    SimplexBubbleSlot.ReceivedQuote -> stops.receivedQuote
  }
}

// Paint a bubble's background with the tilted gradient anchored to the chat viewport.
// Brush start/end live in viewport coordinates; we translate them into bubble-local
// coordinates so the gradient continues across bubbles as a single coherent axis.
fun Modifier.simplexBubbleBackground(slot: SimplexBubbleSlot): Modifier = composed {
  val viewport = LocalChatViewportInfo.current ?: return@composed this
  val bubblePos = remember { mutableStateOf(Offset.Zero) }
  val stops = stopsFor(slot) ?: return@composed this
  this
    .onGloballyPositioned { bubblePos.value = it.positionInWindow() }
    .drawBehind {
      val sz = viewport.sizePx.value
      if (sz.width == 0 || sz.height == 0) return@drawBehind
      val (axisStartLocal, axisEndLocal) = axisEndpoints(sz.width.toFloat(), sz.height.toFloat())
      val viewportOrigin = viewport.originInWindow.value
      val offset = viewportOrigin - bubblePos.value
      val brush = Brush.linearGradient(
        colorStops = stops,
        start = axisStartLocal + offset,
        end = axisEndLocal + offset,
      )
      drawRect(brush)
    }
}

// Single entry point for chat-bubble backgrounds: routes all four slots to SIMPLEX
// gradients when SIMPLEX is active and viewport info is available, else flat AppColors.
fun Modifier.chatBubbleBackground(
  sent: Boolean,
  isQuote: Boolean,
  transparent: Boolean = false,
): Modifier = composed {
  if (transparent) return@composed this.background(Color.Transparent)
  val simplexActive = isSimplexActive()
  val viewportAvailable = LocalChatViewportInfo.current != null
  val stops = activeSimplexStops()
  if (simplexActive && viewportAvailable && stops != null) {
    val slot = when {
      sent && isQuote -> SimplexBubbleSlot.SentQuote
      sent -> SimplexBubbleSlot.Sent
      isQuote -> SimplexBubbleSlot.ReceivedQuote
      else -> SimplexBubbleSlot.Received
    }
    return@composed this.simplexBubbleBackground(slot)
  }
  this.background(
    when {
      sent && isQuote -> MaterialTheme.appColors.sentQuote
      sent -> MaterialTheme.appColors.sentMessage
      isQuote -> MaterialTheme.appColors.receivedQuote
      else -> MaterialTheme.appColors.receivedMessage
    }
  )
}

// Static tinted-transparency for small text and default avatars on the SIMPLEX gradient. A
// semi-transparent warm colour blends with the gradient at every height, so it stays uniformly
// legible without sampling the axis by screen position (only bubbles still sample it).
// Composition-scoped: honours the per-chat override / wallpaper preview, and its fallback is
// the composition secondary, so non-SIMPLEX chats keep their own (override-aware) colour.
@Composable
fun simplexSecondaryTint(): Color {
  if (!isSimplexActive()) return MaterialTheme.colors.secondary
  return activeSimplexStops()?.secondaryTint ?: MaterialTheme.colors.secondary
}

@Composable
fun simplexAuthorTint(): Color {
  if (!isSimplexActive()) return MaterialTheme.colors.onBackground
  return activeSimplexStops()?.authorTint ?: MaterialTheme.colors.onBackground
}
