package chat.simplex.common.platform

import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.BlurredEdgeTreatment
import androidx.compose.ui.draw.blur
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.input.pointer.PointerIcon
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.KeyChangeEffect
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.filter
import java.io.File

@Composable
expect fun Modifier.desktopOnExternalDrag(
  enabled: Boolean = true,
  onFiles: (List<File>) -> Unit = {},
  onImage: (File) -> Unit = {},
  onText: (String) -> Unit = {}
): Modifier

expect fun Modifier.onRightClick(action: () -> Unit): Modifier

expect fun Modifier.desktopPointerHoverIconHand(): Modifier

/**
 * Directly sets the mouse cursor on desktop ([PointerIcon.Hand], [PointerIcon.Text], anything else
 * means default). Compose's pointerHoverIcon displays icons only on Enter/Exit edges or on icon
 * change while marked in-bounds — edges that are silently lost when chat items shift/recompose
 * under the cursor, leaving a stale cursor. Calling this on every hover move (and resetting on
 * exit) keeps it self-correcting. No-op on Android.
 * Verified against Compose 1.8.2, re-verify on upgrade (JetBrains/compose-multiplatform #2091,
 * #1314, #3750). Details: plans/2026-07-13-fix-command-hover-cursor.md
 */
expect fun desktopSetHoverCursor(icon: PointerIcon)

expect fun Modifier.desktopOnHovered(action: (Boolean) -> Unit): Modifier

@Composable
fun Modifier.desktopModifyBlurredState(enabled: Boolean, blurred: MutableState<Boolean>, showMenu: State<Boolean>,): Modifier {
  val blurRadius = remember { appPrefs.privacyMediaBlurRadius.state }
  if (appPlatform.isDesktop) {
    KeyChangeEffect(blurRadius.value) {
      blurred.value = enabled && blurRadius.value > 0
    }
  }
  return if (appPlatform.isDesktop && enabled && blurRadius.value > 0 && !showMenu.value) {
    var job: Job = remember { Job() }
    LaunchedEffect(Unit) {
      // The approach here is to allow menu to show up and to not blur the view. When menu is shown and mouse is hovering,
      // unhovered action is still received, but we don't need to handle it until menu closes. When it closes, it takes one frame to catch a
      // hover action again and if:
      // 1. mouse is still on the view, the hover action will cancel this coroutine and the view will stay unblurred
      // 2. mouse is not on the view, the view will become blurred after 100 ms
      job = launch {
        delay(100)
        blurred.value = true
      }
    }
    this then Modifier.desktopOnHovered { hovered ->
      job.cancel()
      blurred.value = !hovered && !showMenu.value
    }
  } else {
    this
  }
}

@Composable
fun Modifier.privacyBlur(
  enabled: Boolean,
  blurred: MutableState<Boolean> = remember { mutableStateOf(appPrefs.privacyMediaBlurRadius.get() > 0) },
  scrollState: State<Boolean>,
  onLongClick: () -> Unit = {}
): Modifier {
  val blurRadius = remember { appPrefs.privacyMediaBlurRadius.state }
  return if (enabled && blurred.value) {
    this then Modifier.blur(
      radiusX = remember { appPrefs.privacyMediaBlurRadius.state }.value.dp,
      radiusY = remember { appPrefs.privacyMediaBlurRadius.state }.value.dp,
      edgeTreatment = BlurredEdgeTreatment(RoundedCornerShape(0.dp))
    )
      .combinedClickable(
        onLongClick = onLongClick,
        onClick = {
          blurred.value = false
        }
      )
  } else if (enabled && blurRadius.value > 0 && appPlatform.isAndroid) {
      LaunchedEffect(Unit) {
        snapshotFlow { scrollState.value }
          .filter { it }
          .filter { !blurred.value }
          .collect { blurred.value = true }
      }
      this
    } else {
      this
    }
}
