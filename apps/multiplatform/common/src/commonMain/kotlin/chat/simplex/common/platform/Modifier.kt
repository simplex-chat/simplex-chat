package chat.simplex.common.platform

import androidx.compose.foundation.clickable
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.BlurredEdgeTreatment
import androidx.compose.ui.draw.blur
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.KeyChangeEffect
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.cancel
import kotlinx.coroutines.flow.filter
import java.io.File

expect fun Modifier.navigationBarsWithImePadding(): Modifier

@Composable
expect fun ProvideWindowInsets(
  consumeWindowInsets: Boolean = true,
  windowInsetsAnimationsEnabled: Boolean = true,
  content: @Composable () -> Unit
)

@Composable
expect fun Modifier.desktopOnExternalDrag(
  enabled: Boolean = true,
  onFiles: (List<File>) -> Unit = {},
  onImage: (Painter) -> Unit = {},
  onText: (String) -> Unit = {}
): Modifier

expect fun Modifier.onRightClick(action: () -> Unit): Modifier

expect fun Modifier.desktopPointerHoverIconHand(): Modifier

@Composable
fun Modifier.privacyBlur(
  enabled: Boolean,
  blurred: MutableState<Boolean> = remember { mutableStateOf(appPrefs.privacyMediaBlurRadius.get() > 0) },
  scrollState: State<Boolean>,
  onLongClick: () -> Unit = {}
): Modifier {
  val blurRadius = remember { appPrefs.privacyMediaBlurRadius.state }
  KeyChangeEffect(blurRadius.value) {
    blurred.value = blurRadius.value > 0
  }
  return if (enabled && blurred.value) {
    this then Modifier.blur(
      radiusX = remember { appPrefs.privacyMediaBlurRadius.state }.value.dp,
      radiusY = remember { appPrefs.privacyMediaBlurRadius.state }.value.dp,
      edgeTreatment = BlurredEdgeTreatment(RoundedCornerShape(0.dp))
    )
      .combinedClickable(
        onLongClick = onLongClick,
        onClick = {
          // Wait until scrolling is finished and only after that hide blur effect. Scroll can happen automatically when user press
          // on the item because in this case Compose will focus the item and scroll to it. If there is no waiting of the end of scrolling,
          // the blur will be shown again in the process of scrolling
          withBGApi {
            snapshotFlow { scrollState.value }
              .filter { !it }
              .collect {
                blurred.value = false
                cancel()
              }
          }
        }
      )
  } else if (enabled && blurRadius.value > 0) {
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
