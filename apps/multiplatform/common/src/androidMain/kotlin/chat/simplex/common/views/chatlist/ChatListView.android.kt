package chat.simplex.common.views.chatlist

import android.app.Activity
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.platform.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.durationText
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.datetime.Clock

@Composable
actual fun ActiveCallInteractiveArea(call: Call, newChatSheetState: MutableStateFlow<AnimatedViewState>) {
  Row(
    Modifier
      .fillMaxSize()
      .background(SimplexGreen)
      .combinedClickable(onClick = {
        platform.androidStartCallActivity(false)
      })
      .padding(horizontal = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.Center
  ) {
    Spacer(Modifier.weight(1f))
    CallDuration(call)
  }
  DisposableEffectOnGone {
    chatModel.activeCallViewIsCollapsed.value = false
  }
  val window = (LocalContext.current as Activity).window
  DisposableEffect(Unit) {
    window.statusBarColor = SimplexGreen.toArgb()
    onDispose {
      window.statusBarColor = Color.Black.toArgb()
    }
  }
}

@Composable
private fun CallDuration(call: Call) {
  val connectedAt = call.connectedAt
  if (connectedAt != null) {
    val time = remember { mutableStateOf(durationText(0)) }
    LaunchedEffect(Unit) {
      while (true) {
        time.value = durationText((Clock.System.now() - connectedAt).inWholeSeconds.toInt())
        delay(250)
      }
    }
    val sp50 = with(LocalDensity.current) { 50.sp.toDp() }
    Text(time.value, Modifier.widthIn(min = sp50), color = Color.White)
  }
}
