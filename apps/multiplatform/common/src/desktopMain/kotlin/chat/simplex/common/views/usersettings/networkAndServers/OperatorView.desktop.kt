package chat.simplex.common.views.usersettings.networkAndServers

import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.DesktopScrollBar
import chat.simplex.common.views.helpers.detectCursorMove
import kotlinx.coroutines.*

@Composable
actual fun ConditionsBox(modifier: Modifier, scrollState: ScrollState, content: @Composable() (BoxScope.() -> Unit)) {
  val scope = rememberCoroutineScope()
  val scrollBarAlpha = remember { Animatable(0f) }
  val scrollJob: MutableState<Job> = remember { mutableStateOf(Job()) }
  val scrollBarDraggingState = remember { mutableStateOf(false) }
  val scrollModifier = remember {
    Modifier
      .pointerInput(Unit) {
        detectCursorMove {
          scope.launch {
            scrollBarAlpha.animateTo(1f)
          }
          scrollJob.value.cancel()
          scrollJob.value = scope.launch {
            delay(1000L)
            scrollBarAlpha.animateTo(0f)
          }
        }
      }
  }

  Box(modifier = modifier) {
    Box(
      Modifier
        .fillMaxSize()
        .verticalScroll(scrollState)
        .padding(8.dp)
        .then(scrollModifier)
    ) {
      content()
    }
    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.CenterEnd) {
      DesktopScrollBar(rememberScrollbarAdapter(scrollState), Modifier.fillMaxHeight(), scrollBarAlpha, scrollJob, false, scrollBarDraggingState)
    }
  }
}
