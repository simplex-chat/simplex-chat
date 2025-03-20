package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.ANDROID_CALL_TOP_PADDING
import chat.simplex.common.model.durationText
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.delay
import kotlinx.datetime.Clock

private val CALL_INTERACTIVE_AREA_HEIGHT = 74.dp
private val CALL_TOP_OFFSET = (-10).dp
private val CALL_TOP_GREEN_LINE_HEIGHT = ANDROID_CALL_TOP_PADDING - CALL_TOP_OFFSET
private val CALL_BOTTOM_ICON_OFFSET = (-15).dp
private val CALL_BOTTOM_ICON_HEIGHT = CALL_INTERACTIVE_AREA_HEIGHT + CALL_BOTTOM_ICON_OFFSET

@Composable
actual fun TagsRow(content: @Composable() (() -> Unit)) {
  Row(
    modifier = Modifier
      .padding(horizontal = 14.dp)
      .horizontalScroll(rememberScrollState()),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(2.dp)
  ) {
    content()
  }
}

@Composable
actual fun ActiveCallInteractiveArea(call: Call) {
  val onClick = { platform.androidStartCallActivity(false) }
  val statusBar = WindowInsets.statusBars.asPaddingValues().calculateTopPadding()
  Box(Modifier.offset(y = CALL_TOP_OFFSET).height(CALL_INTERACTIVE_AREA_HEIGHT + statusBar)) {
    val source = remember { MutableInteractionSource() }
    val ripple = remember { ripple(bounded = true, 3000.dp) }
    Box(Modifier.height(CALL_TOP_GREEN_LINE_HEIGHT + statusBar).clickable(onClick = onClick, indication = ripple, interactionSource = source)) {
      GreenLine(statusBar, call)
    }
    Box(
      Modifier
        .offset(y = CALL_BOTTOM_ICON_OFFSET)
        .size(CALL_BOTTOM_ICON_HEIGHT)
        .background(SimplexGreen, CircleShape)
        .clip(CircleShape)
        .clickable(onClick = onClick, indication = ripple, interactionSource = source)
        .align(Alignment.BottomCenter),
      contentAlignment = Alignment.Center
    ) {
      if (call.hasVideo) {
        Icon(painterResource(MR.images.ic_videocam_filled), null, Modifier.size(27.dp).offset(x = 2.5.dp, y = 2.dp), tint = Color.White)
      } else {
        Icon(painterResource(MR.images.ic_call_filled), null, Modifier.size(27.dp).offset(x = -0.5.dp, y = 2.dp), tint = Color.White)
      }
    }
  }
}

@Composable
private fun GreenLine(statusBarHeight: Dp, call: Call) {
  Row(
    Modifier
      .fillMaxSize()
      .background(SimplexGreen)
      .padding(top = -CALL_TOP_OFFSET + statusBarHeight)
      .padding(horizontal = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.Center
  ) {
    ContactName(call.contact.displayName)
    Spacer(Modifier.weight(1f))
    CallDuration(call)
  }
  DisposableEffect(Unit) {
    platform.androidSetStatusAndNavigationBarAppearance(false, CurrentColors.value.colors.isLight)
    onDispose {
      platform.androidSetStatusAndNavigationBarAppearance(CurrentColors.value.colors.isLight, CurrentColors.value.colors.isLight)
    }
  }
}

@Composable
private fun ContactName(name: String) {
  Text(name, Modifier.width(windowWidth() * 0.35f), color = Color.White, maxLines = 1, overflow = TextOverflow.Ellipsis)
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
    val text = time.value
    val sp40Or50 = with(LocalDensity.current) { if (text.length >= 6) 60.sp.toDp() else 42.sp.toDp() }
    val offset = with(LocalDensity.current) { 7.sp.toDp() }
    Text(text, Modifier.offset(x = offset).widthIn(min = sp40Or50), color = Color.White)
  }
}
