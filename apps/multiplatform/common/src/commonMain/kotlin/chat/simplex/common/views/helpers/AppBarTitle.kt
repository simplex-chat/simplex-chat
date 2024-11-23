package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.graphics.*
import androidx.compose.ui.unit.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlin.math.absoluteValue

@Composable
fun AppBarTitle(
  title: String,
  hostDevice: Pair<Long?, String>? = null,
  withPadding: Boolean = true,
  bottomPadding: Dp = DEFAULT_PADDING * 1.5f + 8.dp,
  enableAlphaChanges: Boolean = true
) {
  val handler = LocalAppBarHandler.current
  val connection = if (enableAlphaChanges) handler?.connection else null
  LaunchedEffect(title) {
    if (enableAlphaChanges) {
      handler?.title?.value = title
    } else {
      handler?.connection?.scrollTrackingEnabled = false
    }
  }
  val theme = CurrentColors.collectAsState()
  val titleColor = MaterialTheme.appColors.title
  val brush = if (theme.value.base == DefaultTheme.SIMPLEX)
    Brush.linearGradient(listOf(titleColor.darker(0.2f), titleColor.lighter(0.35f)), Offset(0f, Float.POSITIVE_INFINITY), Offset(Float.POSITIVE_INFINITY, 0f))
  else // color is not updated when changing themes if I pass null here
    Brush.linearGradient(listOf(titleColor, titleColor), Offset(0f, Float.POSITIVE_INFINITY), Offset(Float.POSITIVE_INFINITY, 0f))
  Column {
    Text(
      title,
      Modifier
        .padding(start = if (withPadding) DEFAULT_PADDING else 0.dp, top = DEFAULT_PADDING_HALF, end = if (withPadding) DEFAULT_PADDING else 0.dp,)
        .graphicsLayer {
          alpha = bottomTitleAlpha(connection)
        },
      overflow = TextOverflow.Ellipsis,
      style = MaterialTheme.typography.h1.copy(brush = brush),
      color = MaterialTheme.colors.primaryVariant,
      textAlign = TextAlign.Start
    )
    if (hostDevice != null) {
      Box(Modifier.padding(start = if (withPadding) DEFAULT_PADDING else 0.dp, end = if (withPadding) DEFAULT_PADDING else 0.dp).graphicsLayer {
        alpha = bottomTitleAlpha(connection)
      }) {
        HostDeviceTitle(hostDevice)
      }
    }
    Spacer(Modifier.height(bottomPadding))
  }
}

private fun bottomTitleAlpha(connection: CollapsingAppBarNestedScrollConnection?) =
  if (connection?.scrollTrackingEnabled == false) 1f
  else if ((connection?.appBarOffset ?: 0f).absoluteValue < AppBarHandler.appBarMaxHeightPx / 3) 1f
  else ((AppBarHandler.appBarMaxHeightPx) + (connection?.appBarOffset ?: 0f) / 1.5f).coerceAtLeast(0f) / AppBarHandler.appBarMaxHeightPx

@Composable
private fun HostDeviceTitle(hostDevice: Pair<Long?, String>, extraPadding: Boolean = false) {
  Row(Modifier.fillMaxWidth().padding(top = 5.dp, bottom = if (extraPadding) DEFAULT_PADDING * 2 else DEFAULT_PADDING_HALF), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Start) {
    DevicePill(
      active = true,
      onClick = {},
      actionButtonVisible = false,
      icon = painterResource(if (hostDevice.first == null) MR.images.ic_desktop else MR.images.ic_smartphone_300),
      text = hostDevice.second
    )
  }
}
