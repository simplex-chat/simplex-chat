package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import kotlin.math.absoluteValue

@Composable
fun CloseSheetBar(
  close: (() -> Unit)?,
  showClose: Boolean = true,
  tintColor: Color = if (close != null) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
  arrangement: Arrangement.Vertical = Arrangement.Top,
  closeBarTitle: String? = null,
  barPaddingValues: PaddingValues = PaddingValues(horizontal = AppBarHorizontalPadding),
  endButtons: @Composable RowScope.() -> Unit = {}
) {
  val background = MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)
  val themeBackgroundMix = background.copy(remember { appPrefs.inAppBarsAlpha.state }.value)
  val handler = LocalAppBarHandler.current
  val connection = LocalAppBarHandler.current?.connection
  val title = remember(handler?.title?.value) { handler?.title ?: mutableStateOf("") }

  val interactionSource = remember { MutableInteractionSource() }
  Column(
    verticalArrangement = arrangement,
    modifier = Modifier
      .fillMaxWidth()
      .then(if (arrangement == Arrangement.Bottom) Modifier.navigationBarsPadding() else Modifier)
      .clickable(interactionSource = interactionSource, indication = null) { /* receive clicks to not allow to click through */ }
      .heightIn(min = AppBarHeight * fontSizeSqrtMultiplier)
      .drawWithCache {
        val backgroundColor = if (arrangement == Arrangement.Bottom) themeBackgroundMix else if (connection != null) themeBackgroundMix.copy(alpha = topTitleAlpha(false, connection)) else Color.Transparent
        onDrawBehind {
          drawRect(backgroundColor)
        }
      }
  ) {
    Row(
      modifier = Modifier.padding(barPaddingValues),
      content = {
        Row(
          Modifier
            .fillMaxWidth()
            .then(if (arrangement == Arrangement.Top) Modifier.statusBarsPadding() else Modifier)
            .height(AppBarHeight * fontSizeSqrtMultiplier),
          verticalAlignment = Alignment.CenterVertically
        ) {
          if (showClose) {
            NavigationButtonBack(tintColor = tintColor, onButtonClicked = close)
          } else {
            Spacer(Modifier)
          }
          if (!closeBarTitle.isNullOrEmpty()) {
            Row(
              Modifier.weight(1f),
              horizontalArrangement = Arrangement.Center,
              verticalAlignment = Alignment.CenterVertically
            ) {
              Text(
                closeBarTitle,
                fontWeight = FontWeight.SemiBold,
                maxLines = 1
              )
            }
          } else if (title.value.isNotEmpty() && connection != null) {
            Row(
              Modifier
                .padding(start = if (showClose) 0.dp else DEFAULT_PADDING_HALF)
                .weight(1f) // hides the title if something wants full width (eg, search field in chat profiles screen)
                .graphicsLayer {
                  alpha = topTitleAlpha(true, connection)
                }
                .padding(start = 4.dp),
              verticalAlignment = Alignment.CenterVertically
            ) {
              Text(
                title.value,
                fontWeight = FontWeight.SemiBold,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis
              )
            }
          } else {
            Spacer(Modifier.weight(1f))
          }
          Row {
            endButtons()
          }
        }
      }
    )
    if (closeBarTitle.isNullOrEmpty() && title.value.isNotEmpty() && connection != null) {
      Divider(
        Modifier
          .graphicsLayer {
            alpha = topTitleAlpha(false, connection)
          }
      )
    }
  }
}

@Composable
fun AppBarTitle(title: String, hostDevice: Pair<Long?, String>? = null,  withPadding: Boolean = true, bottomPadding: Dp = DEFAULT_PADDING * 1.5f + 8.dp) {
  val handler = LocalAppBarHandler.current
  val connection = handler?.connection
  LaunchedEffect(title) {
    handler?.title?.value = title
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

private fun topTitleAlpha(text: Boolean, connection: CollapsingAppBarNestedScrollConnection) =
  if (connection.appBarOffset.absoluteValue < AppBarHandler.appBarMaxHeightPx / 3) 0f
  else ((-connection.appBarOffset * 1.5f) / (AppBarHandler.appBarMaxHeightPx)).coerceIn(0f, if (text) 1f else appPrefs.inAppBarsAlpha.get())

private fun bottomTitleAlpha(connection: CollapsingAppBarNestedScrollConnection?) =
  if ((connection?.appBarOffset ?: 0f).absoluteValue < AppBarHandler.appBarMaxHeightPx / 3) 1f
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

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewCloseSheetBar() {
  SimpleXTheme {
    CloseSheetBar(close = {})
  }
}
