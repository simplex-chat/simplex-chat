package chat.simplex.common.views.helpers

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.*
import androidx.compose.ui.graphics.layer.drawLayer
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import kotlin.math.absoluteValue

@Composable
fun DefaultTopAppBar(
  navigationButton: (@Composable RowScope.() -> Unit)? = null,
  title: (@Composable () -> Unit)? = null,
  fixedTitleText: String? = null,
  onTitleClick: (() -> Unit)? = null,
  onTop: Boolean,
  showSearch: Boolean = false,
  searchAlwaysVisible: Boolean = false,
  onSearchValueChanged: (String) -> Unit = {},
  buttons: List<@Composable RowScope.() -> Unit> = emptyList(),
) {
  // If I just disable clickable modifier when don't need it, it will stop passing clicks to search. Replacing the whole modifier
  val modifier = if (!showSearch) {
    Modifier.clickable(enabled = onTitleClick != null, onClick = onTitleClick ?: { })
  } else Modifier

  val themeBackgroundMix = MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)
  val prefAlpha = remember { appPrefs.inAppBarsAlpha.state }.value
  val handler = LocalAppBarHandler.current
  val connection = LocalAppBarHandler.current?.connection
  val titleText = remember(handler?.title?.value, fixedTitleText) {
    if (fixedTitleText != null) {
      mutableStateOf(fixedTitleText)
    } else {
      handler?.title ?: mutableStateOf("")
    }
  }
  Box(modifier) {
    val graphicsLayer = handler?.graphicsLayer
    val blurRadius = remember { appPrefs.appearanceBarsBlurRadius.state }
    Box(Modifier
      .matchParentSize()
      .then(if (graphicsLayer != null && blurRadius.value > 0 && prefAlpha < 1f)
        Modifier
          .graphicsLayer {
            renderEffect = if (blurRadius.value > 0) BlurEffect(blurRadius.value.toFloat(), blurRadius.value.toFloat()) else null
            clip = blurRadius.value > 0
          }
          .drawBehind {
            //drawRect(Color.Black)
            clipRect {
              translate(top = if (!onTop) size.height - graphicsLayer.size.height else 0f) {
                drawLayer(graphicsLayer)
              }
            }
          } else Modifier)
      .drawWithCache {
        val backgroundColor = if (title != null || fixedTitleText != null || connection == null || !onTop) {
          themeBackgroundMix.copy(prefAlpha)
        } else {
          themeBackgroundMix.copy(topTitleAlpha(false, connection))
        }
        onDrawBehind {
          drawRect(backgroundColor)
        }
      }
    )
    Box(
      Modifier
        .fillMaxWidth()
        .then(if (!onTop) Modifier.navigationBarsPadding() else Modifier)
        .heightIn(min = AppBarHeight * fontSizeSqrtMultiplier)
    ) {
      TopAppBar(
        title = {
          if (showSearch) {
            SearchTextField(Modifier.fillMaxWidth(), alwaysVisible = searchAlwaysVisible, onValueChange = onSearchValueChanged)
          } else if (title != null) {
            title()
          } else if (titleText.value.isNotEmpty() && connection != null) {
            Row(
              Modifier
                .graphicsLayer {
                  alpha = if (fixedTitleText != null) prefAlpha else topTitleAlpha(true, connection)
                }
            ) {
              Text(
                titleText.value,
                fontWeight = FontWeight.SemiBold,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis
              )
            }
          }
        },
        navigationIcon = navigationButton,
        buttons = if (!showSearch) buttons else emptyList(),
        centered = !showSearch && (title != null || !onTop),
        onTop = onTop,
      )
      CloseBarDivider(onTop, connection)
    }
  }
}


@Composable
fun CallAppBar(
  title: @Composable () -> Unit,
  onBack: () -> Unit
) {
  TopAppBar(
    title,
    navigationIcon = { NavigationButtonBack(tintColor = Color(0xFFFFFFD8), onButtonClicked = onBack) },
    centered = false,
    onTop = true
  )
}

@Composable
fun NavigationButtonBack(onButtonClicked: (() -> Unit)?, tintColor: Color = if (onButtonClicked != null) MaterialTheme.colors.primary else MaterialTheme.colors.secondary, height: Dp = 24.dp) {
  IconButton(onButtonClicked ?: {}, enabled = onButtonClicked != null) {
    Icon(
      painterResource(MR.images.ic_arrow_back_ios_new), stringResource(MR.strings.back), Modifier.height(height), tint = tintColor
    )
  }
}

@Composable
fun NavigationButtonClose(onButtonClicked: (() -> Unit)?, tintColor: Color = if (onButtonClicked != null) MaterialTheme.colors.primary else MaterialTheme.colors.secondary, height: Dp = 24.dp) {
  IconButton(onButtonClicked ?: {}, enabled = onButtonClicked != null) {
    Icon(
      painterResource(MR.images.ic_close), stringResource(MR.strings.back), Modifier.height(height), tint = tintColor
    )
  }
}

@Composable
fun ShareButton(onButtonClicked: () -> Unit) {
  IconButton(onButtonClicked) {
    Icon(
      painterResource(MR.images.ic_share), stringResource(MR.strings.share_verb), tint = MaterialTheme.colors.primary
    )
  }
}

@Composable
fun NavigationButtonMenu(onButtonClicked: () -> Unit) {
  IconButton(onClick = onButtonClicked) {
    Icon(
      painterResource(MR.images.ic_menu),
      stringResource(MR.strings.icon_descr_settings),
      tint = MaterialTheme.colors.primary,
    )
  }
}

@Composable
private fun BoxScope.CloseBarDivider(onTop: Boolean, connection: CollapsingAppBarNestedScrollConnection?) {
  if (connection != null) {
    val prefAlpha = appPrefs.inAppBarsAlpha.get()
    Divider(
      Modifier
        .align(if (onTop) Alignment.BottomStart else Alignment.TopStart)
        .graphicsLayer {
          alpha = if (!onTop) prefAlpha else topTitleAlpha(false, connection)
        }
    )
  } else {
    Divider(Modifier.align(if (onTop) Alignment.BottomStart else Alignment.TopStart))
  }
}

@Composable
private fun TopAppBar(
  title: @Composable () -> Unit,
  modifier: Modifier = Modifier,
  navigationIcon: @Composable (RowScope.() -> Unit)? = null,
  buttons: List<@Composable RowScope.() -> Unit> = emptyList(),
  centered: Boolean,
  onTop: Boolean,
) {
  Box(
    modifier
      .then(if (onTop) Modifier.statusBarsPadding() else Modifier)
      .height(AppBarHeight * fontSizeSqrtMultiplier)
      .fillMaxWidth()
      .padding(horizontal = 4.dp),
    contentAlignment = Alignment.CenterStart,
  ) {
    if (navigationIcon != null) {
      Row(
        Modifier
          .fillMaxHeight()
          .width(TitleInsetWithIcon - AppBarHorizontalPadding),
        verticalAlignment = Alignment.CenterVertically,
        content = navigationIcon
      )
    }
    Row(
      Modifier.fillMaxSize(),
      horizontalArrangement = Arrangement.End,
      verticalAlignment = Alignment.CenterVertically,
    ) {
      buttons.forEach { it() }
    }
    val startPadding = if (navigationIcon != null) TitleInsetWithIcon else TitleInsetWithoutIcon
    val endPadding = (buttons.size * 50f).dp
    Box(
      Modifier
        .fillMaxWidth()
        .padding(
          start = if (centered) kotlin.math.max(startPadding.value, endPadding.value).dp else startPadding,
          end = if (centered) kotlin.math.max(startPadding.value, endPadding.value).dp else endPadding
        ),
      contentAlignment = if (centered) Alignment.Center else Alignment.CenterStart
    ) {
      title()
    }
  }
}

private fun topTitleAlpha(text: Boolean, connection: CollapsingAppBarNestedScrollConnection) =
  if (connection.appBarOffset.absoluteValue < AppBarHandler.appBarMaxHeightPx / 3) 0f
  else ((-connection.appBarOffset * 1.5f) / (AppBarHandler.appBarMaxHeightPx)).coerceIn(0f, if (text) 1f else appPrefs.inAppBarsAlpha.get())

val AppBarHeight = 56.dp
val AppBarHorizontalPadding = 4.dp
private val TitleInsetWithoutIcon = DEFAULT_PADDING - AppBarHorizontalPadding
val TitleInsetWithIcon = 52.dp
