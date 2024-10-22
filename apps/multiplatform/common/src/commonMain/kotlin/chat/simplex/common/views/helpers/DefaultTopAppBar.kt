package chat.simplex.common.views.helpers

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.unit.Dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR

@Composable
fun DefaultTopAppBar(
  navigationButton: (@Composable RowScope.() -> Unit)? = null,
  title: (@Composable () -> Unit)?,
  onTitleClick: (() -> Unit)? = null,
  showSearch: Boolean,
  onTop: Boolean,
  onSearchValueChanged: (String) -> Unit,
  buttons: List<@Composable RowScope.() -> Unit> = emptyList(),
) {
  // If I just disable clickable modifier when don't need it, it will stop passing clicks to search. Replacing the whole modifier
  val modifier = if (!showSearch) {
    Modifier.clickable(enabled = onTitleClick != null, onClick = onTitleClick ?: { })
  } else Modifier

  TopAppBar(
    modifier = modifier,
    title = {
      if (!showSearch) {
        title?.invoke()
      } else {
        SearchTextField(Modifier.fillMaxWidth(), alwaysVisible = false, onValueChange = onSearchValueChanged)
      }
    },
    backgroundColor = MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)
      .copy(remember { appPrefs.inAppBarsAlpha.state }.value),
    navigationIcon = navigationButton,
    buttons = if (!showSearch) buttons else emptyList(),
    centered = !showSearch,
    onTop = onTop,
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
private fun TopAppBar(
  title: @Composable () -> Unit,
  modifier: Modifier = Modifier,
  navigationIcon: @Composable (RowScope.() -> Unit)? = null,
  buttons: List<@Composable RowScope.() -> Unit> = emptyList(),
  backgroundColor: Color = MaterialTheme.colors.primarySurface,
  centered: Boolean,
  onTop: Boolean,
) {
  Box(
    modifier
      .fillMaxWidth()
      .then(if (!onTop) Modifier.navigationBarsPadding() else Modifier)
      .background(backgroundColor)
      .then(if (onTop) Modifier.statusBarsPadding() else Modifier)
      .height(AppBarHeight * fontSizeSqrtMultiplier)
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
      Modifier
        .fillMaxHeight()
        .fillMaxWidth(),
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
      contentAlignment = Alignment.Center
    ) {
      title()
    }
  }
}

val AppBarHeight = 56.dp
val AppBarHorizontalPadding = 4.dp
val BottomAppBarHeight = 60.dp
private val TitleInsetWithoutIcon = DEFAULT_PADDING - AppBarHorizontalPadding
val TitleInsetWithIcon = 72.dp
