package chat.simplex.app.views.helpers

import chat.simplex.app.R
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.*

@Composable
fun DefaultTopAppBar(
  navigationButton: @Composable () -> Unit,
  title: @Composable () -> Unit,
  onTitleClick: (() -> Unit)? = null,
  showSearch: Boolean,
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
        title()
      } else {
        SearchTextField(Modifier.fillMaxWidth(), stringResource(android.R.string.search_go), onSearchValueChanged)
      }
    },
    backgroundColor = if (isInDarkTheme()) ToolbarDark else ToolbarLight,
    navigationIcon = navigationButton,
    buttons = if (!showSearch) buttons else emptyList(),
    centered = !showSearch
  )
}

@Composable
fun NavigationButtonBack(onButtonClicked: () -> Unit) {
  IconButton(onButtonClicked) {
    Icon(
      Icons.Outlined.ArrowBack, stringResource(R.string.back), tint = MaterialTheme.colors.primary
    )
  }
}

@Composable
fun NavigationButtonMenu(onButtonClicked: () -> Unit) {
  IconButton(onClick = onButtonClicked) {
    Icon(
      Icons.Outlined.Menu,
      stringResource(R.string.icon_descr_settings),
      tint = MaterialTheme.colors.primary,
    )
  }
}

@Composable
private fun TopAppBar(
  title: @Composable () -> Unit,
  modifier: Modifier = Modifier,
  navigationIcon: @Composable (() -> Unit)? = null,
  buttons: List<@Composable RowScope.() -> Unit> = emptyList(),
  backgroundColor: Color = MaterialTheme.colors.primarySurface,
  centered: Boolean,
) {
  TopAppBar(
    title,
    modifier,
    navigationIcon,
    { Row{ buttons.forEach { it() }}},
    elevation = 1.dp,
    backgroundColor = backgroundColor,
  )
}

val AppBarHeight = 56.dp
private val AppBarHorizontalPadding = 4.dp
private val TitleInsetWithoutIcon = 16.dp - AppBarHorizontalPadding
private val TitleInsetWithIcon = 72.dp
