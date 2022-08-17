package chat.simplex.app.views.helpers

import chat.simplex.app.R
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowBackIos
import androidx.compose.material.icons.outlined.Menu
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.ToolbarDark
import chat.simplex.app.ui.theme.ToolbarLight
import chat.simplex.app.views.helpers.SearchTextField

@Composable
fun DefaultTopAppBar(
  navigationButton: @Composable () -> Unit,
  title: @Composable () -> Unit,
  onTitleClick: (() -> Unit)? = null,
  showSearch: Boolean,
  onSearchValueChanged: (String) -> Unit,
  buttons: @Composable RowScope.() -> Unit,
) {
  TopAppBar(
    modifier = Modifier
      .height(AppBarHeight)
      .background(if (isSystemInDarkTheme()) ToolbarDark else ToolbarLight)
      .clickable(enabled = onTitleClick != null, onClick = onTitleClick ?: { }),
    title = {
      Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
        if (!showSearch) {
          title()
        } else {
          SearchTextField(Modifier.fillMaxWidth(), stringResource(android.R.string.search_go), onSearchValueChanged)
        }
      }
    },
    elevation = 0.dp,
    backgroundColor = if (isSystemInDarkTheme()) ToolbarDark else ToolbarLight,
    navigationIcon = navigationButton,
    actions = if (!showSearch) buttons else {
      {}
    }
  )
}

@Composable
fun NavigationButtonBack(onButtonClicked: () -> Unit) {
  IconButton(onButtonClicked) {
    Icon(Icons.Outlined.ArrowBackIos, stringResource(R.string.back), tint = MaterialTheme.colors.primary)
  }
}

@Composable
fun NavigationButtonMenu(onButtonClicked: () -> Unit) {
  IconButton(onClick = onButtonClicked) {
    Icon(
      Icons.Outlined.Menu,
      stringResource(R.string.icon_descr_settings),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(10.dp)
    )
  }
}
