package chat.simplex.app.views.helpers

import android.content.res.Configuration
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.ui.theme.*

@Composable
fun CloseSheetBar(title: String? = null, close: () -> Unit) {
  DefaultTopAppBar(
    navigationButton = { NavigationButtonBack(close) },
    title = if (title != null) { { AppBarTitle(title)} } else null,
    onTitleClick = null,
    showSearch = false,
    onSearchValueChanged = {},
    buttons = emptyList(),
    bigBar = true,
  )
}

@Composable
private fun AppBarTitle(title: String) {
  Text(
    title,
    Modifier,
    overflow = TextOverflow.Ellipsis,
    style = MaterialTheme.typography.h1
  )
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewCloseSheetBar() {
  SimpleXTheme {
    CloseSheetBar("Test", close = {})
  }
}
