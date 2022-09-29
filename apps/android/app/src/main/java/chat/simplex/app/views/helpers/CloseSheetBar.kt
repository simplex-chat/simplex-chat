package chat.simplex.app.views.helpers

import android.content.res.Configuration
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.ui.theme.*

@Composable
fun CloseSheetBar(title: String? = null, close: () -> Unit) {
  DefaultTopAppBar(
    navigationButton = { NavigationButtonBack(close) },
    title = { AppBarTitle(title) },
    onTitleClick = null,
    showSearch = false,
    onSearchValueChanged = {},
    buttons = emptyList()
  )
}

@Composable
private fun AppBarTitle(title: String?) {
  Text(
    title ?: "", fontWeight = FontWeight.SemiBold,
    maxLines = 1, overflow = TextOverflow.Ellipsis
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
