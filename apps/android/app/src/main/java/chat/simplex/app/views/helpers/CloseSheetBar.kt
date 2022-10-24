package chat.simplex.app.views.helpers

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.*

@Composable
fun CloseSheetBar(close: () -> Unit) {
  Column(
    Modifier
      .fillMaxWidth()
      .heightIn(min = AppBarHeight)
      .padding(horizontal = AppBarHorizontalPadding),
  ) {
    Row(
      Modifier
        .width(TitleInsetWithIcon - AppBarHorizontalPadding)
        .padding(top = 4.dp), // Like in DefaultAppBar
      content = { NavigationButtonBack(close) }
    )
  }
}

@Composable
fun AppBarTitle(title: String, withPadding: Boolean = true) {
  val padding = if (withPadding)
    PaddingValues(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING )
  else
    PaddingValues(bottom = DEFAULT_PADDING)
  Text(
    title,
    Modifier
      .fillMaxWidth()
      .padding(padding),
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
    CloseSheetBar(close = {})
  }
}
