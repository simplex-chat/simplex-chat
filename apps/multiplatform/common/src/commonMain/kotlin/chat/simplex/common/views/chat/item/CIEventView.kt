package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.padding
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.LocalFlatLayout
import chat.simplex.common.ui.theme.*

@Composable
fun CIEventView(text: AnnotatedString) {
  val padding = if (LocalFlatLayout.current) PaddingValues(start = FLAT_TEXT_START_PADDING, top = FLAT_TEXT_VERTICAL_PADDING, end = 6.dp, bottom = FLAT_TEXT_VERTICAL_PADDING) else PaddingValues(horizontal = 6.dp, vertical = 6.dp)
  Text(text, Modifier.padding(padding), style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp), maxLines = 4)
}
@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun CIEventViewPreview() {
  SimpleXTheme {
    CIEventView(buildAnnotatedString { append("event happened") })
  }
}
