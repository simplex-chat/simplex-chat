package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.padding
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.*

@Composable
fun CIEventView(text: AnnotatedString) {
  Text(text, Modifier.padding(horizontal = 8.dp, vertical = 4.dp), style = chatMessageTextStyle(12.sp), maxLines = 4)
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
