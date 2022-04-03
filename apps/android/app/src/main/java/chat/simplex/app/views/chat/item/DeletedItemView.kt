package chat.simplex.app.views.chat.item

import android.content.res.Configuration
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.tooling.preview.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun DeletedItemView(ci: ChatItem, showMember: Boolean = false) {
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = ReceivedColorLight,
  ) {
    Row(
      Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
      verticalAlignment = Alignment.Bottom
    ) {
      Text(
        buildAnnotatedString {
          appendSender(this, if (showMember) ci.memberDisplayName else null, true)
          withStyle(SpanStyle(fontStyle = FontStyle.Italic, color = HighOrLowlight)) { append(ci.content.text) }
        },
        style = MaterialTheme.typography.body1.copy(lineHeight = 22.sp),
        modifier = Modifier.padding(end = 8.dp)
      )
      CIMetaView(ci)
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun PreviewDeletedItemView() {
  SimpleXTheme {
    DeletedItemView(
      ChatItem.getDeletedContentSampleData()
    )
  }
}
