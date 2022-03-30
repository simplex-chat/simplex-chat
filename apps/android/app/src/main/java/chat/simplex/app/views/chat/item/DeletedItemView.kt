package chat.simplex.app.views.chat.item

import android.content.res.Configuration
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun DeletedItemView(ci: ChatItem) {
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = MaterialTheme.colors.background,
    border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
  ) {
    Box(contentAlignment = Alignment.BottomEnd) {
      Column(
        Modifier
          .width(IntrinsicSize.Max)
          .padding(vertical = 6.dp, horizontal = 12.dp)
          .padding(bottom = 18.dp),
        horizontalAlignment = Alignment.Start
      ) {
        Text(ci.content.text, style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onSurface, lineHeight = 22.sp))
      }
      Box(Modifier.padding(vertical = 6.dp, horizontal = 12.dp)) {
        CIMetaView(ci)
      }
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
