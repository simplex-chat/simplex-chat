package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*

@Composable
fun CIChatFeatureView(
  chatItem: ChatItem,
  feature: Feature,
  iconColor: Color
) {
  Row(
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(feature.icon(true), feature.text(), Modifier.size(15.dp), tint = iconColor)
    Text(
      chatEventText(chatItem),
      Modifier,
      // this is important. Otherwise, aligning will be bad because annotated string has a Span with size 12.sp
      fontSize = 12.sp
    )
  }
}
