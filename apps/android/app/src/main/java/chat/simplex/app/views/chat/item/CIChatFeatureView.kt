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

@Composable
fun CIChatFeatureView(
  chatItem: ChatItem,
  feature: Feature,
  iconColor: Color
) {
  @Composable
  fun featureIcon() {
    Icon(feature.iconFilled, feature.text, Modifier.size(15.dp), tint = iconColor)
  }

  @Composable
  fun featureText() {
    Text(
      chatEventText(chatItem),
      Modifier,
      // this is important. Otherwise, aligning will be bad because annotated string has a Span with size 12.sp
      fontSize = 12.sp
    )
  }

  Row(
    Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    if (chatItem.chatDir.sent) {
      featureText()
      featureIcon()
    } else {
      featureIcon()
      featureText()
    }
  }
}
