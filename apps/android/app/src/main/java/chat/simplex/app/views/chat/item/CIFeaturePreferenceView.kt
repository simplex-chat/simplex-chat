package chat.simplex.app.views.chat.item

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleButton

@Composable
fun CIFeaturePreferenceView(
  chatItem: ChatItem,
  chatInfo: ChatInfo,
  feature: ChatFeature,
  allowed: FeatureAllowed,
  acceptFeature: (Contact, ChatFeature) -> Unit
) {
  Row(
    Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Icon(feature.icon, feature.text, Modifier.size(15.dp), tint = HighOrLowlight)
    Text(chatItem.content.text, fontSize = 12.sp, fontWeight = FontWeight.Light, color = HighOrLowlight)
    if (chatInfo is ChatInfo.Direct && allowed != FeatureAllowed.NO) {
      val ct = chatInfo.contact
      if (ct.allowsFeature(feature) && !ct.userAllowsFeature(feature)) {
        Text(stringResource(R.string.accept), modifier = Modifier.clickable { acceptFeature(ct, feature) },
          fontSize = 12.sp, color = MaterialTheme.colors.primary)
      }
    }
    Text(chatItem.timestampText, fontSize = 12.sp, fontWeight = FontWeight.Light, color = HighOrLowlight)
//      buildAnnotatedString {
//        withChatEventStyle(this, chatItem.content.text)
//        append(" ")
//        withChatEventStyle(this, chatItem.timestampText)
//      },
//      Modifier,
//      // this is important. Otherwise, aligning will be bad because annotated string has a Span with size 12.sp
//      fontSize = 12.sp
//    )
  }
}
