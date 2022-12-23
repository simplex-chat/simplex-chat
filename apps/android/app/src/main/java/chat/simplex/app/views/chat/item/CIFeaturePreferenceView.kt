package chat.simplex.app.views.chat.item

import android.content.ActivityNotFoundException
import android.util.Log
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun CIFeaturePreferenceView(
  chatItem: ChatItem,
  contact: Contact?,
  feature: ChatFeature,
  allowed: FeatureAllowed,
  acceptFeature: (Contact, ChatFeature) -> Unit
) {
  Row(
    Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Icon(feature.icon, feature.text, Modifier.size(18.dp), tint = HighOrLowlight)
    if (contact != null && allowed != FeatureAllowed.NO && contact.allowsFeature(feature) && !contact.userAllowsFeature(feature)) {
      val acceptStyle = SpanStyle(color = MaterialTheme.colors.primary, fontSize = 12.sp)
      val annotatedText = buildAnnotatedString {
        withStyle(chatEventStyle) { append(chatItem.content.text + "  ") }
        withAnnotation(tag = "Accept", annotation = "Accept") {
          withStyle(acceptStyle) { append(generalGetString(R.string.accept) + "  ") }
        }
        withStyle(chatEventStyle) { append(chatItem.timestampText) }
      }
      fun accept(offset: Int): Boolean = annotatedText.getStringAnnotations(tag = "Accept", start = offset, end = offset).isNotEmpty()
      ClickableText(
        annotatedText,
        onClick = { if (accept(it)) { acceptFeature(contact, feature) } },
        shouldConsumeEvent = ::accept
      )
    } else {
      Text(chatItem.content.text + "  " + chatItem.timestampText,
        fontSize = 12.sp, fontWeight = FontWeight.Light, color = HighOrLowlight)
    }
  }
}
