package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun CIFeaturePreferenceView(
  chatItem: ChatItem,
  contact: Contact?,
  feature: ChatFeature,
  allowed: FeatureAllowed,
  acceptFeature: (Contact, ChatFeature, Int?) -> Unit
) {
  Row(
    Modifier.padding(horizontal = 6.dp, vertical = 6.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Icon(feature.icon, feature.text, Modifier.size(18.dp), tint = MaterialTheme.colors.secondary)
    if (contact != null && allowed != FeatureAllowed.NO && contact.allowsFeature(feature) && !contact.userAllowsFeature(feature)) {
      val acceptStyle = SpanStyle(color = MaterialTheme.colors.primary, fontSize = 12.sp)
      val setParam = feature == ChatFeature.TimedMessages && contact.mergedPreferences.timedMessages.userPreference.pref.ttl == null
      val acceptTextId = if (setParam) R.string.accept_feature_set_1_day else R.string.accept_feature
      val param = if (setParam) 86400 else null
      val annotatedText = buildAnnotatedString {
        withStyle(chatEventStyle) { append(chatItem.content.text + "  ") }
        withAnnotation(tag = "Accept", annotation = "Accept") {
          withStyle(acceptStyle) { append(generalGetString(acceptTextId) + "  ") }
        }
        withStyle(chatEventStyle) { append(chatItem.timestampText) }
      }
      fun accept(offset: Int): Boolean = annotatedText.getStringAnnotations(tag = "Accept", start = offset, end = offset).isNotEmpty()
      ClickableText(
        annotatedText,
        onClick = { if (accept(it)) { acceptFeature(contact, feature, param) } },
        shouldConsumeEvent = ::accept
      )
    } else {
      Text(chatItem.content.text + "  " + chatItem.timestampText,
        fontSize = 12.sp, fontWeight = FontWeight.Light, color = MaterialTheme.colors.secondary)
    }
  }
}
