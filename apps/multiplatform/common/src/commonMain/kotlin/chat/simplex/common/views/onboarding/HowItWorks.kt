package chat.simplex.common.views.onboarding

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource

@Composable
fun HowItWorks(user: User?, onboardingStage: SharedPreference<OnboardingStage>? = null) {
  ColumnWithScrollBar(Modifier.padding(DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.how_simplex_works), withPadding = false)
    ReadableText(MR.strings.many_people_asked_how_can_it_deliver)
    ReadableText(MR.strings.to_protect_privacy_simplex_has_ids_for_queues)
    ReadableText(MR.strings.you_control_servers_to_receive_your_contacts_to_send)
    ReadableText(MR.strings.only_client_devices_store_contacts_groups_e2e_encrypted_messages)
    if (onboardingStage == null) {
      ReadableTextWithLink(MR.strings.read_more_in_github_with_link, "https://github.com/simplex-chat/simplex-chat#readme")
    } else {
      ReadableText(MR.strings.read_more_in_github)
    }

    Spacer(Modifier.fillMaxHeight().weight(1f))

    if (onboardingStage != null) {
      Box(Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
        OnboardingActionButton(user, onboardingStage, onclick = { ModalManager.fullscreen.closeModal() })
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))
    }
    Spacer(Modifier.height(DEFAULT_PADDING))
  }
}

@Composable
fun ReadableText(stringResId: StringResource, textAlign: TextAlign = TextAlign.Start, padding: PaddingValues = PaddingValues(bottom = 12.dp), style: TextStyle = LocalTextStyle.current, args: Any? = null) {
  Text(annotatedStringResource(stringResId, args), modifier = Modifier.padding(padding), textAlign = textAlign, lineHeight = 22.sp, style = style)
}

@Composable
fun ReadableTextWithLink(stringResId: StringResource, link: String, textAlign: TextAlign = TextAlign.Start, padding: PaddingValues = PaddingValues(bottom = 12.dp), simplexLink: Boolean = false) {
  val annotated = annotatedStringResource(stringResId)
  val primary = MaterialTheme.colors.primary
  // This replaces links in text highlighted with specific color, e.g. SimplexBlue
  val newStyles = remember(stringResId) {
    val newStyles = ArrayList<AnnotatedString.Range<SpanStyle>>()
    annotated.spanStyles.forEach {
      if (it.item.color == SimplexBlue) {
        newStyles.add(it.copy(item = it.item.copy(primary)))
      } else {
        newStyles.add(it)
      }
    }
    newStyles
  }
  val uriHandler = LocalUriHandler.current
  Text(AnnotatedString(annotated.text, newStyles), modifier = Modifier.padding(padding).clickable { if (simplexLink) uriHandler.openVerifiedSimplexUri(link) else uriHandler.openUriCatching(link) }, textAlign = textAlign, lineHeight = 22.sp)
}

@Composable
fun ReadableText(text: String, textAlign: TextAlign = TextAlign.Start, padding: PaddingValues = PaddingValues(bottom = 12.dp)) {
  Text(text, modifier = Modifier.padding(padding), textAlign = textAlign, lineHeight = 22.sp)
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewHowItWorks() {
  SimpleXTheme {
    HowItWorks(user = null)
  }
}
