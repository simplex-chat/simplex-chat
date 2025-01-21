package chat.simplex.common.views.onboarding

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.background
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
  ColumnWithScrollBar(Modifier.padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.how_simplex_works), withPadding = false)
    ReadableText(MR.strings.to_protect_privacy_simplex_has_ids_for_queues)
    ReadableText(MR.strings.only_client_devices_store_contacts_groups_e2e_encrypted_messages)
    ReadableText(MR.strings.all_message_and_files_e2e_encrypted)
    if (onboardingStage == null) {
      ReadableTextWithLink(MR.strings.read_more_in_github_with_link, "https://github.com/simplex-chat/simplex-chat#readme")
    }

    Spacer(Modifier.fillMaxHeight().weight(1f))

    if (onboardingStage != null) {
      Column(Modifier.fillMaxWidth(), horizontalAlignment = Alignment.CenterHorizontally) {
        OnboardingActionButton(user, onboardingStage, onclick = { ModalManager.fullscreen.closeModal() })
        // Reserve space
        TextButtonBelowOnboardingButton("", null)
      }
    }
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
