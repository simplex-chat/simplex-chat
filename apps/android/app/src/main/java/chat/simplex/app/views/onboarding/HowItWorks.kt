package chat.simplex.app.views.onboarding

import android.content.res.Configuration
import androidx.annotation.StringRes
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.User
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.MarkdownText
import chat.simplex.app.views.helpers.*

@Composable
fun HowItWorks(user: User?, onboardingStage: MutableState<OnboardingStage?>? = null) {
  Column(Modifier
    .fillMaxWidth()
    .padding(horizontal = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(R.string.how_simplex_works), false)
    ReadableText(R.string.many_people_asked_how_can_it_deliver)
    ReadableText(R.string.to_protect_privacy_simplex_has_ids_for_queues)
    ReadableText(R.string.you_control_servers_to_receive_your_contacts_to_send)
    ReadableText(R.string.only_client_devices_store_contacts_groups_e2e_encrypted_messages)
    if (onboardingStage == null) {
      ReadableTextWithLink(R.string.read_more_in_github_with_link, "https://github.com/simplex-chat/simplex-chat#readme")
    } else {
      ReadableText(R.string.read_more_in_github)
    }

    Spacer(Modifier.fillMaxHeight().weight(1f))

    if (onboardingStage != null) {
      Box(Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
        OnboardingActionButton(user, onboardingStage, onclick = { ModalManager.shared.closeModal() })
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))
    }
  }
}

@Composable
fun ReadableText(@StringRes stringResId: Int, textAlign: TextAlign = TextAlign.Start, padding: PaddingValues = PaddingValues(bottom = 12.dp), style: TextStyle = LocalTextStyle.current) {
  Text(annotatedStringResource(stringResId), modifier = Modifier.padding(padding), textAlign = textAlign, lineHeight = 22.sp, style = style)
}

@Composable
fun ReadableTextWithLink(@StringRes stringResId: Int, link: String, textAlign: TextAlign = TextAlign.Start, padding: PaddingValues = PaddingValues(bottom = 12.dp)) {
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
  Text(AnnotatedString(annotated.text, newStyles), modifier = Modifier.padding(padding).clickable { uriHandler.openUriCatching(link) }, textAlign = textAlign, lineHeight = 22.sp)
}

@Composable
fun ReadableText(text: String, textAlign: TextAlign = TextAlign.Start, padding: PaddingValues = PaddingValues(bottom = 12.dp)) {
  Text(text, modifier = Modifier.padding(padding), textAlign = textAlign, lineHeight = 22.sp)
}

@Composable
fun ReadableMarkdownText(text: String, textAlign: TextAlign = TextAlign.Start, padding: PaddingValues = PaddingValues(bottom = 12.dp)) {
  MarkdownText(
    text,
    formattedText = remember(text) { parseToMarkdown(text) },
    modifier = Modifier.padding(padding),
    style = TextStyle(textAlign = textAlign, lineHeight = 22.sp, fontSize = 16.sp),
    linkMode = SimplexApp.context.chatModel.controller.appPrefs.simplexLinkMode.get(),
  )
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewHowItWorks() {
  SimpleXTheme {
    HowItWorks(user = null)
  }
}
