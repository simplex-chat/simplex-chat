package chat.simplex.common.views.onboarding

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
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
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource

@Composable
fun HowItWorks(user: User?, onboardingStage: SharedPreference<OnboardingStage>? = null) {
  Column(Modifier.fillMaxSize().padding(horizontal = if (appPlatform.isDesktop) DEFAULT_PADDING * 2 else DEFAULT_PADDING)) {
    Spacer(Modifier.statusBarsPadding().padding(top = AppBarHeight * fontSizeSqrtMultiplier))
    val paraPadding = PaddingValues(bottom = if (appPlatform.isDesktop) 10.dp else 12.dp)
    Column(Modifier.weight(1f).padding(bottom = DEFAULT_PADDING).verticalScroll(rememberScrollState())) {
      Text(stringResource(MR.strings.why_built_heading), style = MaterialTheme.typography.h1, modifier = Modifier.padding(bottom = DEFAULT_PADDING))
      ReadableText(MR.strings.why_built_p1, padding = paraPadding)
      ReadableText(MR.strings.why_built_p2, padding = paraPadding)
      ReadableText(MR.strings.why_built_p3, padding = paraPadding)
      ReadableText(MR.strings.why_built_p4, padding = paraPadding)
      ReadableText(MR.strings.why_built_p5, padding = paraPadding)
      ReadableText(MR.strings.why_built_p6, padding = paraPadding)
      ReadableText(MR.strings.why_built_p7, padding = paraPadding)
      ReadableText(MR.strings.why_built_tagline, padding = paraPadding)
    }
    if (onboardingStage != null) {
      Column(
        Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally),
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        OnboardingActionButton(user, onboardingStage, onclick = { ModalManager.fullscreen.closeModal() })
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
  Text(AnnotatedString(annotated.text, newStyles), modifier = Modifier.padding(padding).clickable { if (simplexLink) uriHandler.openVerifiedSimplexUri(link) else uriHandler.openExternalLink(link) }, textAlign = textAlign, lineHeight = 22.sp)
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
