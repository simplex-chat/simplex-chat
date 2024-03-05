package chat.simplex.common.views.onboarding

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.MigrateFromAnotherDeviceView
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource

@Composable
fun SimpleXInfo(chatModel: ChatModel, onboarding: Boolean = true) {
  SimpleXInfoLayout(
    user = chatModel.currentUser.value,
    onboardingStage = if (onboarding) chatModel.controller.appPrefs.onboardingStage else null,
    showModal = { modalView -> { if (onboarding) ModalManager.fullscreen.showModal { modalView(chatModel) } else ModalManager.start.showModal { modalView(chatModel) } } },
  )
}

@Composable
fun SimpleXInfoLayout(
  user: User?,
  onboardingStage: SharedPreference<OnboardingStage>?,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
) {
  Column(
    Modifier
      .fillMaxSize()
      .verticalScroll(rememberScrollState())
      .padding(start = DEFAULT_PADDING , end = DEFAULT_PADDING, top = DEFAULT_PADDING),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Box(Modifier.fillMaxWidth().padding(top = 8.dp, bottom = 10.dp), contentAlignment = Alignment.Center) {
      SimpleXLogo()
    }

    Text(stringResource(MR.strings.next_generation_of_private_messaging), style = MaterialTheme.typography.h2, modifier = Modifier.padding(bottom = 48.dp).padding(horizontal = 36.dp), textAlign = TextAlign.Center)

    Column {
      InfoRow(painterResource(MR.images.privacy), MR.strings.privacy_redefined, MR.strings.first_platform_without_user_ids, width = 80.dp)
      InfoRow(painterResource(MR.images.shield), MR.strings.immune_to_spam_and_abuse, MR.strings.people_can_connect_only_via_links_you_share)
      InfoRow(painterResource(if (isInDarkTheme()) MR.images.decentralized_light else MR.images.decentralized), MR.strings.decentralized, MR.strings.opensource_protocol_and_code_anybody_can_run_servers)
    }

    Spacer(Modifier.fillMaxHeight().weight(1f))

    if (onboardingStage != null) {
      Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
        OnboardingActionButton(user, onboardingStage)
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))

      Box(
        Modifier
          .fillMaxWidth()
          .padding(bottom = DEFAULT_PADDING.times(1.5f), top = DEFAULT_PADDING), contentAlignment = Alignment.Center
      ) {
        SimpleButtonDecorated(text = stringResource(MR.strings.migrate_from_another_device), icon = painterResource(MR.images.ic_download),
          click = { ModalManager.fullscreen.showCustomModal { close -> MigrateFromAnotherDeviceView(chatModel.migrationState.value, close) } })
      }
    }

    Box(
      Modifier
        .fillMaxWidth()
        .padding(bottom = DEFAULT_PADDING.times(1.5f), top = DEFAULT_PADDING), contentAlignment = Alignment.Center
    ) {
      SimpleButtonDecorated(text = stringResource(MR.strings.how_it_works), icon = painterResource(MR.images.ic_info),
        click = showModal { HowItWorks(user, onboardingStage) })
    }
  }
  LaunchedEffect(Unit) {
    if (chatModel.migrationState.value != null && !ModalManager.fullscreen.hasModalsOpen()) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close -> MigrateFromAnotherDeviceView(chatModel.migrationState.value, close) }
    }
  }
}

@Composable
fun SimpleXLogo() {
  Image(
    painter = painterResource(if (isInDarkTheme()) MR.images.logo_light else MR.images.logo),
    contentDescription = stringResource(MR.strings.image_descr_simplex_logo),
    modifier = Modifier
      .padding(vertical = DEFAULT_PADDING)
      .fillMaxWidth(0.60f)
  )
}

@Composable
private fun InfoRow(icon: Painter, titleId: StringResource, textId: StringResource, width: Dp = 76.dp) {
  Row(Modifier.padding(bottom = 27.dp), verticalAlignment = Alignment.Top) {
    Image(icon, contentDescription = null, modifier = Modifier
      .width(width)
      .padding(top = 8.dp, start = 8.dp, end = 24.dp))
    Column {
      Text(stringResource(titleId), fontWeight = FontWeight.Bold, style = MaterialTheme.typography.h3, lineHeight = 24.sp)
      Text(stringResource(textId), lineHeight = 24.sp, style = MaterialTheme.typography.body1)
    }
  }
}

@Composable
expect fun OnboardingActionButton(user: User?, onboardingStage: SharedPreference<OnboardingStage>, onclick: (() -> Unit)? = null)

@Composable
fun OnboardingActionButton(
  labelId: StringResource,
  onboarding: OnboardingStage?,
  border: Boolean,
  icon: Painter? = null,
  iconColor: Color = MaterialTheme.colors.primary,
  onclick: (() -> Unit)?
) {
  val modifier = if (border) {
    Modifier
      .border(border = BorderStroke(1.dp, MaterialTheme.colors.primary), shape = RoundedCornerShape(50))
      .padding(
      horizontal = if (icon == null) DEFAULT_PADDING * 2 else DEFAULT_PADDING_HALF,
      vertical = 4.dp
    )
  } else {
    Modifier
  }

  SimpleButtonFrame(click = {
    onclick?.invoke()
    if (onboarding != null) {
      ChatController.appPrefs.onboardingStage.set(onboarding)
    }
  }, modifier) {
    if (icon != null) {
      Icon(icon, stringResource(labelId), Modifier.padding(end = DEFAULT_PADDING_HALF), tint = iconColor)
    }
    Text(stringResource(labelId), style = MaterialTheme.typography.h2, color = MaterialTheme.colors.primary, fontSize = 20.sp)
    Icon(
      painterResource(MR.images.ic_arrow_forward_ios), "next stage", tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(start = DEFAULT_PADDING.div(4)).size(20.dp)
    )
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewSimpleXInfo() {
  SimpleXTheme {
    SimpleXInfoLayout(
      user = null,
      onboardingStage = null,
      showModal = { {} }
    )
  }
}
