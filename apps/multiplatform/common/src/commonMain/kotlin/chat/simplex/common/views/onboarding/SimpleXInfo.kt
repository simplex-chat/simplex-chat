package chat.simplex.common.views.onboarding

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.ContentScale
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.MigrateToDeviceView
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource

@Composable
fun SimpleXInfo(chatModel: ChatModel, onboarding: Boolean = true) {
  if (onboarding) {
    ModalView({}, showClose = false, endButtons = {
      IconButton({ ModalManager.fullscreen.showModal { HowItWorks(chatModel.currentUser.value, null) }}) {
        Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.primary)
      }
    }) {
      SimpleXInfoLayout(
        user = chatModel.currentUser.value,
        onboardingStage = chatModel.controller.appPrefs.onboardingStage
      )
    }
  } else {
    SimpleXInfoLayout(
      user = chatModel.currentUser.value,
      onboardingStage = null
    )
  }
}

@Composable
fun SimpleXInfoLayout(
  user: User?,
  onboardingStage: SharedPreference<OnboardingStage>?
) {
  ColumnWithScrollBar(
    Modifier
      .fillMaxSize()
      .padding(start = DEFAULT_PADDING * 2, end = DEFAULT_PADDING * 2),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Box(Modifier.widthIn(max = if (appPlatform.isAndroid) 250.dp else 500.dp).padding(top = DEFAULT_PADDING + 8.dp, bottom = 30.dp), contentAlignment = Alignment.Center) {
      SimpleXLogo()
    }

    Text(
      stringResource(MR.strings.next_generation_of_private_messaging),
      style = MaterialTheme.typography.h3,
      color = MaterialTheme.colors.secondary,
      modifier = Modifier.padding(bottom = 68.dp),
      textAlign = TextAlign.Center
    )

    Column {
      InfoRow(painterResource(MR.images.privacy), MR.strings.privacy_redefined, MR.strings.first_platform_without_user_ids, width = 60.dp)
      InfoRow(painterResource(MR.images.shield), MR.strings.immune_to_spam_and_abuse, MR.strings.people_can_connect_only_via_links_you_share, width = 46.dp)
      InfoRow(painterResource(if (isInDarkTheme()) MR.images.decentralized_light else MR.images.decentralized), MR.strings.decentralized, MR.strings.opensource_protocol_and_code_anybody_can_run_servers)
    }

    Spacer(Modifier.fillMaxHeight().weight(1f))

    if (onboardingStage != null) {
      Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
        OnboardingActionButton(user, onboardingStage)
        TextButtonBelowOnboardingButton(stringResource(MR.strings.migrate_from_another_device)) {
          chatModel.migrationState.value = MigrationToState.PasteOrScanLink
          ModalManager.fullscreen.showCustomModal { close -> MigrateToDeviceView(close) }
        }
      }
    }
    Spacer(Modifier.height(DEFAULT_PADDING))
  }
  LaunchedEffect(Unit) {
    if (chatModel.migrationState.value != null && !ModalManager.fullscreen.hasModalsOpen()) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close -> MigrateToDeviceView(close) }
    }
  }
}

@Composable
fun SimpleXLogo() {
  Image(
    painter = painterResource(if (isInDarkTheme()) MR.images.logo_light else MR.images.logo),
    contentDescription = stringResource(MR.strings.image_descr_simplex_logo),
    contentScale = ContentScale.FillWidth,
    modifier = Modifier
      .padding(vertical = DEFAULT_PADDING)
      .fillMaxWidth()
  )
}

@Composable
private fun InfoRow(icon: Painter, titleId: StringResource, textId: StringResource, width: Dp = 58.dp) {
  Row(Modifier.padding(bottom = 27.dp), verticalAlignment = Alignment.Top) {
    Spacer(Modifier.width((18.dp + 58.dp - width) / 2))
    Image(icon, contentDescription = null, modifier = Modifier
      .width(width))
    Spacer(Modifier.width((18.dp + 58.dp - width) / 2 + DEFAULT_PADDING_HALF))
    Column(Modifier.padding(top = 4.dp), verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF)) {
      Text(stringResource(titleId), fontWeight = FontWeight.Bold, style = MaterialTheme.typography.h3, lineHeight = 24.sp)
      Text(stringResource(textId), lineHeight = 24.sp, style = MaterialTheme.typography.body1, color = MaterialTheme.colors.secondary)
    }
  }
}

@Composable
expect fun OnboardingActionButton(user: User?, onboardingStage: SharedPreference<OnboardingStage>, onclick: (() -> Unit)? = null)

@Composable
fun OnboardingActionButton(
  modifier: Modifier = Modifier,
  labelId: StringResource,
  onboarding: OnboardingStage?,
  enabled: Boolean = true,
  icon: Painter? = null,
  iconColor: Color = Color.White,
  onclick: (() -> Unit)?
) {
  Button(
    onClick = {
      onclick?.invoke()
      if (onboarding != null) {
        appPrefs.onboardingStage.set(onboarding)
      }
    },
    modifier = modifier,
    shape = CircleShape,
    enabled = enabled,
//    elevation = ButtonDefaults.elevation(defaultElevation = 0.dp, focusedElevation = 0.dp, pressedElevation = 0.dp, hoveredElevation = 0.dp),
    contentPadding = PaddingValues(horizontal = if (icon == null) DEFAULT_PADDING * 2 else DEFAULT_PADDING * 1.5f, vertical = DEFAULT_PADDING),
    colors = ButtonDefaults.buttonColors(MaterialTheme.colors.primary, disabledBackgroundColor = MaterialTheme.colors.secondary)
  ) {
    if (icon != null) {
      Icon(icon, stringResource(labelId), Modifier.padding(end = DEFAULT_PADDING_HALF), tint = iconColor)
    }
    Text(stringResource(labelId), style = MaterialTheme.typography.h2, color = Color.White, fontSize = 18.sp, fontWeight = FontWeight.Medium)
  }
}

@Composable
fun TextButtonBelowOnboardingButton(text: String, onClick: (() -> Unit)?) {
  TextButton({ onClick?.invoke() }, Modifier.padding(top = DEFAULT_PADDING_HALF).clip(CircleShape), enabled = onClick != null) {
    Text(
      text,
      Modifier.padding(start = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING_HALF, bottom = 5.dp),
      color = MaterialTheme.colors.primary,
      fontWeight = FontWeight.Medium,
      textAlign = TextAlign.Center
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
      onboardingStage = null
    )
  }
}
