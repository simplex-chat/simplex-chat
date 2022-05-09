package chat.simplex.app.views.onboarding

import android.content.res.Configuration
import androidx.annotation.StringRes
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.model.User
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.ModalManager

@Composable
fun SimpleXInfo(chatModel: ChatModel, onboarding: Boolean = true) {
  SimpleXInfoLayout(
    user = chatModel.currentUser.value,
    onboardingStage = if (onboarding) chatModel.onboardingStage else null,
    showModal = { modalView -> { ModalManager.shared.showModal { modalView(chatModel) } } },
  )
}

@Composable
fun SimpleXInfoLayout(
  user: User?,
  onboardingStage: MutableState<OnboardingStage?>?,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
) {
  Column(Modifier.fillMaxHeight(), horizontalAlignment = Alignment.Start) {
    Image(
      painter = painterResource(R.drawable.logo),
      contentDescription = stringResource(R.string.image_descr_simplex_logo),
      modifier = Modifier.padding(vertical = 15.dp).fillMaxWidth(0.80f)
    )

    InfoRow("🎭", R.string.privacy_redefined, R.string.first_platform_without_user_ids)
    InfoRow("📭", R.string.immune_to_spam_and_abuse, R.string.people_can_connect_only_via_links_you_share)
    InfoRow("🤝", R.string.decentralized, R.string.opensource_protocol_and_code_anybody_can_run_servers)

    Spacer(Modifier.fillMaxHeight().weight(1f))

    if (onboardingStage != null) {
      Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
        OnboardingActionButton(user, onboardingStage)
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))
    }

    Box(Modifier.fillMaxWidth().padding(bottom = 16.dp), contentAlignment = Alignment.Center) {
      SimpleButton(text = stringResource(R.string.how_it_works), icon = Icons.Outlined.Info) {
        showModal { m -> HowItWorks(user, onboardingStage) }
      }
    }
  }
}

@Composable
private fun InfoRow(emoji: String, @StringRes titleId: Int, @StringRes textId: Int) {
  Row(Modifier.padding(bottom = 16.dp), verticalAlignment = Alignment.Top) {
    Text(emoji, fontSize = 36.sp, modifier = Modifier.width(60.dp).padding(end = 16.dp))
    Column(horizontalAlignment = Alignment.Start) {
      Text(stringResource(titleId), fontWeight = FontWeight.Bold)
      Text(stringResource(textId), lineHeight = 22.sp)
    }
  }
}


@Composable
fun OnboardingActionButton(user: User?, onboardingStage: MutableState<OnboardingStage?>) {
  if (user == null) {
    ActionButton(stringResource(R.string.create_your_profile), onboarding = OnboardingStage.Step2_CreateProfile, onboardingStage)
  } else {
    ActionButton(stringResource(R.string.make_private_connection), onboarding = OnboardingStage.Step3_MakeConnection, onboardingStage)
  }
}

@Composable
private fun ActionButton(label: String, onboarding: OnboardingStage?, onboardingStage: MutableState<OnboardingStage?>) {
  SimpleButtonFrame(click = { onboardingStage.value = onboarding }) {
    Text(label, style = MaterialTheme.typography.caption, color = MaterialTheme.colors.primary)
    Icon(
      Icons.Outlined.ArrowForwardIos, "next stage", tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(end = 8.dp)
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSimpleXInfo() {
  SimpleXTheme {
    SimpleXInfoLayout(
      user = null,
      onboardingStage = null,
      showModal = {{}}
    )
  }
}
