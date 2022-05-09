package chat.simplex.app.views.onboarding

import android.content.res.Configuration
import androidx.annotation.StringRes
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.User
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.ModalManager
import chat.simplex.app.views.helpers.annotatedStringResource
import chat.simplex.app.views.usersettings.HelpLayout
import chat.simplex.app.views.usersettings.simplexTeamUri

@Composable
fun HowItWorks(user: User?, onboardingStage: MutableState<OnboardingStage?>? = null) {
  Column(Modifier.fillMaxHeight(), horizontalAlignment = Alignment.Start) {
    Text(stringResource(R.string.how_simplex_works), style = MaterialTheme.typography.h1, modifier = Modifier.padding(bottom = 8.dp))
    ReadableText(R.string.many_people_asked_how_can_it_deliver)
    ReadableText(R.string.to_protect_privacy_simplex_has_ids_for_queues)
    ReadableText(R.string.you_control_servers_to_receive_your_contacts_to_send)
    ReadableText(R.string.only_client_devices_store_contacts_groups_e2e_encrypted_messages)
    if (onboardingStage == null) {
      val uriHandler = LocalUriHandler.current
      Text(
        annotatedStringResource(R.string.read_more_in_github_with_link),
        modifier = Modifier.padding(bottom = 12.dp).clickable { uriHandler.openUri("https://github.com/simplex-chat/simplex-chat#readme") },
        lineHeight = 22.sp
      )
    } else {
      ReadableText(R.string.read_more_in_github)
    }

    Spacer(Modifier.fillMaxHeight().weight(1f))

    if (onboardingStage != null) {
      Box(Modifier.fillMaxWidth().padding(bottom = 16.dp), contentAlignment = Alignment.Center) {
        OnboardingActionButton(user, onboardingStage, onclick = { ModalManager.shared.closeModal() })
      }
    }
  }
}

@Composable
fun ReadableText(@StringRes stringResId: Int) {
  Text(annotatedStringResource(stringResId), modifier = Modifier.padding(bottom = 12.dp), lineHeight = 22.sp)
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