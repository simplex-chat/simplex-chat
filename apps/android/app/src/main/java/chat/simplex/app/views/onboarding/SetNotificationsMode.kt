package chat.simplex.app.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.AppBarTitle
import chat.simplex.app.views.usersettings.notificationModes
import chat.simplex.app.views.usersettings.changeNotificationsMode

@Composable
fun SetNotificationsMode(m: ChatModel) {
    Column(
      Modifier
        .fillMaxSize()
        .verticalScroll(rememberScrollState())
        .padding(20.dp)
    ) {
      AppBarTitle(stringResource(R.string.settings_notifications_mode_title), false)
      Spacer(Modifier.padding(DEFAULT_PADDING_HALF))
      val modes = remember { notificationModes() }
      val currentMode = remember { m.controller.appPrefs.notificationsMode.state }
      modes.forEach { mode ->
        TextButton(
          onClick = { changeNotificationsMode(mode.value, m) },
          border = BorderStroke(2.dp, color = if (currentMode.value == mode.value.name) MaterialTheme.colors.primary else HighOrLowlight.copy(alpha = 0.5f)),
          shape = RoundedCornerShape(15.dp),
        ) {
          Column(Modifier.padding(5.dp)) {
            Text(
              mode.title,
              fontWeight = FontWeight.Medium,
              color = if (currentMode.value == mode.value.name) MaterialTheme.colors.primary else HighOrLowlight
            )
            Text(mode.description, color = MaterialTheme.colors.onBackground)
          }
        }
        Spacer(Modifier.height(DEFAULT_PADDING))
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))
      Box(Modifier.fillMaxWidth().padding(bottom = 16.dp), contentAlignment = Alignment.Center) {
        OnboardingActionButton(R.string.use_chat, OnboardingStage.OnboardingComplete, m.onboardingStage) {
          m.controller.showBackgroundServiceNoticeIfNeeded()
        }
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))
    }
}
