package chat.simplex.app.views.onboarding

import androidx.annotation.StringRes
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.NotificationsMode
import chat.simplex.app.views.usersettings.changeNotificationsMode

@Composable
fun SetNotificationsMode(m: ChatModel) {
    Column(
      Modifier
        .fillMaxSize()
        .verticalScroll(rememberScrollState())
        .padding(20.dp)
    ) {
      AppBarTitle(stringResource(R.string.onboarding_notifications_mode_title), false)
      val currentMode = rememberSaveable { mutableStateOf(NotificationsMode.default) }
      Text(stringResource(R.string.onboarding_notifications_mode_subtitle))
      Spacer(Modifier.padding(DEFAULT_PADDING_HALF))
      NotificationButton(currentMode, NotificationsMode.OFF, R.string.onboarding_notifications_mode_off, R.string.onboarding_notifications_mode_off_desc)
      NotificationButton(currentMode, NotificationsMode.PERIODIC, R.string.onboarding_notifications_mode_periodic, R.string.onboarding_notifications_mode_periodic_desc)
      NotificationButton(currentMode, NotificationsMode.SERVICE, R.string.onboarding_notifications_mode_service, R.string.onboarding_notifications_mode_service_desc)
      Spacer(Modifier.fillMaxHeight().weight(1f))
      Box(Modifier.fillMaxWidth().padding(bottom = 16.dp), contentAlignment = Alignment.Center) {
        OnboardingActionButton(R.string.use_chat, OnboardingStage.OnboardingComplete, m.onboardingStage) {
          changeNotificationsMode(currentMode.value, m)
        }
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))
    }
}

@Composable
private fun NotificationButton(currentMode: MutableState<NotificationsMode>, mode: NotificationsMode, @StringRes title: Int, @StringRes description: Int) {
  TextButton(
    onClick = { currentMode.value = mode },
    border = BorderStroke(1.dp, color = if (currentMode.value == mode) MaterialTheme.colors.primary else HighOrLowlight.copy(alpha = 0.5f)),
    shape = RoundedCornerShape(15.dp),
  ) {
    Column(Modifier.padding(bottom = 6.dp).padding(horizontal = 8.dp)) {
      Text(
        stringResource(title),
        style = MaterialTheme.typography.h2,
        fontWeight = FontWeight.Medium,
        color = if (currentMode.value == mode) MaterialTheme.colors.primary else HighOrLowlight,
        modifier = Modifier.padding(bottom = 4.dp)
      )
      Text(annotatedStringResource(description), color = MaterialTheme.colors.onBackground, lineHeight = 24.sp)
    }
  }
  Spacer(Modifier.height(DEFAULT_PADDING))
}
