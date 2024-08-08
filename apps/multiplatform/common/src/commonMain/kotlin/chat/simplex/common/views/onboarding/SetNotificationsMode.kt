package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.changeNotificationsMode
import chat.simplex.res.MR

@Composable
fun SetNotificationsMode(m: ChatModel) {
  ColumnWithScrollBar(
    modifier = Modifier
      .fillMaxSize()
  ) {
    CloseSheetBar(showClose = false, close = {})
    AppBarTitle(stringResource(MR.strings.onboarding_notifications_mode_title))
    val currentMode = rememberSaveable { mutableStateOf(NotificationsMode.default) }
    Column(Modifier.padding(horizontal = DEFAULT_PADDING * 1f)) {
      Text(stringResource(MR.strings.onboarding_notifications_mode_subtitle), Modifier.fillMaxWidth(), textAlign = TextAlign.Center)
      Spacer(Modifier.height(DEFAULT_PADDING * 2f))
      SelectableCard(currentMode, NotificationsMode.OFF, stringResource(MR.strings.onboarding_notifications_mode_off), annotatedStringResource(MR.strings.onboarding_notifications_mode_off_desc)) {
        currentMode.value = NotificationsMode.OFF
      }
      SelectableCard(currentMode, NotificationsMode.PERIODIC, stringResource(MR.strings.onboarding_notifications_mode_periodic), annotatedStringResource(MR.strings.onboarding_notifications_mode_periodic_desc)){
        currentMode.value = NotificationsMode.PERIODIC
      }
      SelectableCard(currentMode, NotificationsMode.SERVICE, stringResource(MR.strings.onboarding_notifications_mode_service), annotatedStringResource(MR.strings.onboarding_notifications_mode_service_desc)){
        currentMode.value = NotificationsMode.SERVICE
      }
    }
    Spacer(Modifier.fillMaxHeight().weight(1f))
    Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
      OnboardingActionButton(
        modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier,
        labelId = MR.strings.use_chat,
        onboarding = OnboardingStage.OnboardingComplete,
        onclick = {
          changeNotificationsMode(currentMode.value, m)
        }
      )
      // Reserve space
      TextButtonBelowOnboardingButton("", null)
    }
  }
  SetNotificationsModeAdditions()
}

@Composable
expect fun SetNotificationsModeAdditions()

@Composable
fun <T> SelectableCard(currentValue: State<T>, newValue: T, title: String, description: AnnotatedString, onSelected: (T) -> Unit) {
  TextButton(
    onClick = { onSelected(newValue) },
    border = BorderStroke(1.dp, color = if (currentValue.value == newValue) MaterialTheme.colors.primary else MaterialTheme.colors.secondary.copy(alpha = 0.5f)),
    shape = RoundedCornerShape(35.dp),
  ) {
    Column(Modifier.padding(horizontal = 10.dp).padding(top = 4.dp, bottom = 8.dp).fillMaxWidth()) {
      Text(
        title,
        style = MaterialTheme.typography.h3,
        fontWeight = FontWeight.Medium,
        color = if (currentValue.value == newValue) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
        modifier = Modifier.padding(bottom = 8.dp).align(Alignment.CenterHorizontally),
        textAlign = TextAlign.Center
      )
      Text(description,
        Modifier.align(Alignment.CenterHorizontally),
        fontSize = 15.sp,
        color = MaterialTheme.colors.onBackground,
        lineHeight = 24.sp,
        textAlign = TextAlign.Center
      )
    }
  }
  Spacer(Modifier.height(14.dp))
}
