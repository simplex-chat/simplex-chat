package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.changeNotificationsMode
import chat.simplex.res.MR

@Composable
fun SetNotificationsMode(m: ChatModel) {
  LaunchedEffect(Unit) {
    prepareChatBeforeFinishingOnboarding()
  }

  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false) {
      ColumnWithScrollBar(Modifier.themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer)) {
        val currentMode = rememberSaveable { mutableStateOf(NotificationsMode.default) }
        
        Column(
          Modifier
            .fillMaxWidth()
            .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING)
            .padding(top = DEFAULT_PADDING),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          Text(
            text = stringResource(MR.strings.onboarding_notifications_mode_title),
            style = MaterialTheme.typography.h1,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colors.onBackground,
            textAlign = TextAlign.Center
          )
          
          Spacer(Modifier.height(DEFAULT_PADDING))
          
          OnboardingInformationButton(
            stringResource(MR.strings.onboarding_notifications_mode_subtitle),
            onClick = { ModalManager.fullscreen.showModalCloseable { NotificationBatteryUsageInfo() } }
          )
        }
        
        Spacer(Modifier.weight(1f))
        
        // Notification options with connecting line
        Column(
          Modifier
            .fillMaxWidth()
            .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          NotificationOptionsWithConnector(
            currentMode = currentMode,
            onModeSelected = { currentMode.value = it }
          )
        }
        
        Spacer(Modifier.weight(1f))
        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          OnboardingActionButton(
            modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier,
            labelId = MR.strings.use_chat,
            onboarding = OnboardingStage.OnboardingComplete,
            onclick = {
              changeNotificationsMode(currentMode.value, m)
              ModalManager.fullscreen.closeModals()
            }
          )
          // Reserve space
          TextButtonBelowOnboardingButton("", null)
        }
      }
    }
  }
  SetNotificationsModeAdditions()
}

@Composable
expect fun SetNotificationsModeAdditions()

@Composable
private fun NotificationOptionsWithConnector(
  currentMode: State<NotificationsMode>,
  onModeSelected: (NotificationsMode) -> Unit
) {
  val options = listOf(
    NotificationsMode.SERVICE to (stringResource(MR.strings.onboarding_notifications_mode_service) to annotatedStringResource(MR.strings.onboarding_notifications_mode_service_desc_short)),
    NotificationsMode.PERIODIC to (stringResource(MR.strings.onboarding_notifications_mode_periodic) to annotatedStringResource(MR.strings.onboarding_notifications_mode_periodic_desc_short)),
    NotificationsMode.OFF to (stringResource(MR.strings.onboarding_notifications_mode_off) to annotatedStringResource(MR.strings.onboarding_notifications_mode_off_desc_short))
  )
  
  val iconResByMode = mapOf(
    NotificationsMode.SERVICE to MR.images.ic_bolt_filled,
    NotificationsMode.PERIODIC to MR.images.ic_schedule,
    NotificationsMode.OFF to MR.images.ic_refresh
  )
  
  Column(
    modifier = Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    options.forEachIndexed { index, (mode, titleDesc) ->
      val (title, description) = titleDesc
      
      SelectableCard(
        currentValue = currentMode,
        newValue = mode,
        title = title,
        description = description,
        onSelected = { onModeSelected(mode) },
        icon = painterResource(iconResByMode[mode]!!)
      )
      
      if (index < options.size - 1) {
        Spacer(Modifier.height(16.dp))
      }
    }
  }
}

@Composable
fun <T> SelectableCard(
  currentValue: State<T>,
  newValue: T,
  title: String,
  description: AnnotatedString,
  onSelected: (T) -> Unit,
  icon: Painter? = null
) {
  val isSelected = currentValue.value == newValue
  val borderColor = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary.copy(alpha = 0.5f)
  val titleColor = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
  val iconTint = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
  
  TextButton(
    onClick = { onSelected(newValue) },
    border = BorderStroke(2.dp, color = borderColor),
    shape = RoundedCornerShape(18.dp),
    modifier = Modifier.fillMaxWidth()
  ) {
    Row(
      Modifier
        .padding(horizontal = 12.dp)
        .padding(vertical = 8.dp)
        .fillMaxWidth(),
      verticalAlignment = Alignment.CenterVertically
    ) {
      if (icon != null) {
        Icon(
          painter = icon,
          contentDescription = null,
          tint = iconTint,
          modifier = Modifier.size(24.dp)
        )
        Spacer(Modifier.width(12.dp))
      }
      Column {
        Text(
          title,
          style = MaterialTheme.typography.h4,
          fontWeight = FontWeight.Bold,
          color = titleColor,
          textAlign = TextAlign.Start
        )
        Text(
          description,
          fontSize = 15.sp,
          color = MaterialTheme.colors.onBackground,
          lineHeight = 20.sp,
          textAlign = TextAlign.Start
        )
      }
    }
  }
}

@Composable
private fun NotificationBatteryUsageInfo() {
  ColumnWithScrollBar(Modifier.padding(DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.onboarding_notifications_mode_battery), withPadding = false)
    Text(stringResource(MR.strings.onboarding_notifications_mode_service), style = MaterialTheme.typography.h3, color = MaterialTheme.colors.secondary)
    ReadableText(MR.strings.onboarding_notifications_mode_service_desc)
    Spacer(Modifier.height(DEFAULT_PADDING_HALF))
    Text(stringResource(MR.strings.onboarding_notifications_mode_periodic), style = MaterialTheme.typography.h3, color = MaterialTheme.colors.secondary)
    ReadableText(MR.strings.onboarding_notifications_mode_periodic_desc)
    Spacer(Modifier.height(DEFAULT_PADDING_HALF))
    Text(stringResource(MR.strings.onboarding_notifications_mode_off), style = MaterialTheme.typography.h3, color = MaterialTheme.colors.secondary)
    ReadableText(MR.strings.onboarding_notifications_mode_off_desc)
  }
}

fun prepareChatBeforeFinishingOnboarding() {
  // No visible users but may have hidden. In this case chat should be started anyway because it's stopped on this stage with hidden users
  if (chatModel.users.any { u -> !u.user.hidden }) return
  withBGApi {
    val user = chatModel.controller.apiGetActiveUser(null) ?: return@withBGApi
    chatModel.currentUser.value = user
    chatModel.controller.startChat(user)
  }
}
