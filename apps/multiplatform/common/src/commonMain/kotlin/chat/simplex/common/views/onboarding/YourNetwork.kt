package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.ColorFilter
import androidx.compose.ui.graphics.ColorMatrix
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.gradientPoints
import chat.simplex.common.views.newchat.lightStops
import chat.simplex.common.views.usersettings.changeNotificationsMode
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

internal object OnboardingSharedState {
  var selectedOperatorIds: Set<Long> = emptySet()
}

@Composable
fun YourNetworkView(chatModel: ChatModel) {
  LaunchedEffect(Unit) {
    prepareChatBeforeFinishingOnboarding()
  }

  val serverOperators = remember { derivedStateOf { chatModel.conditions.value.serverOperators } }
  val selectedOperatorIds = remember {
    mutableStateOf(serverOperators.value.filter { it.enabled }.map { it.operatorId }.toSet())
  }

  LaunchedEffect(selectedOperatorIds.value) {
    OnboardingSharedState.selectedOperatorIds = selectedOperatorIds.value
  }

  val notificationMode = rememberSaveable { mutableStateOf(NotificationsMode.default) }

  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false, showAppBar = false) {
      ColumnWithScrollBar(
        Modifier.themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer),
        horizontalAlignment = Alignment.CenterHorizontally,
        maxIntrinsicSize = true
      ) {
        Spacer(Modifier.weight(1f))

        if (BuildConfigCommon.SIMPLEX_ASSETS) {
          Image(
            painterResource(if (isInDarkTheme()) MR.images.your_network_light else MR.images.your_network),
            contentDescription = null,
            contentScale = ContentScale.Fit,
            modifier = Modifier.fillMaxWidth().padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING * 2)
              .then(if (!appPlatform.isAndroid) Modifier.heightIn(max = 280.dp) else Modifier)
          )
        } else {
          val isDark = isInDarkTheme()
          val stops = if (isDark) darkStops else lightStops
          val scale = if (isDark) 1.5f else 1.2f
          Box(
            Modifier
              .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING * 2)
              .then(if (appPlatform.isAndroid) Modifier.fillMaxWidth() else Modifier.heightIn(max = 280.dp))
              .aspectRatio(1f)
              .clip(RoundedCornerShape(24.dp))
              .drawBehind {
                val gp = gradientPoints(size.height / size.width, scale)
                drawRect(
                  Brush.linearGradient(
                    colorStops = stops,
                    start = Offset(gp.startX * size.width, gp.startY * size.height),
                    end = Offset(gp.endX * size.width, gp.endY * size.height)
                  )
                )
              },
            contentAlignment = Alignment.Center
          ) {
            Icon(
              painterResource(MR.images.ic_dns),
              contentDescription = null,
              modifier = Modifier.size(80.dp),
              tint = MaterialTheme.colors.primary
            )
          }
        }

        Text(
          stringResource(MR.strings.onboarding_your_network),
          style = MaterialTheme.typography.h1,
          fontWeight = FontWeight.Bold,
          textAlign = TextAlign.Center,
          lineHeight = 42.sp,
          modifier = Modifier.padding(top = DEFAULT_PADDING_HALF)
        )

        Text(
          stringResource(MR.strings.onboarding_network_routers_cannot_know),
          style = MaterialTheme.typography.h3,
          fontWeight = FontWeight.Medium,
          color = MaterialTheme.colors.secondary,
          fontSize = 20.sp,
          lineHeight = 30.sp,
          textAlign = TextAlign.Center,
          modifier = Modifier.padding(top = 14.dp)
        )

        Column(
          Modifier.padding(top = DEFAULT_PADDING_HALF),
          horizontalAlignment = Alignment.Start,
          verticalArrangement = Arrangement.spacedBy(4.dp)
        ) {
          ConfigureRoutersButton(serverOperators, selectedOperatorIds) {
            ModalManager.fullscreen.showCustomModal { close ->
              ChooseServerOperators(serverOperators, selectedOperatorIds, close)
            }
          }

          if (appPlatform.isAndroid) {
            ConfigureNotificationsButton(notificationMode) {
              ModalManager.fullscreen.showModalCloseable { close ->
                NotificationModeSheet(notificationMode, close)
              }
            }
          }
        }

        Spacer(Modifier.weight(1f))

        Column(
          Modifier
            .widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp)
            .align(Alignment.CenterHorizontally),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          OnboardingActionButton(
            modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
            labelId = MR.strings.onboarding_network_operators_continue,
            onboarding = null,
            onclick = {
              if (appPlatform.isAndroid) {
                changeNotificationsMode(notificationMode.value, chatModel)
              }
              appPrefs.onboardingStage.set(OnboardingStage.Step4_NetworkCommitments)
            }
          )
          TextButtonBelowOnboardingButton("", null)
        }
      }
    }
  }
}

@Composable
private fun ConfigureRoutersButton(serverOperators: State<List<ServerOperator>>, selectedOperatorIds: State<Set<Long>>, onClick: () -> Unit) {
  Box(
    modifier = Modifier
      .clip(CircleShape)
      .clickable { onClick() }
  ) {
    Row(Modifier.padding(8.dp), horizontalArrangement = Arrangement.spacedBy(4.dp), verticalAlignment = Alignment.CenterVertically) {
      Text(
        stringResource(MR.strings.onboarding_configure_routers),
        style = MaterialTheme.typography.button,
        fontWeight = FontWeight.Medium,
        color = MaterialTheme.colors.primary
      )
      serverOperators.value.forEach { op ->
        Image(
          painterResource(op.logo),
          contentDescription = null,
          modifier = Modifier.size(22.dp),
          colorFilter = if (selectedOperatorIds.value.contains(op.operatorId)) null else ColorFilter.colorMatrix(ColorMatrix().apply {
            setToSaturation(0f)
          })
        )
      }
    }
  }
}

@Composable
private fun ConfigureNotificationsButton(notificationMode: State<NotificationsMode>, onClick: () -> Unit) {
  val icon = when (notificationMode.value) {
    NotificationsMode.SERVICE -> MR.images.ic_bolt
    NotificationsMode.PERIODIC -> MR.images.ic_timer
    NotificationsMode.OFF -> MR.images.ic_bolt_off
  }
  Box(
    modifier = Modifier
      .clip(CircleShape)
      .clickable { onClick() }
  ) {
    Row(Modifier.padding(8.dp), horizontalArrangement = Arrangement.spacedBy(4.dp), verticalAlignment = Alignment.CenterVertically) {
      Text(
        stringResource(MR.strings.onboarding_configure_notifications),
        style = MaterialTheme.typography.button,
        fontWeight = FontWeight.Medium,
        color = MaterialTheme.colors.primary
      )
      Icon(
        painterResource(icon),
        contentDescription = null,
        tint = MaterialTheme.colors.primary
      )
    }
  }
}

@Composable
private fun NotificationModeSheet(currentMode: MutableState<NotificationsMode>, close: () -> Unit) {
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false) {
      ColumnWithScrollBar(Modifier.themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer)) {
        Box(Modifier.align(Alignment.CenterHorizontally)) {
          AppBarTitle(stringResource(MR.strings.onboarding_notifications_mode_title), bottomPadding = DEFAULT_PADDING)
        }
        Column(Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth(), horizontalAlignment = Alignment.CenterHorizontally) {
          OnboardingInformationButton(
            stringResource(MR.strings.onboarding_notifications_mode_subtitle),
            onClick = { ModalManager.fullscreen.showModalCloseable { NotificationBatteryUsageInfo() } }
          )
        }
        Spacer(Modifier.weight(1f))
        Column(Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING)) {
          SelectableCard(currentMode, NotificationsMode.SERVICE, stringResource(MR.strings.onboarding_notifications_mode_service), annotatedStringResource(MR.strings.onboarding_notifications_mode_service_desc_short)) {
            currentMode.value = NotificationsMode.SERVICE
          }
          SelectableCard(currentMode, NotificationsMode.PERIODIC, stringResource(MR.strings.onboarding_notifications_mode_periodic), annotatedStringResource(MR.strings.onboarding_notifications_mode_periodic_desc_short)) {
            currentMode.value = NotificationsMode.PERIODIC
          }
          SelectableCard(currentMode, NotificationsMode.OFF, stringResource(MR.strings.onboarding_notifications_mode_off), annotatedStringResource(MR.strings.onboarding_notifications_mode_off_desc_short)) {
            currentMode.value = NotificationsMode.OFF
          }
        }
        Spacer(Modifier.weight(1f))
        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          OnboardingActionButton(
            modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier,
            labelId = MR.strings.ok,
            onboarding = null,
            onclick = { close() }
          )
          TextButtonBelowOnboardingButton("", null)
        }
      }
    }
  }
}
