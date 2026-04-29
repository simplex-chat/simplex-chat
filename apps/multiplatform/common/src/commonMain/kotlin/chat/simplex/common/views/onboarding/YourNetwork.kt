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

  if (appPlatform.isDesktop) {
    YourNetworkDesktop(serverOperators, selectedOperatorIds)
  } else {
    CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
      ModalView({}, showClose = false, showAppBar = false) {
        OnboardingShrinkingLayout(
          modifier = Modifier.fillMaxSize().themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer)
            .systemBarsPadding()
            .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING),
          topPadding = DEFAULT_PADDING,
          image = {
            Column(Modifier.padding(vertical = DEFAULT_PADDING_HALF), horizontalAlignment = Alignment.CenterHorizontally) {
              OnboardingImage(
                MR.images.your_network, MR.images.your_network_light, MR.images.ic_dns,
                modifier = Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth()
              )
            }
          },
          content = {
            Column(horizontalAlignment = Alignment.CenterHorizontally) {
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
                lineHeight = 25.sp,
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
                ConfigureNotificationsButton(notificationMode) {
                  ModalManager.fullscreen.showModalCloseable { close ->
                    SetNotificationsMode(notificationMode, close)
                  }
                }
              }
            }
          },
          button = {
            Column(
              Modifier.widthIn(max = 450.dp).padding(bottom = DEFAULT_PADDING * 2),
              horizontalAlignment = Alignment.CenterHorizontally
            ) {
              OnboardingActionButton(
                modifier = Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth(),
                labelId = MR.strings.onboarding_network_operators_continue,
                onboarding = null,
                onclick = {
                  changeNotificationsMode(notificationMode.value, chatModel)
                  appPrefs.onboardingStage.set(OnboardingStage.Step4_NetworkCommitments)
                }
              )
            }
          }
        )
      }
    }
  }
}

@Composable
private fun YourNetworkDesktop(
  serverOperators: State<List<ServerOperator>>,
  selectedOperatorIds: MutableState<Set<Long>>
) {
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false) {
      ColumnWithScrollBar(horizontalAlignment = Alignment.CenterHorizontally) {
        Column(Modifier.widthIn(max = 600.dp).fillMaxHeight().padding(horizontal = DEFAULT_PADDING).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          Box(Modifier.align(Alignment.CenterHorizontally)) {
            AppBarTitle(stringResource(MR.strings.onboarding_your_network), bottomPadding = DEFAULT_PADDING, withPadding = false, overrideTitleColor = MaterialTheme.colors.onBackground, textAlign = TextAlign.Center, lineHeight = 42.sp)
          }
          Text(stringResource(MR.strings.onboarding_network_routers_cannot_know), style = MaterialTheme.typography.h3, fontWeight = FontWeight.Medium, color = MaterialTheme.colors.secondary, lineHeight = 25.sp, textAlign = TextAlign.Center)
          Spacer(Modifier.height(DEFAULT_PADDING))
          ConfigureRoutersButton(serverOperators, selectedOperatorIds) {
            ModalManager.fullscreen.showCustomModal(forceAnimated = true) { close ->
              ChooseServerOperators(serverOperators, selectedOperatorIds, close)
            }
          }
        }
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Column(Modifier.widthIn(max = 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          OnboardingActionButton(
            Modifier.widthIn(min = 300.dp),
            labelId = MR.strings.onboarding_network_operators_continue,
            onboarding = null,
            onclick = {
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
