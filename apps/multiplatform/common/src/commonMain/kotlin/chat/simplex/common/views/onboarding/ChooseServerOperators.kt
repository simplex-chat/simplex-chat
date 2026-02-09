package chat.simplex.common.views.onboarding

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import TextIconSpaced
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.TextStyle
 import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.foundation.Image
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.material.Icon
import chat.simplex.common.views.helpers.BoltFilled
import chat.simplex.common.views.onboarding.SetNotificationsMode

@Composable
fun ModalData.OnboardingConditionsView() {
  LaunchedEffect(Unit) {
    prepareChatBeforeFinishingOnboarding()
  }
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false) {
      val serverOperators = remember { derivedStateOf { chatModel.conditions.value.serverOperators } }
      val selectedOperatorIds = remember { stateGetOrPut("selectedOperatorIds") { serverOperators.value.filter { it.enabled }.map { it.operatorId }.toSet() } }

      ColumnWithScrollBar(
        Modifier
          .themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer),
        maxIntrinsicSize = true
      ) {
        Column(
          (if (appPlatform.isDesktop) Modifier.width(450.dp).align(Alignment.CenterHorizontally) else Modifier)
          .fillMaxWidth()
          .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          // Isometric illustration (conditional on USE_BRANDED_IMAGES)
          if (BuildConfigCommon.USE_BRANDED_IMAGES) {
            Spacer(Modifier.height(DEFAULT_PADDING * 2))
            Box(
              modifier = Modifier
                .fillMaxWidth()
                .padding(vertical = DEFAULT_PADDING),
              contentAlignment = Alignment.Center
            ) {
              Image(
                painter = painterResource(MR.images.intro_2),
                contentDescription = null,
                modifier = Modifier.size(200.dp),
                contentScale = ContentScale.Fit
              )
            }
            Spacer(Modifier.height(DEFAULT_PADDING))
          }
          
          // Title: "Conditions of use" (centered, bold)
          Text(
            text = stringResource(MR.strings.conditions_of_use_title),
            style = MaterialTheme.typography.h1,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colors.onBackground,
            textAlign = TextAlign.Center,
            modifier = Modifier.fillMaxWidth()
          )
          
          Spacer(Modifier.height(DEFAULT_PADDING * 2))
        }

        Spacer(Modifier.weight(1f))
        
        Column(
          (if (appPlatform.isDesktop) Modifier.width(450.dp).align(Alignment.CenterHorizontally) else Modifier)
          .fillMaxWidth()
          .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING),
          horizontalAlignment = Alignment.Start
        ) {
          // Body text (left-aligned)
          Text(
            stringResource(MR.strings.onboarding_conditions_private_chats_not_accessible),
            style = TextStyle(fontSize = 17.sp, lineHeight = 23.sp),
            color = MaterialTheme.colors.onBackground
          )
          Spacer(Modifier.height(DEFAULT_PADDING))
          Text(
            "By using SimpleX Chat you agree to:",
            style = TextStyle(fontSize = 17.sp, lineHeight = 23.sp),
            color = MaterialTheme.colors.onBackground
          )
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))
          Text(
            "• send only legal content in public groups.",
            style = TextStyle(fontSize = 17.sp, lineHeight = 23.sp),
            color = MaterialTheme.colors.onBackground,
            modifier = Modifier.padding(start = DEFAULT_PADDING)
          )
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))
          Text(
            "• respect other users – no spam.",
            style = TextStyle(fontSize = 17.sp, lineHeight = 23.sp),
            color = MaterialTheme.colors.onBackground,
            modifier = Modifier.padding(start = DEFAULT_PADDING)
          )
          Spacer(Modifier.height(DEFAULT_PADDING))
          // Privacy policy link (blue, underlined)
          Text(
            stringResource(MR.strings.onboarding_conditions_privacy_policy_and_conditions_of_use),
            style = TextStyle(fontSize = 17.sp, textDecoration = TextDecoration.Underline),
            color = MaterialTheme.colors.primary,
            modifier = Modifier
              .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null
              ) {
                ModalManager.fullscreen.showModal(endButtons = { ConditionsLinkButton() }) {
                  SimpleConditionsView(rhId = null)
                }
              }
          )
        }
        Spacer(Modifier.weight(1f))

        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          AcceptConditionsButton(enabled = selectedOperatorIds.value.isNotEmpty(), selectedOperatorIds)
          
          // Configure server operators link with icon
          Row(
            modifier = Modifier
              .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null
              ) {
                ModalManager.fullscreen.showModalCloseable { close ->
                  ChooseServerOperators(serverOperators, selectedOperatorIds, close)
                }
              }
              .padding(vertical = DEFAULT_PADDING_HALF),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.Center
          ) {
            Text(
              text = stringResource(MR.strings.onboarding_conditions_configure_server_operators),
              color = MaterialTheme.colors.primary,
              fontSize = 16.sp
            )
            Spacer(Modifier.width(8.dp))
            // Server operator icon - using network icon if available, otherwise a simple circle
            Icon(
              painterResource(MR.images.ic_info),
              contentDescription = null,
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier.size(16.dp)
            )
          }
          
          // Configure notifications link with icon
          Row(
            modifier = Modifier
              .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null
              ) {
                ModalManager.fullscreen.showModalCloseable { close ->
                  SetNotificationsMode(chatModel)
                }
              }
              .padding(vertical = DEFAULT_PADDING_HALF),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.Center
          ) {
            Text(
              text = stringResource(MR.strings.configure_notifications),
              color = MaterialTheme.colors.primary,
              fontSize = 16.sp
            )
            Spacer(Modifier.width(8.dp))
            // Notification icon - using bolt/lightning icon
            Icon(
              BoltFilled,
              contentDescription = null,
              tint = MaterialTheme.colors.primary,
              modifier = Modifier.size(16.dp)
            )
          }
        }
      }
    }
  }
}

@Composable
fun ModalData.ChooseServerOperators(
  serverOperators: State<List<ServerOperator>>,
  selectedOperatorIds: MutableState<Set<Long>>,
  close: (() -> Unit)
) {
  LaunchedEffect(Unit) {
    prepareChatBeforeFinishingOnboarding()
  }
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false) {
      ColumnWithScrollBar(
        Modifier
          .themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer),
        maxIntrinsicSize = true
      ) {
        Box(Modifier.align(Alignment.CenterHorizontally)) {
          AppBarTitle(stringResource(MR.strings.onboarding_choose_server_operators), bottomPadding = DEFAULT_PADDING)
        }

        Column(Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING), horizontalAlignment = Alignment.CenterHorizontally) {
          OnboardingInformationButton(
            stringResource(MR.strings.how_it_helps_privacy),
            onClick = { ModalManager.fullscreen.showModal { ChooseServerOperatorsInfoView() } }
          )
        }

        Spacer(Modifier.weight(1f))
        Column((
            if (appPlatform.isDesktop) Modifier.width(600.dp).align(Alignment.CenterHorizontally) else Modifier)
          .fillMaxWidth()
          .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          serverOperators.value.forEachIndexed { index, srvOperator ->
            OperatorCheckView(srvOperator, selectedOperatorIds)
            if (index != serverOperators.value.lastIndex) {
              Spacer(Modifier.height(DEFAULT_PADDING))
            }
          }
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))

          SectionTextFooter(annotatedStringResource(MR.strings.onboarding_network_operators_simplex_flux_agreement), textAlign = TextAlign.Center)
          SectionTextFooter(annotatedStringResource(MR.strings.onboarding_network_operators_configure_via_settings), textAlign = TextAlign.Center)
        }
        Spacer(Modifier.weight(1f))

        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          val enabled = selectedOperatorIds.value.isNotEmpty()
          SetOperatorsButton(enabled, close)
          // Reserve space
          TextButtonBelowOnboardingButton("", null)
        }
      }
    }
  }
}

@Composable
private fun OperatorCheckView(serverOperator: ServerOperator, selectedOperatorIds: MutableState<Set<Long>>) {
  val checked = selectedOperatorIds.value.contains(serverOperator.operatorId)
  TextButton({
    if (checked) {
      selectedOperatorIds.value -= serverOperator.operatorId
    } else {
      selectedOperatorIds.value += serverOperator.operatorId
    }
  },
    border = BorderStroke(1.dp, color = if (checked) MaterialTheme.colors.primary else MaterialTheme.colors.secondary.copy(alpha = 0.5f)),
    shape = RoundedCornerShape(18.dp)
  ) {
    Row(Modifier.padding(DEFAULT_PADDING_HALF), verticalAlignment = Alignment.CenterVertically) {
      Image(painterResource(serverOperator.largeLogo), null, Modifier.height(48.dp))
      Spacer(Modifier.width(DEFAULT_PADDING_HALF).weight(1f))
      CircleCheckbox(checked)
    }
  }
}

@Composable
private fun CircleCheckbox(checked: Boolean) {
  if (checked) {
    Box(contentAlignment = Alignment.Center) {
      Icon(
        painterResource(MR.images.ic_circle_filled),
        null,
        Modifier.size(26.dp),
        tint = MaterialTheme.colors.primary
      )
      Icon(
        painterResource(MR.images.ic_check_filled),
        null,
        Modifier.size(20.dp), tint = MaterialTheme.colors.background
      )
    }
  } else {
    Icon(
      painterResource(MR.images.ic_circle),
      null,
      Modifier.size(26.dp),
      tint = MaterialTheme.colors.secondary.copy(alpha = 0.5f)
    )
  }
}

@Composable
private fun SetOperatorsButton(enabled: Boolean, close: () -> Unit) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = MR.strings.ok,
    onboarding = null,
    enabled = enabled,
    onclick = {
      close()
    }
  )
}

@Composable
private fun AcceptConditionsButton(
  enabled: Boolean,
  selectedOperatorIds: State<Set<Long>>
) {
  fun continueOnAccept() {
    if (appPlatform.isDesktop) {
      continueToNextStep()
    } else {
      continueToSetNotificationsAfterAccept()
    }
  }
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = MR.strings.onboarding_conditions_accept,
    onboarding = null,
    enabled = enabled,
    onclick = {
      withBGApi {
        val conditionsId = chatModel.conditions.value.currentConditions.conditionsId
        val r = chatController.acceptConditions(chatModel.remoteHostId(), conditionsId = conditionsId, operatorIds = selectedOperatorIds.value.toList())
        if (r != null) {
          chatModel.conditions.value = r
          val enabledOperators = enabledOperators(r.serverOperators, selectedOperatorIds.value)
          if (enabledOperators != null) {
            val r2 = chatController.setServerOperators(rh = chatModel.remoteHostId(), operators = enabledOperators)
            if (r2 != null) {
              chatModel.conditions.value = r2
              continueOnAccept()
            }
          } else {
            continueOnAccept()
          }
        }
      }
    }
  )
}

private fun continueToNextStep() {
    appPrefs.onboardingStage.set(if (appPlatform.isAndroid) OnboardingStage.Step4_SetNotificationsMode else OnboardingStage.OnboardingComplete)
}

private fun continueToSetNotificationsAfterAccept() {
  appPrefs.onboardingStage.set(OnboardingStage.Step4_SetNotificationsMode)
  ModalManager.fullscreen.showModalCloseable(showClose = false) { SetNotificationsMode(chatModel) }
}

private fun enabledOperators(operators: List<ServerOperator>, selectedOperatorIds: Set<Long>): List<ServerOperator>? {
  val ops = ArrayList(operators)
  if (ops.isNotEmpty()) {
    for (i in ops.indices) {
      val op = ops[i]
      ops[i] = op.copy(enabled = selectedOperatorIds.contains(op.operatorId))
    }
    val haveSMPStorage = ops.any { it.enabled && it.smpRoles.storage }
    val haveSMPProxy = ops.any { it.enabled && it.smpRoles.proxy }
    val haveXFTPStorage = ops.any { it.enabled && it.xftpRoles.storage }
    val haveXFTPProxy = ops.any { it.enabled && it.xftpRoles.proxy }
    val firstEnabledIndex = ops.indexOfFirst { it.enabled }
    if (haveSMPStorage && haveSMPProxy && haveXFTPStorage && haveXFTPProxy) {
      return ops
    } else if (firstEnabledIndex != -1) {
      var op = ops[firstEnabledIndex]
      if (!haveSMPStorage) op = op.copy(smpRoles = op.smpRoles.copy(storage = true))
      if (!haveSMPProxy) op = op.copy(smpRoles = op.smpRoles.copy(proxy = true))
      if (!haveXFTPStorage) op = op.copy(xftpRoles = op.xftpRoles.copy(storage = true))
      if (!haveXFTPProxy) op = op.copy(xftpRoles = op.xftpRoles.copy(proxy = true))
      ops[firstEnabledIndex] = op
      return ops
    } else { // Shouldn't happen - view doesn't let to proceed if no operators are enabled
      return null
    }
  } else {
    return null
  }
}

@Composable
private fun ChooseServerOperatorsInfoView() {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.onboarding_network_operators))

    Column(
      Modifier.padding(horizontal = DEFAULT_PADDING)
    ) {
      ReadableText(stringResource(MR.strings.onboarding_network_operators_app_will_use_different_operators))
      ReadableText(stringResource(MR.strings.onboarding_network_operators_cant_see_who_talks_to_whom))
      ReadableText(stringResource(MR.strings.onboarding_network_operators_app_will_use_for_routing))
    }

    SectionDividerSpaced()

    SectionView(title = stringResource(MR.strings.onboarding_network_about_operators).uppercase()) {
      chatModel.conditions.value.serverOperators.forEach { op ->
        ServerOperatorRow(op)
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun ServerOperatorRow(
  operator: ServerOperator
) {
  SectionItemView(
    {
      ModalManager.fullscreen.showModalCloseable { close ->
        OperatorInfoView(operator)
      }
    }
  ) {
    Image(
      painterResource(operator.logo),
      operator.tradeName,
      modifier = Modifier.size(24.dp)
    )
    TextIconSpaced()
    Text(operator.tradeName)
  }
}
