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
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.TextStyle
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
import chat.simplex.common.views.usersettings.networkAndServers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun OnboardingConditionsView(chatModel: ChatModel) {
  LaunchedEffect(Unit) {
    prepareChatBeforeFinishingOnboarding()
  }

  val serverOperators = remember { derivedStateOf { chatModel.conditions.value.serverOperators } }
  val selectedOperatorIds = remember {
    mutableStateOf(OnboardingSharedState.selectedOperatorIds.ifEmpty {
      serverOperators.value.filter { it.enabled }.map { it.operatorId }.toSet()
    })
  }

  if (appPlatform.isDesktop) {
    OnboardingConditionsDesktop(selectedOperatorIds)
  } else {
    CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
      ModalView({}, showClose = false, showAppBar = false, cardScreen = true) {
        OnboardingShrinkingLayout(
          modifier = Modifier.fillMaxSize().themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer)
            .systemBarsPadding()
            .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING),
          topPadding = DEFAULT_PADDING,
          image = {
            Column(Modifier.padding(vertical = DEFAULT_PADDING_HALF), horizontalAlignment = Alignment.CenterHorizontally) {
              OnboardingImage(
                MR.images.network_commitments, MR.images.network_commitments_light, MR.images.ic_shield,
                modifier = Modifier.fillMaxWidth(),
                aspectRatio = 1.5f
              )
            }
          },
          content = {
            Column(horizontalAlignment = Alignment.CenterHorizontally) {
              Text(
                stringResource(MR.strings.onboarding_network_commitments),
                style = MaterialTheme.typography.h1,
                fontWeight = FontWeight.Bold,
                textAlign = TextAlign.Center,
                lineHeight = 42.sp,
                modifier = Modifier.padding(top = DEFAULT_PADDING_HALF)
              )
              Column(
                Modifier.fillMaxWidth()
                  .padding(horizontal = DEFAULT_PADDING_HALF)
                  .padding(top = DEFAULT_PADDING),
                horizontalAlignment = Alignment.Start
              ) {
                Text(
                  stringResource(MR.strings.onboarding_conditions_private_chats_not_accessible),
                  style = MaterialTheme.typography.body2,
                  lineHeight = 22.sp
                )
                Spacer(Modifier.height(DEFAULT_PADDING))
                Text(
                  stringResource(MR.strings.onboarding_conditions_by_using_you_agree),
                  style = MaterialTheme.typography.body2,
                  lineHeight = 22.sp
                )
                Spacer(Modifier.height(DEFAULT_PADDING))
                Text(
                  stringResource(MR.strings.onboarding_conditions_privacy_policy_and_conditions_of_use),
                  style = MaterialTheme.typography.body1,
                  fontWeight = FontWeight.Medium,
                  color = MaterialTheme.colors.primary,
                  modifier = Modifier
                    .clickable(
                      interactionSource = remember { MutableInteractionSource() },
                      indication = null
                    ) {
                      ModalManager.fullscreen.showModal(endButtons = { ConditionsLinkButton() }) {
                        SimpleConditionsView(rhId = null) {
                          ModalManager.fullscreen.closeModal()
                          acceptConditions(selectedOperatorIds.value)
                        }
                      }
                    }
                )
              }
            }
          },
          button = {
            Column(Modifier.widthIn(max = 450.dp).padding(bottom = DEFAULT_PADDING * 2), horizontalAlignment = Alignment.CenterHorizontally) {
              AcceptConditionsButton(enabled = selectedOperatorIds.value.isNotEmpty(), selectedOperatorIds)
            }
          }
        )
      }
    }
  }
}

@Composable
private fun OnboardingConditionsDesktop(selectedOperatorIds: MutableState<Set<Long>>) {
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false, cardScreen = true) {
      ColumnWithScrollBar(horizontalAlignment = Alignment.CenterHorizontally) {
        Column(Modifier.widthIn(max = 600.dp).fillMaxHeight().padding(horizontal = DEFAULT_PADDING).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          Box(Modifier.align(Alignment.CenterHorizontally)) {
            AppBarTitle(stringResource(MR.strings.onboarding_network_commitments), bottomPadding = DEFAULT_PADDING, withPadding = false, overrideTitleColor = MaterialTheme.colors.onBackground, textAlign = TextAlign.Center, lineHeight = 42.sp)
          }
          Column(Modifier.width(450.dp), horizontalAlignment = Alignment.Start) {
            ReadableText(MR.strings.onboarding_conditions_private_chats_not_accessible, TextAlign.Start, padding = PaddingValues(), style = MaterialTheme.typography.body1)
            Spacer(Modifier.height(DEFAULT_PADDING))
            ReadableText(MR.strings.onboarding_conditions_by_using_you_agree, TextAlign.Start, padding = PaddingValues(), style = MaterialTheme.typography.body1)
            Spacer(Modifier.height(DEFAULT_PADDING))
            Text(
              stringResource(MR.strings.onboarding_conditions_privacy_policy_and_conditions_of_use),
              style = MaterialTheme.typography.body1,
              fontWeight = FontWeight.Medium,
              color = MaterialTheme.colors.primary,
              modifier = Modifier
                .clickable(
                  interactionSource = remember { MutableInteractionSource() },
                  indication = null
                ) {
                  ModalManager.fullscreen.showModal(forceAnimated = true, endButtons = { ConditionsLinkButton() }) {
                    SimpleConditionsView(rhId = null) {
                      ModalManager.fullscreen.closeModal()
                      acceptConditions(selectedOperatorIds.value)
                    }
                  }
                }
            )
          }
        }
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Column(Modifier.widthIn(max = 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          AcceptConditionsButton(enabled = selectedOperatorIds.value.isNotEmpty(), selectedOperatorIds)
          TextButtonBelowOnboardingButton("", null)
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
    ModalView(close, enableClose = selectedOperatorIds.value.isNotEmpty(), cardScreen = true) {
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

        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).padding(bottom = DEFAULT_PADDING * 2).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          val enabled = selectedOperatorIds.value.isNotEmpty()
          SetOperatorsButton(enabled, close)
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

private fun acceptConditions(selectedOperatorIds: Set<Long>) {
  withBGApi {
    val conditionsId = chatModel.conditions.value.currentConditions.conditionsId
    val r = chatController.acceptConditions(chatModel.remoteHostId(), conditionsId = conditionsId, operatorIds = selectedOperatorIds.toList())
    if (r != null) {
      chatModel.conditions.value = r
      val enabledOps = enabledOperators(r.serverOperators, selectedOperatorIds)
      if (enabledOps != null) {
        val r2 = chatController.setServerOperators(rh = chatModel.remoteHostId(), operators = enabledOps)
        if (r2 != null) {
          chatModel.conditions.value = r2
          completeOnboarding()
        }
      } else {
        completeOnboarding()
      }
    }
  }
}

@Composable
private fun AcceptConditionsButton(
  enabled: Boolean,
  selectedOperatorIds: State<Set<Long>>
) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = MR.strings.onboarding_conditions_accept,
    onboarding = null,
    enabled = enabled,
    onclick = { acceptConditions(selectedOperatorIds.value) }
  )
}

private fun completeOnboarding() {
  appPrefs.onboardingStage.set(OnboardingStage.OnboardingComplete)
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

    SectionView(title = stringResource(MR.strings.onboarding_network_about_operators)) {
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
