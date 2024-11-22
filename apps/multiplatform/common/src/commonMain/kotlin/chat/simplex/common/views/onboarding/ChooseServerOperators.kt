package chat.simplex.common.views.onboarding

import SectionBottomSpacer
import SectionTextFooter
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ServerOperator
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.net.URI

@Composable
fun ModalData.ChooseServerOperators(onboarding: Boolean, close: (() -> Unit) = { ModalManager.fullscreen.closeModals() }) {
  LaunchedEffect(Unit) {
    prepareChatBeforeFinishingOnboarding()
  }

  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false, endButtons = {
      IconButton({ ModalManager.fullscreen.showModal { ChooseServerOperatorsInfoView() } }) {
        Icon(painterResource(MR.images.ic_info), null, Modifier.size(28.dp), tint = MaterialTheme.colors.primary)
      }
    }) {
      val serverOperators = remember { derivedStateOf { chatModel.conditions.value.serverOperators } }
      val selectedOperatorIds = remember { stateGetOrPut("selectedOperatorIds") { serverOperators.value.filter { it.enabled }.map { it.operatorId }.toSet() } }
      val selectedOperators = remember { derivedStateOf { serverOperators.value.filter { selectedOperatorIds.value.contains(it.operatorId) } } }

      ColumnWithScrollBar(
        Modifier
          .themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer),
        maxIntrinsicSize = true
      ) {
        Box(Modifier.align(Alignment.CenterHorizontally)) {
          AppBarTitle(stringResource(MR.strings.onboarding_choose_server_operators))
        }
        Column((
            if (appPlatform.isDesktop) Modifier.width(600.dp).align(Alignment.CenterHorizontally) else Modifier)
          .padding(horizontal = DEFAULT_PADDING)
        ) {
          Text(stringResource(MR.strings.onboarding_select_network_operators_to_use))
          Spacer(Modifier.height(DEFAULT_PADDING))
        }
        Spacer(Modifier.weight(1f))
        Column((
            if (appPlatform.isDesktop) Modifier.width(600.dp).align(Alignment.CenterHorizontally) else Modifier)
          .fillMaxWidth()
          .padding(horizontal = DEFAULT_PADDING)
        ) {
          serverOperators.value.forEachIndexed { index, srvOperator ->
            OperatorCheckView(srvOperator, selectedOperatorIds)
            if (index != serverOperators.value.lastIndex) {
              Spacer(Modifier.height(DEFAULT_PADDING))
            }
          }
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))

          SectionTextFooter(annotatedStringResource(MR.strings.onboarding_network_operators_configure_via_settings), textAlign = TextAlign.Center)
        }
        Spacer(Modifier.weight(1f))

        val reviewForOperators = selectedOperators.value.filter { !it.conditionsAcceptance.conditionsAccepted }
        val canReviewLater = reviewForOperators.all { it.conditionsAcceptance.usageAllowed }
        val currEnabledOperatorIds = serverOperators.value.filter { it.enabled }.map { it.operatorId }.toSet()

        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          val enabled = selectedOperatorIds.value.isNotEmpty()
          when {
            reviewForOperators.isNotEmpty() -> ReviewConditionsButton(enabled, onboarding, selectedOperators, selectedOperatorIds)
            selectedOperatorIds.value != currEnabledOperatorIds && enabled -> SetOperatorsButton(true, onboarding, serverOperators, selectedOperatorIds, close)
            else -> ContinueButton(enabled, onboarding, close)
          }
          if (onboarding && reviewForOperators.isEmpty()) {
              TextButtonBelowOnboardingButton(stringResource(MR.strings.operator_conditions_of_use)) {
                ModalManager.fullscreen.showModalCloseable(endButtons = { ConditionsLinkButton() }) { close ->
                  UsageConditionsView(
                    currUserServers = remember { mutableStateOf(emptyList()) },
                    userServers = remember { mutableStateOf(emptyList()) },
                    close = close,
                    rhId = null
                  )
                }
              }
          } else if (onboarding || reviewForOperators.isEmpty()) {
            // Reserve space
            TextButtonBelowOnboardingButton("", null)
          }
          if (!onboarding && reviewForOperators.isNotEmpty()) {
            ReviewLaterButton(canReviewLater, close)
            SectionTextFooter(
              annotatedStringResource(MR.strings.onboarding_network_operators_conditions_will_be_accepted) +
                  AnnotatedString(" ") +
                  annotatedStringResource(MR.strings.onboarding_network_operators_conditions_you_can_configure),
              textAlign = TextAlign.Center
            )
            SectionBottomSpacer()
          }
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
private fun ReviewConditionsButton(
  enabled: Boolean,
  onboarding: Boolean,
  selectedOperators: State<List<ServerOperator>>,
  selectedOperatorIds: State<Set<Long>>
) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier,
    labelId = MR.strings.operator_review_conditions,
    onboarding = null,
    enabled = enabled,
    onclick = {
      ModalManager.fullscreen.showModalCloseable(endButtons = { ConditionsLinkButton() }) { close ->
        ReviewConditionsView(onboarding, selectedOperators, selectedOperatorIds, close)
      }
    }
  )
}

@Composable
private fun SetOperatorsButton(enabled: Boolean, onboarding: Boolean, serverOperators: State<List<ServerOperator>>, selectedOperatorIds: State<Set<Long>>, close: () -> Unit) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier,
    labelId = MR.strings.onboarding_network_operators_update,
    onboarding = null,
    enabled = enabled,
    onclick = {
      withBGApi {
        val enabledOperators = enabledOperators(serverOperators.value, selectedOperatorIds.value)
        if (enabledOperators != null) {
          val r = chatController.setServerOperators(rh = chatModel.remoteHostId(), operators = enabledOperators)
          if (r != null) {
            chatModel.conditions.value = r
          }
          continueToNextStep(onboarding, close)
        }
      }
    }
  )
}

@Composable
private fun ContinueButton(enabled: Boolean, onboarding: Boolean, close: () -> Unit) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier,
    labelId = MR.strings.onboarding_network_operators_continue,
    onboarding = null,
    enabled = enabled,
    onclick = {
      continueToNextStep(onboarding, close)
    }
  )
}

@Composable
private fun ReviewLaterButton(enabled: Boolean, close: () -> Unit) {
  TextButtonBelowOnboardingButton(
    stringResource(MR.strings.onboarding_network_operators_review_later),
    onClick = if (!enabled) null else {{ continueToNextStep(false, close) }}
  )
}

@Composable
private fun ReviewConditionsView(
  onboarding: Boolean,
  selectedOperators: State<List<ServerOperator>>,
  selectedOperatorIds: State<Set<Long>>,
  close: () -> Unit
) {
  // remembering both since we don't want to reload the view after the user accepts conditions
  val operatorsWithConditionsAccepted = remember { chatModel.conditions.value.serverOperators.filter { it.conditionsAcceptance.conditionsAccepted } }
  val acceptForOperators = remember { selectedOperators.value.filter { !it.conditionsAcceptance.conditionsAccepted } }
  ColumnWithScrollBar(modifier = Modifier.fillMaxSize().padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.operator_conditions_of_use), withPadding = false, enableAlphaChanges = false)
    if (operatorsWithConditionsAccepted.isNotEmpty()) {
      ReadableText(MR.strings.operator_conditions_accepted_for_some, args = operatorsWithConditionsAccepted.joinToString(", ") { it.legalName_ })
      ReadableText(MR.strings.operator_same_conditions_will_apply_to_operators, args = acceptForOperators.joinToString(", ") { it.legalName_ })
    } else {
      ReadableText(MR.strings.operator_conditions_will_be_accepted_for_some, args = acceptForOperators.joinToString(", ") { it.legalName_ })
    }
    Box(Modifier.weight(1f)) {
      ConditionsTextView(chatModel.remoteHostId())
    }
    Column(Modifier.padding(top = DEFAULT_PADDING * 2).widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
      AcceptConditionsButton(onboarding, selectedOperators, selectedOperatorIds, close)
      // Reserve space
      TextButtonBelowOnboardingButton("", null)
    }
  }
}

@Composable
private fun AcceptConditionsButton(
  onboarding: Boolean,
  selectedOperators: State<List<ServerOperator>>,
  selectedOperatorIds: State<Set<Long>>,
  close: () -> Unit
) {
  fun continueOnAccept() {
    if (appPlatform.isDesktop || !onboarding) {
      if (onboarding) { close() }
      continueToNextStep(onboarding, close)
    } else {
      continueToSetNotificationsAfterAccept()
    }
  }
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier,
    labelId = MR.strings.accept_conditions,
    onboarding = null,
    onclick = {
      withBGApi {
        val conditionsId = chatModel.conditions.value.currentConditions.conditionsId
        val acceptForOperators = selectedOperators.value.filter { !it.conditionsAcceptance.conditionsAccepted }
        val operatorIds = acceptForOperators.map { it.operatorId }
        val r = chatController.acceptConditions(chatModel.remoteHostId(), conditionsId = conditionsId, operatorIds = operatorIds)
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

private fun continueToNextStep(onboarding: Boolean, close: () -> Unit) {
  if (onboarding) {
    appPrefs.onboardingStage.set(if (appPlatform.isAndroid) OnboardingStage.Step4_SetNotificationsMode else OnboardingStage.OnboardingComplete)
  } else {
    close()
  }
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
  ColumnWithScrollBar(Modifier.padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.onboarding_network_operators), withPadding = false)
    ReadableText(stringResource(MR.strings.onboarding_network_operators_app_will_use_different_operators))
    ReadableText(stringResource(MR.strings.onboarding_network_operators_app_will_use_for_routing))
    SectionBottomSpacer()
  }
}
