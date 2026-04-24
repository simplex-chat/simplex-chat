package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
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
import chat.simplex.common.views.usersettings.networkAndServers.ConditionsLinkButton
import chat.simplex.common.views.usersettings.networkAndServers.SimpleConditionsView
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign

@Composable
fun NetworkCommitmentsView(chatModel: ChatModel) {
  LaunchedEffect(Unit) {
    prepareChatBeforeFinishingOnboarding()
  }

  val serverOperators = remember { derivedStateOf { chatModel.conditions.value.serverOperators } }
  val selectedOperatorIds = remember {
    mutableStateOf(OnboardingSharedState.selectedOperatorIds.ifEmpty {
      serverOperators.value.filter { it.enabled }.map { it.operatorId }.toSet()
    })
  }

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
            painterResource(if (isInDarkTheme()) MR.images.network_commitments_light else MR.images.network_commitments),
            contentDescription = null,
            contentScale = ContentScale.Fit,
            modifier = Modifier.fillMaxWidth().padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING)
              .then(if (!appPlatform.isAndroid) Modifier.heightIn(max = 220.dp) else Modifier)
          )
        } else {
          val isDark = isInDarkTheme()
          val stops = if (isDark) darkStops else lightStops
          val scale = if (isDark) 1.5f else 1.2f
          Box(
            Modifier
              .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING)
              .then(if (appPlatform.isAndroid) Modifier.fillMaxWidth() else Modifier.heightIn(max = 220.dp))
              .aspectRatio(1.5f)
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
              painterResource(MR.images.ic_shield),
              contentDescription = null,
              modifier = Modifier.size(80.dp),
              tint = MaterialTheme.colors.primary
            )
          }
        }

        Text(
          stringResource(MR.strings.onboarding_network_commitments),
          style = MaterialTheme.typography.h1,
          fontWeight = FontWeight.Bold,
          textAlign = TextAlign.Center,
          lineHeight = 42.sp,
          modifier = Modifier.padding(top = DEFAULT_PADDING_HALF)
        )

        Column(
          (if (appPlatform.isDesktop) Modifier.width(450.dp).align(Alignment.CenterHorizontally) else Modifier)
            .fillMaxWidth()
            .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING)
            .padding(top = DEFAULT_PADDING),
          horizontalAlignment = Alignment.Start
        ) {
          Text(
            stringResource(MR.strings.onboarding_network_commitments_operators),
            style = TextStyle(fontSize = 17.sp, lineHeight = 23.sp)
          )
          Spacer(Modifier.height(DEFAULT_PADDING))
          Text(
            stringResource(MR.strings.onboarding_network_commitments_you),
            style = TextStyle(fontSize = 17.sp, lineHeight = 23.sp)
          )
          Spacer(Modifier.height(DEFAULT_PADDING))
          Text(
            stringResource(MR.strings.onboarding_conditions_privacy_policy_and_conditions_of_use),
            style = TextStyle(fontSize = 17.sp),
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
          NetworkCommitmentsAcceptButton(enabled = selectedOperatorIds.value.isNotEmpty(), selectedOperatorIds)
          TextButtonBelowOnboardingButton("", null)
        }
      }
    }
  }
}

@Composable
private fun NetworkCommitmentsAcceptButton(
  enabled: Boolean,
  selectedOperatorIds: State<Set<Long>>
) {
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
          val enabledOps = enabledOperators(r.serverOperators, selectedOperatorIds.value)
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
    } else {
      return null
    }
  } else {
    return null
  }
}
