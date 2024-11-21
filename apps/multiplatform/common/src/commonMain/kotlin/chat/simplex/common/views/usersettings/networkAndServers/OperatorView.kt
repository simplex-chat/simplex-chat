package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionCustomFooter
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import TextIconSpaced
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.getUsageConditions
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingActionButton
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.launch

@Composable
fun ModalData.OperatorView(
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?
) {
  val testing = remember { mutableStateOf(false) }
  val operator = remember { userServers.value[operatorIndex].operator_ }
  val currentUser = remember { chatModel.currentUser }.value
  val scope = rememberCoroutineScope()

  Box {
    ColumnWithScrollBar(Modifier.alpha(if (testing.value) 0.6f else 1f)) {
      AppBarTitle(String.format(stringResource(MR.strings.operator_servers_title), operator.tradeName))
      OperatorViewLayout(
        currUserServers,
        userServers,
        serverErrors,
        operatorIndex,
        navigateToProtocolView = { serverIndex, server, protocol ->
          navigateToProtocolView(scope, userServers, serverErrors, operatorIndex, rhId, serverIndex, server, protocol)
        },
        currentUser,
        rhId,
        testing
      )
    }

    if (testing.value) {
      DefaultProgressView(null)
    }
  }
}

fun navigateToProtocolView(
  scope: CoroutineScope,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?,
  serverIndex: Int,
  server: UserServer,
  protocol: ServerProtocol
) {
  ModalManager.start.showCustomModal { close ->
    ProtocolServerView(
      m = chatModel,
      server = server,
      serverProtocol = protocol,
      userServers = userServers,
      serverErrors = serverErrors,
      onDelete = {
        if (protocol == ServerProtocol.SMP) {
          deleteSMPServer(userServers, operatorIndex, serverIndex)
        } else {
          deleteXFTPServer(userServers, operatorIndex, serverIndex)
        }
        close()
        scope.launch { validateServers(rhId, userServers, serverErrors) }
      },
      onUpdate = { updatedServer ->
        userServers.value = userServers.value.toMutableList().apply {
          this[operatorIndex] = this[operatorIndex].copy(
            smpServers = if (protocol == ServerProtocol.SMP) {
              this[operatorIndex].smpServers.toMutableList().apply {
                this[serverIndex] = updatedServer
              }
            } else this[operatorIndex].smpServers,
            xftpServers = if (protocol == ServerProtocol.XFTP) {
              this[operatorIndex].xftpServers.toMutableList().apply {
                this[serverIndex] = updatedServer
              }
            } else this[operatorIndex].xftpServers
          )
        }
      },
      close = close,
      rhId = rhId
    )
  }
}

@Composable
fun OperatorViewLayout(
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  navigateToProtocolView: (Int, UserServer, ServerProtocol) -> Unit,
  currentUser: User?,
  rhId: Long?,
  testing: MutableState<Boolean>
) {
  val operator by remember { derivedStateOf { userServers.value[operatorIndex].operator_ } }
  val scope = rememberCoroutineScope()
  val duplicateHosts = findDuplicateHosts(serverErrors.value)

  Column {
    SectionView(generalGetString(MR.strings.operator).uppercase()) {
      SectionItemView({ ModalManager.start.showModalCloseable { _ -> OperatorInfoView(operator) } }) {
        Image(
          painterResource(MR.images.decentralized),
          operator.tradeName,
          modifier = Modifier.size(24.dp),
          colorFilter = if (operator.enabled) null else ColorFilter.colorMatrix(ColorMatrix().apply {
            setToSaturation(0f)
          })
        )
        TextIconSpaced()
        Text(operator.tradeName, color = MaterialTheme.colors.onBackground)
      }
      UseOperatorToggle(
        scope = scope,
        currUserServers = currUserServers,
        userServers = userServers,
        serverErrors = serverErrors,
        operatorIndex = operatorIndex,
        rhId = rhId
      )
    }
    val serversErr = globalServersError(serverErrors.value)
    if (serversErr != null) {
      SectionCustomFooter {
        ServersErrorFooter(serversErr)
      }
    } else {
      val footerText = when (val c = operator.conditionsAcceptance) {
        is ConditionsAcceptance.Accepted -> if (c.acceptedAt != null) {
          String.format(generalGetString(MR.strings.operator_conditions_accepted_on), localTimestamp(c.acceptedAt))
        } else null
        is ConditionsAcceptance.Required -> if (operator.enabled && c.deadline != null) {
          String.format(generalGetString(MR.strings.operator_conditions_accepted_after), localTimestamp(c.deadline))
        } else null
      }
      if (footerText != null) {
        SectionTextFooter(footerText)
      }
    }

    if (operator.enabled) {
      if (userServers.value[operatorIndex].smpServers.any { !it.deleted }) {
        SectionDividerSpaced()
        SectionView(generalGetString(MR.strings.operator_use_for_messages).uppercase()) {
          SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
            Text(
              stringResource(MR.strings.operator_use_for_messages_receiving),
              Modifier.padding(end = 24.dp),
              color = Color.Unspecified
            )
            Spacer(Modifier.fillMaxWidth().weight(1f))
            DefaultSwitch(
              checked = userServers.value[operatorIndex].operator_.smpRoles.storage,
              onCheckedChange = { enabled ->
                userServers.value = userServers.value.toMutableList().apply {
                  this[operatorIndex] = this[operatorIndex].copy(
                    operator = this[operatorIndex].operator?.copy(
                      smpRoles = this[operatorIndex].operator?.smpRoles?.copy(storage = enabled) ?: ServerRoles(storage = enabled, proxy = false)
                    )
                  )
                }
                scope.launch {
                  validateServers(rhId, userServers, serverErrors)
                }
              }
            )
          }
          SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
            Text(
              stringResource(MR.strings.operator_use_for_messages_private_routing),
              Modifier.padding(end = 24.dp),
              color = Color.Unspecified
            )
            Spacer(Modifier.fillMaxWidth().weight(1f))
            DefaultSwitch(
              checked = userServers.value[operatorIndex].operator_.smpRoles.proxy,
              onCheckedChange = { enabled ->
                userServers.value = userServers.value.toMutableList().apply {
                  this[operatorIndex] = this[operatorIndex].copy(
                    operator = this[operatorIndex].operator?.copy(
                      smpRoles = this[operatorIndex].operator?.smpRoles?.copy(proxy = enabled) ?: ServerRoles(storage = false, proxy = enabled)
                    )
                  )
                }
                scope.launch {
                  validateServers(rhId, userServers, serverErrors)
                }
              }
            )
          }

        }
        val smpErr = globalSMPServersError(serverErrors.value)
        if (smpErr != null) {
          SectionCustomFooter {
            ServersErrorFooter(smpErr)
          }
        }
      }

      // Preset servers can't be deleted
      if (userServers.value[operatorIndex].smpServers.any { it.preset }) {
        SectionDividerSpaced()
        SectionView(generalGetString(MR.strings.message_servers).uppercase()) {
          userServers.value[operatorIndex].smpServers.forEachIndexed { i, server  ->
            if (!server.preset) return@forEachIndexed
            SectionItemView({ navigateToProtocolView(i, server, ServerProtocol.SMP) }) {
              ProtocolServerView(
                srv = server,
                serverProtocol = ServerProtocol.SMP,
                duplicateHosts = duplicateHosts
              )
            }
          }
        }
        val smpErr = globalSMPServersError(serverErrors.value)
        if (smpErr != null) {
          SectionCustomFooter {
            ServersErrorFooter(smpErr)
          }
        } else {
          SectionTextFooter(
            remember(currentUser?.displayName) {
              buildAnnotatedString {
                append(generalGetString(MR.strings.smp_servers_per_user) + " ")
                withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
                  append(currentUser?.displayName ?: "")
                }
                append(".")
              }
            }
          )
        }
      }

      if (userServers.value[operatorIndex].smpServers.any { !it.preset && !it.deleted }) {
        SectionDividerSpaced()
        SectionView(generalGetString(MR.strings.operator_added_message_servers).uppercase()) {
          userServers.value[operatorIndex].smpServers.forEachIndexed { i, server ->
            if (server.deleted || server.preset) return@forEachIndexed
            SectionItemView({ navigateToProtocolView(i, server, ServerProtocol.SMP) }) {
              ProtocolServerView(
                srv = server,
                serverProtocol = ServerProtocol.SMP,
                duplicateHosts = duplicateHosts
              )
            }
          }
        }
      }

      if (userServers.value[operatorIndex].xftpServers.any { !it.deleted }) {
        SectionDividerSpaced()
        SectionView(generalGetString(MR.strings.operator_use_for_sending).uppercase()) {
          SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
            Text(
              stringResource(MR.strings.operator_use_for_files),
              Modifier.padding(end = 24.dp),
              color = Color.Unspecified
            )
            Spacer(Modifier.fillMaxWidth().weight(1f))
            DefaultSwitch(
              checked = userServers.value[operatorIndex].operator_.xftpRoles.storage,
              onCheckedChange = { enabled ->
                userServers.value = userServers.value.toMutableList().apply {
                  this[operatorIndex] = this[operatorIndex].copy(
                    operator = this[operatorIndex].operator?.copy(
                      xftpRoles = this[operatorIndex].operator?.xftpRoles?.copy(storage = enabled) ?: ServerRoles(storage = enabled, proxy = false)
                    )
                  )
                }
                scope.launch {
                  validateServers(rhId, userServers, serverErrors)
                }
              }
            )
          }
        }
        val xftpErr = globalXFTPServersError(serverErrors.value)
        if (xftpErr != null) {
          SectionCustomFooter {
            ServersErrorFooter(xftpErr)
          }
        }
      }

      // Preset servers can't be deleted
      if (userServers.value[operatorIndex].xftpServers.any { it.preset }) {
        SectionDividerSpaced()
        SectionView(generalGetString(MR.strings.media_and_file_servers).uppercase()) {
          userServers.value[operatorIndex].xftpServers.forEachIndexed { i, server ->
            if (!server.preset) return@forEachIndexed
            SectionItemView({ navigateToProtocolView(i, server, ServerProtocol.XFTP) }) {
              ProtocolServerView(
                srv = server,
                serverProtocol = ServerProtocol.XFTP,
                duplicateHosts = duplicateHosts
              )
            }
          }
        }
        val xftpErr = globalXFTPServersError(serverErrors.value)
        if (xftpErr != null) {
          SectionCustomFooter {
            ServersErrorFooter(xftpErr)
          }
        } else {
          SectionTextFooter(
            remember(currentUser?.displayName) {
              buildAnnotatedString {
                append(generalGetString(MR.strings.xftp_servers_per_user) + " ")
                withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
                  append(currentUser?.displayName ?: "")
                }
                append(".")
              }
            }
          )
        }
      }

      if (userServers.value[operatorIndex].xftpServers.any { !it.preset && !it.deleted}) {
        SectionDividerSpaced()
        SectionView(generalGetString(MR.strings.operator_added_xftp_servers).uppercase()) {
          userServers.value[operatorIndex].xftpServers.forEachIndexed { i, server ->
            if (server.deleted || server.preset) return@forEachIndexed
            SectionItemView({ navigateToProtocolView(i, server, ServerProtocol.XFTP) }) {
              ProtocolServerView(
                srv = server,
                serverProtocol = ServerProtocol.XFTP,
                duplicateHosts = duplicateHosts
              )
            }
          }
        }
      }

      SectionDividerSpaced()
      SectionView {
        TestServersButton(
          testing = testing,
          smpServers = userServers.value[operatorIndex].smpServers,
          xftpServers = userServers.value[operatorIndex].xftpServers,
        ) { p, l ->
          when (p) {
            ServerProtocol.XFTP -> userServers.value = userServers.value.toMutableList().apply {
              this[operatorIndex] = this[operatorIndex].copy(
                xftpServers = l
              )
            }

            ServerProtocol.SMP -> userServers.value = userServers.value.toMutableList().apply {
              this[operatorIndex] = this[operatorIndex].copy(
                smpServers = l
              )
            }
          }
        }
      }

      SectionBottomSpacer()
    }
  }
}

@Composable
private fun OperatorInfoView(serverOperator: ServerOperator) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.operator_info_title))

    SectionView(generalGetString(MR.strings.operator_description).uppercase()) {
      SectionItemView {
        Text(serverOperator.info.description)
      }
    }
    SectionDividerSpaced()
    SectionView(generalGetString(MR.strings.operator_website).uppercase()) {
      SectionItemView {
        val website = serverOperator.info.website
        val uriHandler = LocalUriHandler.current
        Text(website, color = MaterialTheme.colors.primary, modifier = Modifier.clickable { uriHandler.openUriCatching(website) })
      }
    }
  }
}

@Composable
private fun UseOperatorToggle(
  scope: CoroutineScope,
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?
) {
  SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    Text(
      stringResource(MR.strings.operator_use_operator_toggle_description),
      Modifier.padding(end = 24.dp),
      color = Color.Unspecified
    )
    Spacer(Modifier.fillMaxWidth().weight(1f))
    DefaultSwitch(
      checked = userServers.value[operatorIndex].operator?.enabled ?: false,
      onCheckedChange = { enabled ->
        val operator = userServers.value[operatorIndex].operator
        if (enabled) {
          when (val conditionsAcceptance = operator?.conditionsAcceptance) {
            is ConditionsAcceptance.Accepted -> {
              changeOperatorEnabled(userServers, operatorIndex, true)
              scope.launch { validateServers(rhId, userServers, serverErrors) }
            }

            is ConditionsAcceptance.Required -> {
              if (conditionsAcceptance.deadline == null) {
                ModalManager.start.showModalCloseable { close ->
                  SingleOperatorUsageConditionsView(
                    currUserServers = currUserServers,
                    userServers = userServers,
                    serverErrors = serverErrors,
                    operatorIndex = operatorIndex,
                    rhId = rhId,
                    close = close
                  )
                }
              } else {
                changeOperatorEnabled(userServers, operatorIndex, true)
                scope.launch { validateServers(rhId, userServers, serverErrors) }
              }
            }

            else -> {}
          }
        } else {
          changeOperatorEnabled(userServers, operatorIndex, false)
          scope.launch { validateServers(rhId, userServers, serverErrors) }
        }
      },
    )
  }
}

@Composable
private fun SingleOperatorUsageConditionsView(
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?,
  close: () -> Unit
) {
  val operatorsWithConditionsAccepted = remember { chatModel.conditions.value.serverOperators.filter { it.conditionsAcceptance.conditionsAccepted } }
  val operator = remember { userServers.value[operatorIndex].operator_ }
  val scope = rememberCoroutineScope()

  suspend fun acceptForOperators(rhId: Long?, operatorIds: List<Long>, operatorIndexToEnable: Int, close: () -> Unit) {
    try {
      val conditionsId = chatModel.conditions.value.currentConditions.conditionsId
      val r = chatController.acceptConditions(rhId, conditionsId, operatorIds) ?: return

      chatModel.conditions.value = r
      updateOperatorsConditionsAcceptance(currUserServers, r.serverOperators)
      updateOperatorsConditionsAcceptance(userServers, r.serverOperators)
      changeOperatorEnabled(userServers, operatorIndex, true)
      validateServers(rhId, userServers, serverErrors)
      close()
    } catch (ex: Exception) {
      Log.e(TAG, ex.stackTraceToString())
    }
  }

  @Composable
  fun AcceptConditionsButton(close: () -> Unit) {
    // Opened operator or Other enabled operators with conditions not accepted
    val operatorIds = chatModel.conditions.value.serverOperators
      .filter { it.operatorId == operator.id || (it.enabled && !it.conditionsAcceptance.conditionsAccepted) }
      .map { it.operatorId }

    Column(Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING * 2), horizontalAlignment = Alignment.CenterHorizontally) {
      OnboardingActionButton(
        labelId = MR.strings.accept_conditions,
        onboarding = null,
        enabled = operatorIds.isNotEmpty(),
        onclick = {
          scope.launch {
            acceptForOperators(rhId, operatorIds, operatorIndex, close)
          }
        }
      )
    }
  }

  @Composable
  fun UsageConditionsDestinationView(close: () -> Unit) {
    ColumnWithScrollBar(modifier = Modifier.fillMaxSize()) {
      NonScrollableTitle(stringResource(MR.strings.operator_conditions_of_use))
      Column(modifier = Modifier.weight(1f).padding(end = DEFAULT_PADDING, start = DEFAULT_PADDING, bottom = DEFAULT_PADDING, top = DEFAULT_PADDING)) {
        ConditionsTextView(rhId)
      }
      AcceptConditionsButton(close)
    }
  }

  @Composable
  fun UsageConditionsNavLinkButton() {
    Text(
      stringResource(MR.strings.view_conditions),
      color = MaterialTheme.colors.primary,
      modifier = Modifier.clickable {
        ModalManager.start.showModalCloseable { close ->
          UsageConditionsDestinationView(close)
        }
      }
    )
  }

  ColumnWithScrollBar(modifier = Modifier.fillMaxSize()) {
    NonScrollableTitle(String.format(stringResource(MR.strings.use_operator_x), operator.tradeName))
    if (operator.conditionsAcceptance is ConditionsAcceptance.Accepted) {
      // In current UI implementation this branch doesn't get shown - as conditions can't be opened from inside operator once accepted
      Column(modifier = Modifier.weight(1f).padding(end = DEFAULT_PADDING, start = DEFAULT_PADDING, bottom = DEFAULT_PADDING)) {
        ConditionsTextView(rhId)
      }
    } else if (operatorsWithConditionsAccepted.isNotEmpty()) {
      SectionItemView {
        Text(String.format(stringResource(MR.strings.operator_conditions_accepted_for_some), operatorsWithConditionsAccepted.joinToString(", ") { it.legalName_ }))
      }
      SectionItemView {
        Text(String.format(stringResource(MR.strings.operator_same_conditions_will_be_applied), operator.legalName_))
      }
      ConditionsAppliedToOtherOperatorsText(userServers = userServers.value, operatorIndex = operatorIndex)

      SectionItemView {
        UsageConditionsNavLinkButton()
      }
      Spacer(Modifier.fillMaxWidth().weight(1f))
      AcceptConditionsButton(close)
    } else {
      SectionItemView {
        Text(String.format(stringResource(MR.strings.operator_in_order_to_use_accept_conditions, operator.legalName_)))
      }
      SectionItemView {
        ConditionsAppliedToOtherOperatorsText(userServers = userServers.value, operatorIndex = operatorIndex)
      }
      Column(modifier = Modifier.weight(1f).padding(end = DEFAULT_PADDING, start = DEFAULT_PADDING, bottom = DEFAULT_PADDING)) {
        ConditionsTextView(rhId)
      }
      AcceptConditionsButton(close)
    }
  }
}

@Composable
private fun NonScrollableTitle(title: String) {
  val titleColor = MaterialTheme.appColors.title
  val theme = CurrentColors.collectAsState()

  val brush = if (theme.value.base == DefaultTheme.SIMPLEX)
    Brush.linearGradient(listOf(titleColor.darker(0.2f), titleColor.lighter(0.35f)), Offset(0f, Float.POSITIVE_INFINITY), Offset(Float.POSITIVE_INFINITY, 0f))
  else // color is not updated when changing themes if I pass null here
    Brush.linearGradient(listOf(titleColor, titleColor), Offset(0f, Float.POSITIVE_INFINITY), Offset(Float.POSITIVE_INFINITY, 0f))

  Text(
    title,
    Modifier
      .padding(start = DEFAULT_PADDING, top = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING),
    overflow = TextOverflow.Ellipsis,
    style = MaterialTheme.typography.h1.copy(brush = brush),
    color = MaterialTheme.colors.primaryVariant,
    textAlign = TextAlign.Start
  )
}

@Composable
fun ConditionsTextView(
  rhId: Long?
) {
  val conditionsData = remember { mutableStateOf<Triple<UsageConditionsDetail, String?, UsageConditionsDetail?>?>(null) }
  val failedToLoad = remember { mutableStateOf(false) }
  val defaultConditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md"
  val scope = rememberCoroutineScope()

  LaunchedEffect(Unit) {
    scope.launch {
      try {
        val conditions = getUsageConditions(rh = rhId)

        if (conditions != null) {
          conditionsData.value = conditions
        } else {
          failedToLoad.value = true
        }
      } catch (ex: Exception) {
        failedToLoad.value = true
      }
    }
  }
  val conditions = conditionsData.value

  if (conditions != null) {
    val (usageConditions, conditionsText, _) = conditions

    if (conditionsText != null) {
      val scrollState = rememberScrollState()
        Box(
          modifier = Modifier
            .fillMaxSize()
            .clip(RoundedCornerShape(12.dp))
            .verticalScroll(scrollState)
            .background(
              color =  MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f),
            )
            .padding(8.dp)
        ) {
          Text(
            text = conditionsText.trimIndent(),
            modifier = Modifier.padding(16.dp)
          )
        }
    } else {
      val conditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/${usageConditions.conditionsCommit}/PRIVACY.md"
      ConditionsLinkView(conditionsLink)
    }
  } else if (failedToLoad.value) {
    ConditionsLinkView(defaultConditionsLink)
  } else {
    DefaultProgressView(null)
  }
}

@Composable
private fun ConditionsLinkView(conditionsLink: String) {
  SectionItemView {
    val uriHandler = LocalUriHandler.current
    Text(stringResource(MR.strings.operator_conditions_failed_to_load), color = MaterialTheme.colors.onBackground)
    Text(conditionsLink, color = MaterialTheme.colors.primary, modifier = Modifier.clickable { uriHandler.openUriCatching(conditionsLink) })
  }
}

@Composable
private fun ConditionsAppliedToOtherOperatorsText(userServers: List<UserOperatorServers>, operatorIndex: Int) {
  val otherOperatorsToApply = remember {
    derivedStateOf {
      chatModel.conditions.value.serverOperators.filter {
        it.enabled &&
            !it.conditionsAcceptance.conditionsAccepted &&
            it.operatorId != userServers[operatorIndex].operator_.operatorId
      }
    }
  }

  if (otherOperatorsToApply.value.isNotEmpty()) {
    SectionItemView {
      Text(
        String.format(stringResource(MR.strings.operators_conditions_will_also_apply), otherOperatorsToApply.value.joinToString(", ") { it.legalName_ }),
      )
    }
  }
}

private fun changeOperatorEnabled(userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int, enabled: Boolean) {
  userServers.value = userServers.value.toMutableList().apply {
    this[operatorIndex] = this[operatorIndex].copy(
      operator = this[operatorIndex].operator?.copy(enabled = enabled)
    )
  }
}