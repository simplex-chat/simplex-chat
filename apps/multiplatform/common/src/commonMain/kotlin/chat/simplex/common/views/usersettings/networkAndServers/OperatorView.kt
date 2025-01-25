package chat.simplex.common.views.usersettings.networkAndServers

import SectionBottomSpacer
import SectionCustomFooter
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.getUsageConditions
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.res.MR
import com.mikepenz.markdown.compose.Markdown
import com.mikepenz.markdown.compose.components.markdownComponents
import com.mikepenz.markdown.compose.elements.MarkdownHeader
import com.mikepenz.markdown.m2.markdownColor
import com.mikepenz.markdown.m2.markdownTypography
import com.mikepenz.markdown.model.markdownPadding
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import org.intellij.markdown.flavours.commonmark.CommonMarkFlavourDescriptor

@Composable
fun OperatorView(
  currUserServers: MutableState<List<UserOperatorServers>>,
  userServers: MutableState<List<UserOperatorServers>>,
  serverErrors: MutableState<List<UserServersError>>,
  operatorIndex: Int,
  rhId: Long?
) {
  val testing = remember { mutableStateOf(false) }
  val operator = remember { userServers.value[operatorIndex].operator_ }
  val currentUser = remember { chatModel.currentUser }.value

  LaunchedEffect(userServers) {
    snapshotFlow { userServers.value }
      .collect { updatedServers ->
        validateServers_(rhId = rhId, userServersToValidate = updatedServers, serverErrors = serverErrors)
      }
  }

  Box {
    ColumnWithScrollBar(Modifier.alpha(if (testing.value) 0.6f else 1f)) {
      AppBarTitle(String.format(stringResource(MR.strings.operator_servers_title), operator.tradeName))
      OperatorViewLayout(
        currUserServers,
        userServers,
        serverErrors,
        operatorIndex,
        navigateToProtocolView = { serverIndex, server, protocol ->
          navigateToProtocolView(userServers, serverErrors, operatorIndex, rhId, serverIndex, server, protocol)
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
        Row(
          Modifier.fillMaxWidth(),
          verticalAlignment = Alignment.CenterVertically
        ) {
          Image(
            painterResource(operator.largeLogo),
            operator.tradeName,
            modifier = Modifier.height(48.dp),
            colorFilter = if (operator.enabled) null else ColorFilter.colorMatrix(ColorMatrix().apply {
              setToSaturation(0f)
            })
          )
          Spacer(Modifier.fillMaxWidth().weight(1f))
          Box(Modifier.padding(horizontal = 2.dp)) {
            Icon(painterResource(MR.images.ic_info), null, Modifier.size(24.dp), tint = MaterialTheme.colors.primaryVariant)
          }
        }
      }
      UseOperatorToggle(
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
          String.format(generalGetString(MR.strings.operator_conditions_accepted_on), localDate(c.acceptedAt))
        } else null
        is ConditionsAcceptance.Required -> if (operator.enabled && c.deadline != null) {
          String.format(generalGetString(MR.strings.operator_conditions_will_be_accepted_on), localDate(c.deadline))
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
              ProtocolServerViewLink(
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
              ProtocolServerViewLink(
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
        SectionView(generalGetString(MR.strings.operator_use_for_files).uppercase()) {
          SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
            Text(
              stringResource(MR.strings.operator_use_for_sending),
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
              ProtocolServerViewLink(
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
              ProtocolServerViewLink(
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
fun OperatorInfoView(serverOperator: ServerOperator) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.operator_info_title))

    SectionView {
      SectionItemView {
        Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
          Image(painterResource(serverOperator.largeLogo), null, Modifier.height(48.dp))
          if (serverOperator.legalName != null) {
            Text(serverOperator.legalName)
          }
        }
      }
    }

    SectionDividerSpaced(maxBottomPadding = false)

    val uriHandler = LocalUriHandler.current
    SectionView {
      SectionItemView {
        Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
          serverOperator.info.description.forEach { d ->
            Text(d)
          }
          val website = serverOperator.info.website
          Text(website, color = MaterialTheme.colors.primary, modifier = Modifier.clickable { uriHandler.openUriCatching(website) })
        }
      }
    }

    val selfhost = serverOperator.info.selfhost
    if (selfhost != null) {
      SectionDividerSpaced(maxBottomPadding = false)
      SectionView {
        SectionItemView {
          val (text, link) = selfhost
          Text(text, color = MaterialTheme.colors.primary, modifier = Modifier.clickable { uriHandler.openUriCatching(link) })
        }
      }
    }
  }
}

@Composable
private fun UseOperatorToggle(
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
            }

            is ConditionsAcceptance.Required -> {
              if (conditionsAcceptance.deadline == null) {
                ModalManager.start.showModalCloseable(endButtons = { ConditionsLinkButton() }) { close ->
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
              }
            }

            else -> {}
          }
        } else {
          changeOperatorEnabled(userServers, operatorIndex, false)
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
      AppBarTitle(stringResource(MR.strings.operator_conditions_of_use), enableAlphaChanges = false)
      Column(modifier = Modifier.weight(1f).padding(end = DEFAULT_PADDING, start = DEFAULT_PADDING, bottom = DEFAULT_PADDING, top = DEFAULT_PADDING_HALF)) {
        ConditionsTextView(rhId)
      }
    }
  }

  @Composable
  fun UsageConditionsNavLinkButton() {
    Text(
      stringResource(MR.strings.view_conditions),
      color = MaterialTheme.colors.primary,
      modifier = Modifier.padding(top = DEFAULT_PADDING_HALF).clickable {
        ModalManager.start.showModalCloseable(endButtons = { ConditionsLinkButton() }) { close ->
          UsageConditionsDestinationView(close)
        }
      }
    )
  }

  ColumnWithScrollBar(modifier = Modifier.fillMaxSize().padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle(String.format(stringResource(MR.strings.use_servers_of_operator_x), operator.tradeName), enableAlphaChanges = false, withPadding = false)
    if (operator.conditionsAcceptance is ConditionsAcceptance.Accepted) {
      // In current UI implementation this branch doesn't get shown - as conditions can't be opened from inside operator once accepted
      Column(modifier = Modifier.weight(1f).padding(top = DEFAULT_PADDING_HALF, bottom = DEFAULT_PADDING)) {
        ConditionsTextView(rhId)
      }
    } else if (operatorsWithConditionsAccepted.isNotEmpty()) {
      ReadableText(
        MR.strings.operator_conditions_accepted_for_some,
        args = operatorsWithConditionsAccepted.joinToString(", ") { it.legalName_ }
      )
      ReadableText(
        MR.strings.operator_same_conditions_will_be_applied,
        args = operator.legalName_
      )
      ConditionsAppliedToOtherOperatorsText(userServers = userServers.value, operatorIndex = operatorIndex)

      UsageConditionsNavLinkButton()
      Spacer(Modifier.fillMaxWidth().weight(1f))
      AcceptConditionsButton(close)
    } else {
      ReadableText(
        MR.strings.operator_in_order_to_use_accept_conditions,
        args = operator.legalName_
      )
      ConditionsAppliedToOtherOperatorsText(userServers = userServers.value, operatorIndex = operatorIndex)
      Column(modifier = Modifier.weight(1f).padding(top = DEFAULT_PADDING_HALF, bottom = DEFAULT_PADDING)) {
        ConditionsTextView(rhId)
      }
      AcceptConditionsButton(close)
    }
  }
}

@Composable
fun ConditionsTextView(
  rhId: Long?
) {
  val conditionsData = remember { mutableStateOf<Triple<UsageConditionsDetail, String?, UsageConditionsDetail?>?>(null) }
  val failedToLoad = remember { mutableStateOf(false) }
  val defaultConditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md"
  val scope = rememberCoroutineScope()
  // can show conditions when animation between modals finishes to prevent glitches
  val canShowConditionsAt = remember { System.currentTimeMillis() + 300 }
  LaunchedEffect(Unit) {
    scope.launch(Dispatchers.Default) {
      try {
        val conditions = getUsageConditions(rh = rhId)

        if (conditions != null) {
          val parentLink = "https://github.com/simplex-chat/simplex-chat/blob/${conditions.first.conditionsCommit}"
          val conditionsText = conditions.second
          val preparedText = if (conditionsText != null) prepareMarkdown(conditionsText.trimIndent(), parentLink) else null
          val modifiedConditions = Triple(conditions.first, preparedText, conditions.third)
          delay((canShowConditionsAt - System.currentTimeMillis()).coerceAtLeast(0))
          conditionsData.value = modifiedConditions
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
      ConditionsBox(
        Modifier
          .fillMaxSize()
          .border(border = BorderStroke(1.dp, CurrentColors.value.colors.secondary.copy(alpha = 0.6f)), shape = RoundedCornerShape(12.dp))
          .clip(shape = RoundedCornerShape(12.dp)),
        scrollState
      ) {
        val parentUriHandler = LocalUriHandler.current
        CompositionLocalProvider(LocalUriHandler provides remember { internalUriHandler(parentUriHandler) }) {
          ConditionsMarkdown(conditionsText)
        }
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
expect fun ConditionsBox(modifier: Modifier, scrollState: ScrollState, content: @Composable() (BoxScope.() -> Unit))

@Composable
private fun ConditionsMarkdown(text: String) {
  Markdown(text,
    markdownColor(linkText = MaterialTheme.colors.primary),
    markdownTypography(
      h1 = MaterialTheme.typography.body1,
      h2 = MaterialTheme.typography.h3.copy(fontSize = 22.sp, fontWeight = FontWeight.Bold),
      h3 = MaterialTheme.typography.h4.copy(fontWeight = FontWeight.Bold),
      h4 = MaterialTheme.typography.h5.copy(fontSize = 16.sp, fontWeight = FontWeight.Bold),
      h5 = MaterialTheme.typography.h6.copy(fontWeight = FontWeight.Bold),
      link = MaterialTheme.typography.body1.copy(
        textDecoration = TextDecoration.Underline
      )
    ),
    Modifier.padding(8.dp),
    // using CommonMarkFlavourDescriptor instead of GFMFlavourDescriptor because it shows `https://simplex.chat/` (link inside backticks) incorrectly
    flavour = CommonMarkFlavourDescriptor(),
    components = markdownComponents(
      heading2 = {
        Spacer(Modifier.height(10.dp))
        MarkdownHeader(it.content, it.node, it.typography.h2)
        Spacer(Modifier.height(5.dp))
      },
      heading3 = {
        Spacer(Modifier.height(10.dp))
        MarkdownHeader(it.content, it.node, it.typography.h3)
        Spacer(Modifier.height(3.dp))
      },
      heading4 = {
        Spacer(Modifier.height(10.dp))
        MarkdownHeader(it.content, it.node, it.typography.h4)
        Spacer(Modifier.height(4.dp))
      },
    ),
    padding = markdownPadding(block = 4.dp)
  )
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
    ReadableText(
      MR.strings.operator_conditions_will_be_applied,
      args = otherOperatorsToApply.value.joinToString(", ") { it.legalName_ }
    )
  }
}

@Composable
fun ConditionsLinkButton() {
  val showMenu = remember { mutableStateOf(false) }
  val uriHandler = LocalUriHandler.current
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  Column {
    DefaultDropdownMenu(showMenu, offset = if (oneHandUI.value) DpOffset(0.dp, -AppBarHeight * fontSizeSqrtMultiplier * 3) else DpOffset.Zero) {
      val commit = chatModel.conditions.value.currentConditions.conditionsCommit
      ItemAction(stringResource(MR.strings.operator_open_conditions), painterResource(MR.images.ic_draft), onClick = {
        val mdUrl = "https://github.com/simplex-chat/simplex-chat/blob/$commit/PRIVACY.md"
        uriHandler.openUriCatching(mdUrl)
        showMenu.value = false
      })
      ItemAction(stringResource(MR.strings.operator_open_changes), painterResource(MR.images.ic_more_horiz), onClick = {
        val commitUrl = "https://github.com/simplex-chat/simplex-chat/commit/$commit"
        uriHandler.openUriCatching(commitUrl)
        showMenu.value = false
      })
    }
    IconButton({ showMenu.value = true }) {
      Icon(painterResource(MR.images.ic_outbound), null, tint = MaterialTheme.colors.primary)
    }
  }
}

private fun internalUriHandler(parentUriHandler: UriHandler): UriHandler = object: UriHandler {
  override fun openUri(uri: String) {
    if (uri.startsWith("https://simplex.chat/contact#")) {
      openVerifiedSimplexUri(uri)
    } else {
      parentUriHandler.openUriCatching(uri)
    }
  }
}

private fun prepareMarkdown(text: String, parentLink: String): String {
  val localLinkRegex = Regex("\\[([^\\)]*)\\]\\(#.*\\)", RegexOption.MULTILINE)
  return text
    .replace("](/", "]($parentLink/")
    .replace("](./", "]($parentLink/")
    .replace(localLinkRegex) { it.groupValues.getOrNull(1) ?: it.value }
}

private fun changeOperatorEnabled(userServers: MutableState<List<UserOperatorServers>>, operatorIndex: Int, enabled: Boolean) {
  userServers.value = userServers.value.toMutableList().apply {
    this[operatorIndex] = this[operatorIndex].copy(
      operator = this[operatorIndex].operator?.copy(enabled = enabled)
    )
  }
}