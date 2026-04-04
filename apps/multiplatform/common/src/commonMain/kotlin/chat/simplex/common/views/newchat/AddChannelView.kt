package chat.simplex.common.views.newchat

import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.StrokeCap
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.getUserServers
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.*
import chat.simplex.common.views.chat.group.GroupLinkView
import chat.simplex.common.views.chatlist.openGroupChat
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.networkAndServers.NetworkAndServersView
import chat.simplex.common.views.chat.group.hostFromRelayLink
import chat.simplex.res.MR
import java.net.URI
import dev.icerock.moko.resources.compose.painterResource
import kotlinx.coroutines.*

@Composable
fun AddChannelView(chatModel: ChatModel, close: () -> Unit, closeAll: () -> Unit) {
  val view = LocalMultiplatformView()
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val scope = rememberCoroutineScope()
  val displayName = rememberSaveable { mutableStateOf("") }
  val chosenImage = rememberSaveable { mutableStateOf<URI?>(null) }
  val profileImage = rememberSaveable { mutableStateOf<String?>(null) }
  val focusRequester = remember { FocusRequester() }
  val hasRelays = rememberSaveable { mutableStateOf(true) }
  val groupInfo = remember { mutableStateOf<GroupInfo?>(null) }
  val groupLink = rememberSaveable(stateSaver = GroupLink.nullableStateSaver) { mutableStateOf<GroupLink?>(null) }
  val groupRelays = remember { mutableStateOf<List<GroupRelay>>(emptyList()) }
  val creationInProgress = rememberSaveable { mutableStateOf(false) }
  val showLinkStep = rememberSaveable { mutableStateOf(false) }
  val relayListExpanded = rememberSaveable { mutableStateOf(false) }

  val gInfo = groupInfo.value
  if (showLinkStep.value && gInfo != null) {
    LinkStepView(chatModel, gInfo, groupLink, closeAll)
  } else if (gInfo != null) {
    ProgressStepView(
      chatModel, gInfo, groupRelays, relayListExpanded,
      onLinkReady = if (appPlatform.isDesktop) {
        {
          chatModel.creatingChannelId.value = null
          closeAll()
          withBGApi {
            openGroupChat(null, gInfo.groupId)
            ModalManager.end.showModalCloseable(true) { close ->
              GroupLinkView(chatModel, rhId = null, groupInfo = gInfo, groupLink = groupLink.value, onGroupLinkUpdated = null, creatingGroup = true, isChannel = true, close = close)
            }
          }
        }
      } else {
        { showLinkStep.value = true }
      },
      cancelChannelCreation = {
        chatModel.creatingChannelId.value = null
        ChannelRelaysModel.reset()
        closeAll()
        withBGApi {
          try {
            chatModel.controller.apiDeleteChat(rh = null, type = ChatType.Group, id = gInfo.apiId)
            withContext(Dispatchers.Main) {
              chatModel.chatsContext.removeChat(null, gInfo.id)
            }
          } catch (e: Exception) {
            Log.e(TAG, "cancelChannelCreation error: ${e.message}")
          }
        }
      }
    )
  } else {
    ProfileStepView(
      chatModel = chatModel,
      displayName = displayName,
      profileImage = profileImage,
      chosenImage = chosenImage,
      focusRequester = focusRequester,
      hasRelays = hasRelays,
      creationInProgress = creationInProgress,
      bottomSheetModalState = bottomSheetModalState,
      scope = scope,
      view = view,
      close = close,
      createChannel = {
        hideKeyboard(view)
        val trimmedName = displayName.value.trim()
        displayName.value = trimmedName
        val profile = GroupProfile(
          displayName = trimmedName,
          fullName = "",
          shortDescr = null,
          image = profileImage.value,
          groupPreferences = GroupPreferences(history = GroupPreference(GroupFeatureEnabled.ON))
        )
        creationInProgress.value = true
        withBGApi {
          try {
            val enabledRelays = chooseRandomRelays()
            val relayIds = enabledRelays.mapNotNull { it.chatRelayId }
            if (relayIds.isEmpty()) {
              withContext(Dispatchers.Main) {
                creationInProgress.value = false
                hasRelays.value = false
              }
              return@withBGApi
            }
            val result = chatModel.controller.apiNewPublicGroup(
              rh = null,
              incognito = false,
              relayIds = relayIds,
              groupProfile = profile
            )
            if (result != null) {
              val (gI, gL, gR) = result
              withContext(Dispatchers.Main) {
                chatModel.chatsContext.updateGroup(rhId = null, gI)
                chatModel.creatingChannelId.value = gI.id
                groupInfo.value = gI
                groupLink.value = gL
                groupRelays.value = gR.sortedBy { relayDisplayName(it) }
                ChannelRelaysModel.set(gI.groupId, gR)
                creationInProgress.value = false
              }
            } else {
              withContext(Dispatchers.Main) { creationInProgress.value = false }
            }
          } catch (e: Exception) {
            withContext(Dispatchers.Main) {
              creationInProgress.value = false
              AlertManager.shared.showAlertMsg(
                title = generalGetString(MR.strings.error_creating_channel),
                text = e.message
              )
            }
          }
        }
      }
    )
  }
}

private const val maxRelays = 3

private suspend fun chooseRandomRelays(): List<UserChatRelay> {
  val servers = getUserServers(rh = null) ?: return emptyList()
  // Operator relays are grouped per operator; custom relays (null operator)
  // are treated independently to maximize trust distribution.
  val operatorGroups = mutableListOf<List<UserChatRelay>>()
  var customRelays = mutableListOf<UserChatRelay>()
  for (op in servers) {
    val relays = op.chatRelays.filter { it.enabled && !it.deleted && it.chatRelayId != null }
    if (relays.isEmpty()) continue
    if (op.operator != null) {
      operatorGroups.add(relays.shuffled())
    } else {
      customRelays = relays.shuffled().toMutableList()
    }
  }
  val selected = mutableListOf<UserChatRelay>()
  // Prefer at least one custom relay when available -
  // user's own infrastructure for trust distribution.
  if (customRelays.isNotEmpty()) {
    selected.add(customRelays.removeAt(0))
    if (selected.size >= maxRelays) return selected
  }
  // Round-robin across shuffled groups to distribute relays across operators.
  val groups = (operatorGroups + customRelays.map { listOf(it) }).shuffled()
  val maxDepth = groups.maxOfOrNull { it.size } ?: 0
  for (depth in 0 until maxDepth) {
    for (group in groups) {
      if (depth < group.size) {
        selected.add(group[depth])
        if (selected.size >= maxRelays) return selected
      }
    }
  }
  return selected
}

private suspend fun checkHasRelays(): Boolean {
  val servers = try { getUserServers(rh = null) } catch (_: Exception) { null } ?: return false
  return servers.any { op ->
    op.chatRelays.any { it.enabled && !it.deleted && it.chatRelayId != null }
  }
}

@Composable
private fun ProfileStepView(
  chatModel: ChatModel,
  displayName: MutableState<String>,
  profileImage: MutableState<String?>,
  chosenImage: MutableState<URI?>,
  focusRequester: FocusRequester,
  hasRelays: MutableState<Boolean>,
  creationInProgress: MutableState<Boolean>,
  bottomSheetModalState: ModalBottomSheetState,
  scope: CoroutineScope,
  view: Any?,
  close: () -> Unit,
  createChannel: () -> Unit
) {
  LaunchedEffect(Unit) {
    hasRelays.value = checkHasRelays()
  }

  ModalBottomSheetLayout(
    scrimColor = Color.Black.copy(alpha = 0.12F),
    modifier = Modifier.imePadding(),
    sheetContent = {
      GetImageBottomSheet(
        chosenImage,
        onImageChange = { bitmap -> profileImage.value = resizeImageToStrSize(cropToSquare(bitmap), maxDataSize = 12500) },
        hideBottomSheet = {
          scope.launch { bottomSheetModalState.hide() }
        }
      )
    },
    sheetState = bottomSheetModalState,
    sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp)
  ) {
    ModalView(close = close) {
      ColumnWithScrollBar {
        AppBarTitle(generalGetString(MR.strings.create_channel_title))
        Box(
          Modifier
            .fillMaxWidth()
            .padding(bottom = 24.dp),
          contentAlignment = Alignment.Center
        ) {
          Box(contentAlignment = Alignment.TopEnd) {
            Box(contentAlignment = Alignment.Center) {
              ProfileImage(108.dp, image = profileImage.value)
              EditImageButton { scope.launch { bottomSheetModalState.show() } }
            }
            if (profileImage.value != null) {
              DeleteImageButton { profileImage.value = null }
            }
          }
        }
        Row(
          Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING_HALF).fillMaxWidth(),
          horizontalArrangement = Arrangement.SpaceBetween
        ) {
          Text(
            generalGetString(MR.strings.channel_display_name_field),
            fontSize = 16.sp
          )
          if (!isValidDisplayName(displayName.value.trim())) {
            Spacer(Modifier.size(DEFAULT_PADDING_HALF))
            IconButton({ showInvalidNameAlert(mkValidName(displayName.value.trim()), displayName) }, Modifier.size(20.dp)) {
              Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.error)
            }
          }
        }
        Box(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          ProfileNameField(displayName, "", { isValidDisplayName(it.trim()) }, focusRequester)
        }
        Spacer(Modifier.height(8.dp))

        SettingsActionItem(
          painterResource(MR.images.ic_wifi_tethering),
          generalGetString(MR.strings.configure_relays),
          click = {
            ModalManager.start.showCustomModal { close ->
              NetworkAndServersView(close)
            }
          },
          textColor = if (hasRelays.value) MaterialTheme.colors.primary else WarningOrange,
          iconColor = if (hasRelays.value) MaterialTheme.colors.primary else WarningOrange
        )

        val canCreate = canCreateProfile(displayName.value) && hasRelays.value && !creationInProgress.value
        SettingsActionItem(
          painterResource(MR.images.ic_check),
          generalGetString(MR.strings.create_channel_button),
          click = createChannel,
          textColor = MaterialTheme.colors.primary,
          iconColor = MaterialTheme.colors.primary,
          disabled = !canCreate
        )

        SectionTextFooter(
          if (!hasRelays.value) {
            generalGetString(MR.strings.enable_at_least_one_chat_relay)
          } else {
            val name = chatModel.currentUser.value?.displayName ?: ""
            String.format(generalGetString(MR.strings.your_profile_shared_with_channel_relays), name)
          }
        )

        LaunchedEffect(Unit) {
          delay(1000)
          focusRequester.requestFocus()
        }
      }
    }
  }
}

@Composable
private fun ProgressStepView(
  chatModel: ChatModel,
  gInfo: GroupInfo,
  groupRelays: MutableState<List<GroupRelay>>,
  relayListExpanded: MutableState<Boolean>,
  onLinkReady: () -> Unit,
  cancelChannelCreation: () -> Unit
) {
  val failedCount = groupRelays.value.count { relayMemberConnFailed(chatModel, it) != null }
  val activeCount = groupRelays.value.count { it.relayStatus == RelayStatus.RsActive && relayMemberConnFailed(chatModel, it) == null }
  val total = groupRelays.value.size

  if (appPlatform.isDesktop) {
    DisposableEffect(Unit) {
      chatModel.centerPanelBackgroundClickHandler = {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.cancel_creating_channel_question),
          confirmText = generalGetString(MR.strings.cancel_creating_channel_confirm),
          onConfirm = cancelChannelCreation,
          dismissText = generalGetString(MR.strings.wait_verb),
          destructive = true,
        )
        true
      }
      onDispose {
        chatModel.centerPanelBackgroundClickHandler = null
      }
    }
  }

  LaunchedEffect(gInfo.groupId) {
    snapshotFlow { ChannelRelaysModel.groupRelays.toList() }
      .collect { relays ->
        if (ChannelRelaysModel.groupId.value != gInfo.groupId) return@collect
        groupRelays.value = relays.sortedBy { relayDisplayName(it) }
        if (relays.all { it.relayStatus == RelayStatus.RsActive && relayMemberConnFailed(chatModel, it) == null }) {
          onLinkReady()
          ChannelRelaysModel.reset()
        }
      }
  }

  ModalView(
    close = cancelChannelCreation,
    showClose = false,
    endButtons = {
      TextButton(onClick = cancelChannelCreation) {
        Text(generalGetString(MR.strings.cancel_verb))
      }
    }
  ) {
    ColumnWithScrollBar {
      AppBarTitle(generalGetString(MR.strings.creating_channel))

      Box(
        Modifier.fillMaxWidth().padding(bottom = 8.dp),
        contentAlignment = Alignment.Center
      ) {
        ProfileImage(108.dp, image = gInfo.groupProfile.image)
      }
      Text(
        gInfo.groupProfile.displayName,
        style = MaterialTheme.typography.h6,
        modifier = Modifier.fillMaxWidth().padding(bottom = 16.dp),
        textAlign = TextAlign.Center
      )

      SectionView {
        SectionItemView(click = { relayListExpanded.value = !relayListExpanded.value }) {
          Row(
            Modifier.fillMaxWidth(),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp)
          ) {
            if (activeCount + failedCount < total) {
              RelayProgressIndicator(active = activeCount, total = total)
            }
            val statusText = if (failedCount > 0) {
              String.format(generalGetString(MR.strings.relay_bar_active_with_failures), activeCount, total, failedCount)
            } else {
              String.format(generalGetString(MR.strings.relay_bar_active), activeCount, total)
            }
            Text(statusText, modifier = Modifier.weight(1f))
            Icon(
              painterResource(if (relayListExpanded.value) MR.images.ic_chevron_up else MR.images.ic_chevron_down),
              contentDescription = null,
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier.size(20.dp)
            )
          }
        }
        if (relayListExpanded.value) {
          groupRelays.value.forEach { relay ->
            val failedErr = relayMemberConnFailed(chatModel, relay)
            if (failedErr != null) {
              SectionItemView(
                click = {
                  AlertManager.shared.showAlertMsg(
                    title = generalGetString(MR.strings.relay_connection_failed),
                    text = failedErr
                  )
                },
                minHeight = 30.dp,
                padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = 4.dp)
              ) {
                RelayRow(relay, connFailed = true)
              }
            } else {
              SectionItemView(
                minHeight = 30.dp,
                padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = 4.dp)
              ) {
                RelayRow(relay, connFailed = false)
              }
            }
          }
        }
      }

      Spacer(Modifier.height(16.dp))

      SectionView {
        val enabled = activeCount > 0
        SettingsActionItem(
          painterResource(MR.images.ic_link),
          generalGetString(MR.strings.channel_link),
          click = {
            if (activeCount >= total) {
              onLinkReady()
            } else if (activeCount > 0) {
              val alertText = String.format(
                generalGetString(MR.strings.channel_will_start_with_relays),
                activeCount, total
              )
              if (activeCount + failedCount < total) {
                AlertManager.shared.showAlertDialogButtons(
                  title = generalGetString(MR.strings.not_all_relays_connected),
                  text = alertText,
                  buttons = {
                    Row(Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF), horizontalArrangement = Arrangement.SpaceBetween) {
                      TextButton(onClick = { AlertManager.shared.hideAlert() }) {
                        Text(generalGetString(MR.strings.wait_verb))
                      }
                      TextButton(onClick = {
                        AlertManager.shared.hideAlert()
                        onLinkReady()
                      }) {
                        Text(generalGetString(MR.strings.proceed_verb))
                      }
                    }
                  }
                )
              } else {
                AlertManager.shared.showAlertDialog(
                  title = generalGetString(MR.strings.not_all_relays_connected),
                  text = alertText,
                  confirmText = generalGetString(MR.strings.proceed_verb),
                  onConfirm = { onLinkReady() }
                )
              }
            }
          },
          textColor = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
          iconColor = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
          disabled = !enabled
        )
      }
    }
  }
}

private fun relayMemberConnFailed(chatModel: ChatModel, relay: GroupRelay): String? {
  return chatModel.groupMembers.value
    .firstOrNull { it.groupMemberId == relay.groupMemberId }
    ?.activeConn?.connFailedErr
}

@Composable
private fun RelayRow(relay: GroupRelay, connFailed: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Text(relayDisplayName(relay))
    RelayStatusIndicator(relay.relayStatus, connFailed = connFailed)
  }
}

@Composable
private fun LinkStepView(
  chatModel: ChatModel,
  gInfo: GroupInfo,
  groupLink: MutableState<GroupLink?>,
  closeAll: () -> Unit
) {
  val close: () -> Unit = {
    chatModel.creatingChannelId.value = null
    withBGApi {
      delay(500)
      withContext(Dispatchers.Main) {
        ModalManager.start.closeModals()
        openGroupChat(null, gInfo.groupId)
      }
    }
  }
  ModalView(close = close, showClose = false) {
    GroupLinkView(
      chatModel = chatModel,
      rhId = null,
      groupInfo = gInfo,
      groupLink = groupLink.value,
      onGroupLinkUpdated = { groupLink.value = it },
      creatingGroup = true,
      isChannel = true,
      close = close
    )
  }
}

fun relayDisplayName(relay: GroupRelay): String {
  if (relay.userChatRelay.displayName.isNotEmpty()) return relay.userChatRelay.displayName
  relay.userChatRelay.domains.firstOrNull()?.let { return it }
  relay.relayLink?.let { return hostFromRelayLink(it) }
  return "relay ${relay.groupRelayId}"
}


@Composable
fun RelayStatusIndicator(status: RelayStatus, connFailed: Boolean = false) {
  val color = if (connFailed) Color.Red else if (status == RelayStatus.RsActive) Color.Green else WarningYellow
  val text = if (connFailed) generalGetString(MR.strings.relay_status_failed) else status.text
  Row(
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Canvas(Modifier.size(8.dp)) {
      drawCircle(color = color)
    }
    Text(
      text,
      fontSize = 12.sp,
      color = MaterialTheme.colors.secondary
    )
    if (connFailed) {
      Icon(
        painterResource(MR.images.ic_error),
        contentDescription = null,
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.size(14.dp)
      )
    }
  }
}

@Composable
fun RelayProgressIndicator(active: Int, total: Int) {
  if (active == 0) {
    CircularProgressIndicator(
      Modifier.size(20.dp),
      strokeWidth = 2.5.dp
    )
  } else {
    val progress = active.toFloat() / total.coerceAtLeast(1).toFloat()
    Box(Modifier.size(20.dp)) {
      Canvas(Modifier.fillMaxSize()) {
        // Background circle
        drawCircle(
          color = Color.Gray.copy(alpha = 0.3f),
          style = Stroke(width = 2.5.dp.toPx())
        )
        // Progress arc
        drawArc(
          color = Color(0xFF2196F3), // accent blue
          startAngle = -90f,
          sweepAngle = 360f * progress,
          useCenter = false,
          style = Stroke(width = 2.5.dp.toPx(), cap = StrokeCap.Round)
        )
      }
    }
  }
}

@Preview
@Composable
fun PreviewAddChannelView() {
  SimpleXTheme {
    AddChannelView(chatModel = ChatModel, close = {}, closeAll = {})
  }
}
