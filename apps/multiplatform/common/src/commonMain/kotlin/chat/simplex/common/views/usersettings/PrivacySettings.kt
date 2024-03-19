package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.res.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.ProfileNameField
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.helpers.DatabaseUtils.ksAppPassword
import chat.simplex.common.views.helpers.DatabaseUtils.ksSelfDestructPassword
import chat.simplex.common.views.isValidDisplayName
import chat.simplex.common.views.localauth.SetAppPasscodeView
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*

enum class LAMode {
  SYSTEM,
  PASSCODE;

  val text: String
    get() = when (this) {
      SYSTEM -> generalGetString(MR.strings.la_mode_system)
      PASSCODE -> generalGetString(MR.strings.la_mode_passcode)
    }

  companion object {
    val default: LAMode
      get() = if (appPlatform == AppPlatform.ANDROID) SYSTEM else PASSCODE
  }
}

@Composable
fun PrivacySettingsView(
  chatModel: ChatModel,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean) -> Unit
) {
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    val simplexLinkMode = chatModel.controller.appPrefs.simplexLinkMode
    AppBarTitle(stringResource(MR.strings.your_privacy))
    PrivacyDeviceSection(showSettingsModal, setPerformLA)
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_chats)) {
      SettingsPreferenceItem(painterResource(MR.images.ic_lock), stringResource(MR.strings.encrypt_local_files), chatModel.controller.appPrefs.privacyEncryptLocalFiles, onChange = { enable ->
        withBGApi { chatModel.controller.apiSetEncryptLocalFiles(enable) }
      })
      SettingsPreferenceItem(painterResource(MR.images.ic_image), stringResource(MR.strings.auto_accept_images), chatModel.controller.appPrefs.privacyAcceptImages)
      SettingsPreferenceItem(painterResource(MR.images.ic_travel_explore), stringResource(MR.strings.send_link_previews), chatModel.controller.appPrefs.privacyLinkPreviews)
      SettingsPreferenceItem(
        painterResource(MR.images.ic_chat_bubble),
        stringResource(MR.strings.privacy_show_last_messages),
        chatModel.controller.appPrefs.privacyShowChatPreviews,
        onChange = { showPreviews ->
          chatModel.showChatPreviews.value = showPreviews
        }
      )
      SettingsPreferenceItem(
        painterResource(MR.images.ic_edit_note),
        stringResource(MR.strings.privacy_message_draft),
        chatModel.controller.appPrefs.privacySaveLastDraft,
        onChange = { saveDraft ->
          if (!saveDraft) {
            chatModel.draft.value = null
            chatModel.draftChatId.value = null
          }
        })
      SimpleXLinkOptions(chatModel.simplexLinkMode, onSelected = {
        simplexLinkMode.set(it)
        chatModel.simplexLinkMode.value = it
      })
    }

    val currentUser = chatModel.currentUser.value
    if (currentUser != null) {
      fun setSendReceiptsContacts(enable: Boolean, clearOverrides: Boolean) {
        withLongRunningApi(slow = 60_000) {
          val mrs = UserMsgReceiptSettings(enable, clearOverrides)
          chatModel.controller.apiSetUserContactReceipts(currentUser, mrs)
          chatModel.controller.appPrefs.privacyDeliveryReceiptsSet.set(true)
          chatModel.currentUser.value = currentUser.copy(sendRcptsContacts = enable)
          if (clearOverrides) {
            // For loop here is to prevent ConcurrentModificationException that happens with forEach
            for (i in 0 until chatModel.chats.size) {
              val chat = chatModel.chats[i]
              if (chat.chatInfo is ChatInfo.Direct) {
                var contact = chat.chatInfo.contact
                val sendRcpts = contact.chatSettings.sendRcpts
                if (sendRcpts != null && sendRcpts != enable) {
                  contact = contact.copy(chatSettings = contact.chatSettings.copy(sendRcpts = null))
                  chatModel.updateContact(currentUser.remoteHostId, contact)
                }
              }
            }
          }
        }
      }

      fun setSendReceiptsGroups(enable: Boolean, clearOverrides: Boolean) {
        withLongRunningApi(slow = 60_000) {
          val mrs = UserMsgReceiptSettings(enable, clearOverrides)
          chatModel.controller.apiSetUserGroupReceipts(currentUser, mrs)
          chatModel.controller.appPrefs.privacyDeliveryReceiptsSet.set(true)
          chatModel.currentUser.value = currentUser.copy(sendRcptsSmallGroups = enable)
          if (clearOverrides) {
            // For loop here is to prevent ConcurrentModificationException that happens with forEach
            for (i in 0 until chatModel.chats.size) {
              val chat = chatModel.chats[i]
              if (chat.chatInfo is ChatInfo.Group) {
                var groupInfo = chat.chatInfo.groupInfo
                val sendRcpts = groupInfo.chatSettings.sendRcpts
                if (sendRcpts != null && sendRcpts != enable) {
                  groupInfo = groupInfo.copy(chatSettings = groupInfo.chatSettings.copy(sendRcpts = null))
                  chatModel.updateGroup(currentUser.remoteHostId, groupInfo)
                }
              }
            }
          }
        }
      }

      if (!chatModel.desktopNoUserNoRemote) {
        SectionDividerSpaced()
        DeliveryReceiptsSection(
          currentUser = currentUser,
          setOrAskSendReceiptsContacts = { enable ->
            val contactReceiptsOverrides = chatModel.chats.fold(0) { count, chat ->
              if (chat.chatInfo is ChatInfo.Direct) {
                val sendRcpts = chat.chatInfo.contact.chatSettings.sendRcpts
                count + (if (sendRcpts == null || sendRcpts == enable) 0 else 1)
              } else {
                count
              }
            }
            if (contactReceiptsOverrides == 0) {
              setSendReceiptsContacts(enable, clearOverrides = false)
            } else {
              showUserContactsReceiptsAlert(enable, contactReceiptsOverrides, ::setSendReceiptsContacts)
            }
          },
          setOrAskSendReceiptsGroups = { enable ->
            val groupReceiptsOverrides = chatModel.chats.fold(0) { count, chat ->
              if (chat.chatInfo is ChatInfo.Group) {
                val sendRcpts = chat.chatInfo.groupInfo.chatSettings.sendRcpts
                count + (if (sendRcpts == null || sendRcpts == enable) 0 else 1)
              } else {
                count
              }
            }
            if (groupReceiptsOverrides == 0) {
              setSendReceiptsGroups(enable, clearOverrides = false)
            } else {
              showUserGroupsReceiptsAlert(enable, groupReceiptsOverrides, ::setSendReceiptsGroups)
            }
          }
        )
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun SimpleXLinkOptions(simplexLinkModeState: State<SimplexLinkMode>, onSelected: (SimplexLinkMode) -> Unit) {
  val modeValues = listOf(SimplexLinkMode.DESCRIPTION, SimplexLinkMode.FULL)
  val pickerValues = modeValues + if (modeValues.contains(simplexLinkModeState.value)) emptyList() else listOf(simplexLinkModeState.value)
  val values = remember {
    pickerValues.map {
      when (it) {
        SimplexLinkMode.DESCRIPTION -> it to generalGetString(MR.strings.simplex_link_mode_description)
        SimplexLinkMode.FULL -> it to generalGetString(MR.strings.simplex_link_mode_full)
        SimplexLinkMode.BROWSER -> it to generalGetString(MR.strings.simplex_link_mode_browser)
      }
    }
  }
  ExposedDropDownSettingRow(
    generalGetString(MR.strings.simplex_link_mode),
    values,
    simplexLinkModeState,
    icon = null,
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

@Composable
expect fun PrivacyDeviceSection(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean) -> Unit,
)

@Composable
private fun DeliveryReceiptsSection(
  currentUser: User,
  setOrAskSendReceiptsContacts: (Boolean) -> Unit,
  setOrAskSendReceiptsGroups: (Boolean) -> Unit,
) {
  SectionView(stringResource(MR.strings.settings_section_title_delivery_receipts)) {
    SettingsActionItemWithContent(painterResource(MR.images.ic_person), stringResource(MR.strings.receipts_section_contacts)) {
      DefaultSwitch(
        checked = currentUser.sendRcptsContacts ?: false,
        onCheckedChange = { enable ->
          setOrAskSendReceiptsContacts(enable)
        }
      )
    }
    SettingsActionItemWithContent(painterResource(MR.images.ic_group), stringResource(MR.strings.receipts_section_groups)) {
      DefaultSwitch(
        checked = currentUser.sendRcptsSmallGroups ?: false,
        onCheckedChange = { enable ->
          setOrAskSendReceiptsGroups(enable)
        }
      )
    }
  }
  SectionTextFooter(
    remember(currentUser.displayName) {
      buildAnnotatedString {
        append(generalGetString(MR.strings.receipts_section_description) + " ")
        withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
          append(currentUser.displayName)
        }
        append(".\n")
        append(generalGetString(MR.strings.receipts_section_description_1))
      }
    }
  )
}

private fun showUserContactsReceiptsAlert(
  enable: Boolean,
  contactReceiptsOverrides: Int,
  setSendReceiptsContacts: (Boolean, Boolean) -> Unit
) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(if (enable) MR.strings.receipts_contacts_title_enable else MR.strings.receipts_contacts_title_disable),
    text = AnnotatedString(String.format(generalGetString(if (enable) MR.strings.receipts_contacts_override_disabled else MR.strings.receipts_contacts_override_enabled), contactReceiptsOverrides)),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          setSendReceiptsContacts(enable, false)
        }) {
          val t = stringResource(if (enable) MR.strings.receipts_contacts_enable_keep_overrides else MR.strings.receipts_contacts_disable_keep_overrides)
          Text(t, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          setSendReceiptsContacts(enable, true)
        }
        ) {
          val t = stringResource(if (enable) MR.strings.receipts_contacts_enable_for_all else MR.strings.receipts_contacts_disable_for_all)
          Text(t, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.onBackground)
        }
      }
    }
  )
}

private fun showUserGroupsReceiptsAlert(
  enable: Boolean,
  groupReceiptsOverrides: Int,
  setSendReceiptsGroups: (Boolean, Boolean) -> Unit
) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(if (enable) MR.strings.receipts_groups_title_enable else MR.strings.receipts_groups_title_disable),
    text = AnnotatedString(String.format(generalGetString(if (enable) MR.strings.receipts_groups_override_disabled else MR.strings.receipts_groups_override_enabled), groupReceiptsOverrides)),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          setSendReceiptsGroups(enable, false)
        }) {
          val t = stringResource(if (enable) MR.strings.receipts_groups_enable_keep_overrides else MR.strings.receipts_groups_disable_keep_overrides)
          Text(t, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
          setSendReceiptsGroups(enable, true)
        }
        ) {
          val t = stringResource(if (enable) MR.strings.receipts_groups_enable_for_all else MR.strings.receipts_groups_disable_for_all)
          Text(t, Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
        }
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.onBackground)
        }
      }
    }
  )
}

private val laDelays = listOf(10, 30, 60, 180, 600, 0)

@Composable
fun SimplexLockView(
  chatModel: ChatModel,
  currentLAMode: SharedPreference<LAMode>,
  setPerformLA: (Boolean) -> Unit
) {
  val performLA = remember { chatModel.performLA }
  val laMode = remember { chatModel.controller.appPrefs.laMode.state }
  val laLockDelay = remember { chatModel.controller.appPrefs.laLockDelay }
  val showChangePasscode = remember { derivedStateOf { performLA.value && currentLAMode.state.value == LAMode.PASSCODE } }
  val selfDestructPref = remember { chatModel.controller.appPrefs.selfDestruct }
  val selfDestructDisplayName = remember { mutableStateOf(chatModel.controller.appPrefs.selfDestructDisplayName.get() ?: "") }
  val selfDestructDisplayNamePref = remember { chatModel.controller.appPrefs.selfDestructDisplayName }

  fun resetLAEnabled(onOff: Boolean) {
    chatModel.controller.appPrefs.performLA.set(onOff)
    chatModel.performLA.value = onOff
  }

  fun disableUnavailableLA() {
    resetLAEnabled(false)
    currentLAMode.set(LAMode.default)
    laUnavailableInstructionAlert()
  }

  fun resetSelfDestruct() {
    selfDestructPref.set(false)
    ksSelfDestructPassword.remove()
  }

  fun toggleLAMode(toLAMode: LAMode) {
    authenticate(
      if (toLAMode == LAMode.SYSTEM) {
        generalGetString(MR.strings.la_enter_app_passcode)
      } else {
        generalGetString(MR.strings.chat_lock)
      },
      generalGetString(MR.strings.change_lock_mode)
    ) { laResult ->
      when (laResult) {
        is LAResult.Error -> {
          laFailedAlert()
        }
        is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
        LAResult.Success -> {
          when (toLAMode) {
            LAMode.SYSTEM -> {
              authenticate(generalGetString(MR.strings.auth_enable_simplex_lock), promptSubtitle = "", usingLAMode = toLAMode) { laResult ->
                when (laResult) {
                  LAResult.Success -> {
                    currentLAMode.set(toLAMode)
                    ksAppPassword.remove()
                    resetSelfDestruct()
                    laTurnedOnAlert()
                  }
                  is LAResult.Unavailable, is LAResult.Error -> laFailedAlert()
                  is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
                }
              }
            }
            LAMode.PASSCODE -> {
              ModalManager.fullscreen.showCustomModal { close ->
                Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
                  SetAppPasscodeView(
                    submit = {
                      laLockDelay.set(30)
                      currentLAMode.set(toLAMode)
                      passcodeAlert(generalGetString(MR.strings.passcode_set))
                    },
                    cancel = {},
                    close = close
                  )
                }
              }
            }
          }
        }
        is LAResult.Unavailable -> disableUnavailableLA()
      }
    }
  }

  fun toggleSelfDestruct(selfDestruct: SharedPreference<Boolean>) {
    authenticate(generalGetString(MR.strings.la_current_app_passcode), generalGetString(MR.strings.change_self_destruct_mode)) { laResult ->
      when (laResult) {
        is LAResult.Error -> laFailedAlert()
        is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
        LAResult.Success -> {
          if (!selfDestruct.get()) {
            ModalManager.fullscreen.showCustomModal { close ->
              EnableSelfDestruct(selfDestruct, close)
            }
          } else {
            resetSelfDestruct()
          }
        }
        is LAResult.Unavailable -> disableUnavailableLA()
      }
    }
  }

  fun changeLAPassword() {
    authenticate(generalGetString(MR.strings.la_current_app_passcode), generalGetString(MR.strings.la_change_app_passcode)) { laResult ->
      when (laResult) {
        LAResult.Success -> {
          ModalManager.fullscreen.showCustomModal { close ->
            Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
              SetAppPasscodeView(
                reason = generalGetString(MR.strings.la_app_passcode),
                submit = {
                  passcodeAlert(generalGetString(MR.strings.passcode_changed))
                }, cancel = {
                  passcodeAlert(generalGetString(MR.strings.passcode_not_changed))
                }, close = close
              )
            }
          }
        }
        is LAResult.Error -> laFailedAlert()
        is LAResult.Failed -> {}
        is LAResult.Unavailable -> disableUnavailableLA()
      }
    }
  }

  fun changeSelfDestructPassword() {
    authenticate(generalGetString(MR.strings.la_current_app_passcode), generalGetString(MR.strings.change_self_destruct_passcode)) { laResult ->
      when (laResult) {
        LAResult.Success -> {
          ModalManager.fullscreen.showCustomModal { close ->
            Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
              SetAppPasscodeView(
                passcodeKeychain = ksSelfDestructPassword,
                prohibitedPasscodeKeychain = ksAppPassword,
                reason = generalGetString(MR.strings.self_destruct),
                submit = {
                  selfDestructPasscodeAlert(generalGetString(MR.strings.self_destruct_passcode_changed))
                }, cancel = {
                  passcodeAlert(generalGetString(MR.strings.passcode_not_changed))
                },
                close = close
              )
            }
          }
        }
        is LAResult.Error -> laFailedAlert()
        is LAResult.Failed -> {}
        is LAResult.Unavailable -> disableUnavailableLA()
      }
    }
  }

  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(MR.strings.chat_lock))
    SectionView {
      EnableLock(performLA) { performLAToggle ->
        performLA.value = performLAToggle
        chatModel.controller.appPrefs.laNoticeShown.set(true)
        if (performLAToggle) {
          when (currentLAMode.state.value) {
            LAMode.SYSTEM -> {
              setPerformLA(true)
            }
            LAMode.PASSCODE -> {
              ModalManager.fullscreen.showCustomModal { close ->
                Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
                  SetAppPasscodeView(
                    submit = {
                      laLockDelay.set(30)
                      chatModel.controller.appPrefs.performLA.set(true)
                      passcodeAlert(generalGetString(MR.strings.passcode_set))
                    },
                    cancel = {
                      resetLAEnabled(false)
                    },
                    close = close
                  )
                }
              }
            }
          }
        } else {
          setPerformLA(false)
        }
      }
      if (appPlatform == AppPlatform.ANDROID) {
        LockModeSelector(laMode) { newLAMode ->
          if (laMode.value == newLAMode) return@LockModeSelector
          if (chatModel.controller.appPrefs.performLA.get()) {
            toggleLAMode(newLAMode)
          } else {
            currentLAMode.set(newLAMode)
          }
        }
      }

      if (performLA.value) {
        LockDelaySelector(remember { laLockDelay.state }) { laLockDelay.set(it) }
        if (showChangePasscode.value && laMode.value == LAMode.PASSCODE) {
          SectionItemView({ changeLAPassword() }) {
            Text(
              generalGetString(MR.strings.la_change_app_passcode),
              color = MaterialTheme.colors.primary
            )
          }
        }
      }
      if (performLA.value && laMode.value == LAMode.PASSCODE) {
        SectionDividerSpaced()
        SectionView(stringResource(MR.strings.self_destruct_passcode).uppercase()) {
          val openInfo = {
            ModalManager.start.showModal {
              SelfDestructInfoView()
            }
          }
          SettingsActionItemWithContent(null, null, click = openInfo) {
            SharedPreferenceToggleWithIcon(
              stringResource(MR.strings.enable_self_destruct),
              painterResource(MR.images.ic_info),
              openInfo,
              remember { selfDestructPref.state }.value
            ) {
              toggleSelfDestruct(selfDestructPref)
            }
          }

          if (remember { selfDestructPref.state }.value) {
            Column(Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)) {
              Text(
                stringResource(MR.strings.self_destruct_new_display_name),
                fontSize = 16.sp,
                modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
              )
              ProfileNameField(selfDestructDisplayName, "", { isValidDisplayName(it.trim()) })
              LaunchedEffect(selfDestructDisplayName.value) {
                val new = selfDestructDisplayName.value
                if (isValidDisplayName(new) && selfDestructDisplayNamePref.get() != new) {
                  selfDestructDisplayNamePref.set(new)
                }
              }
            }
            SectionItemView({ changeSelfDestructPassword() }) {
              Text(
                stringResource(MR.strings.change_self_destruct_passcode),
                color = MaterialTheme.colors.primary
              )
            }
          }
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun SelfDestructInfoView() {
  ColumnWithScrollBar(
    Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(MR.strings.self_destruct), withPadding = false)
    ReadableText(stringResource(MR.strings.if_you_enter_self_destruct_code))
    Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
      TextListItem("1.", stringResource(MR.strings.all_app_data_will_be_cleared))
      TextListItem("2.", stringResource(MR.strings.app_passcode_replaced_with_self_destruct))
      TextListItem("3.", stringResource(MR.strings.empty_chat_profile_is_created))
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun EnableSelfDestruct(
  selfDestruct: SharedPreference<Boolean>,
  close: () -> Unit
) {
  Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
    SetAppPasscodeView(
      passcodeKeychain = ksSelfDestructPassword, prohibitedPasscodeKeychain = ksAppPassword, title = generalGetString(MR.strings.set_passcode), reason = generalGetString(MR.strings.enabled_self_destruct_passcode),
      submit = {
        selfDestruct.set(true)
        selfDestructPasscodeAlert(generalGetString(MR.strings.self_destruct_passcode_enabled))
      },
      cancel = {},
      close = close
    )
  }
}

@Composable
private fun EnableLock(performLA: MutableState<Boolean>, onCheckedChange: (Boolean) -> Unit) {
  SectionItemView {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Text(
        stringResource(MR.strings.enable_lock), Modifier
          .padding(end = 24.dp)
          .fillMaxWidth()
          .weight(1F)
      )
      DefaultSwitch(
        checked = performLA.value,
        onCheckedChange = onCheckedChange,
      )
    }
  }
}

@Composable
private fun LockModeSelector(state: State<LAMode>, onSelected: (LAMode) -> Unit) {
  val values by remember { mutableStateOf(LAMode.values().map { it to it.text }) }
  ExposedDropDownSettingRow(
    generalGetString(MR.strings.lock_mode),
    values,
    state,
    icon = null,
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

@Composable
private fun LockDelaySelector(state: State<Int>, onSelected: (Int) -> Unit) {
  val delays = remember { if (laDelays.contains(state.value)) laDelays else listOf(state.value) + laDelays }
  val values by remember { mutableStateOf(delays.map { it to laDelayText(it) }) }
  ExposedDropDownSettingRow(
    generalGetString(MR.strings.lock_after),
    values,
    state,
    icon = null,
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

@Composable
private fun TextListItem(n: String, text: String) {
  Box {
    Text(n)
    Text(text, Modifier.padding(start = 20.dp))
  }
}

private fun laDelayText(t: Int): String {
  val m = t / 60
  val s = t % 60
  return if (t == 0) {
    generalGetString(MR.strings.la_immediately)
  } else if (m == 0 || s != 0) {
    // there are no options where both minutes and seconds are needed
    generalGetString(MR.strings.la_seconds).format(s)
  } else {
    generalGetString(MR.strings.la_minutes).format(m)
  }
}

private fun passcodeAlert(title: String) {
  AlertManager.shared.showAlertMsg(
    title = title,
    text = generalGetString(MR.strings.la_please_remember_to_store_password)
  )
}

private fun selfDestructPasscodeAlert(title: String) {
  AlertManager.shared.showAlertMsg(title, generalGetString(MR.strings.if_you_enter_passcode_data_removed))
}

fun laTurnedOnAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.auth_simplex_lock_turned_on),
  generalGetString(MR.strings.auth_you_will_be_required_to_authenticate_when_you_start_or_resume)
)

fun laPasscodeNotSetAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.lock_not_enabled),
  generalGetString(MR.strings.you_can_turn_on_lock)
)

fun laFailedAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.la_auth_failed),
    text = generalGetString(MR.strings.la_could_not_be_verified)
  )
}

fun laUnavailableInstructionAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.auth_unavailable),
  generalGetString(MR.strings.auth_device_authentication_is_not_enabled_you_can_turn_on_in_settings_once_enabled)
)

fun laUnavailableTurningOffAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.auth_unavailable),
  generalGetString(MR.strings.auth_device_authentication_is_disabled_turning_off)
)
