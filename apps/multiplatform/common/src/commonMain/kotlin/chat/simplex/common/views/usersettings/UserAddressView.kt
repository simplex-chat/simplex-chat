package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import SectionViewWithButton
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.style.TextAlign
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR

@Composable
fun UserAddressView(
  chatModel: ChatModel,
  shareViaProfile: Boolean = false,
  autoCreateAddress: Boolean = false,
  close: () -> Unit
) {
  // TODO close when remote host changes
  val shareViaProfile = remember { mutableStateOf(shareViaProfile) }
  val progressIndicator = remember { mutableStateOf(false) }
  val user = remember { chatModel.currentUser }
  val clipboard = LocalClipboardManager.current
  KeyChangeEffect(user.value?.remoteHostId, user.value?.userId) {
    close()
  }

  fun setProfileAddress(on: Boolean) {
    progressIndicator.value = true
    withBGApi {
      try {
        val u = chatModel.controller.apiSetProfileAddress(user.value?.remoteHostId, on)
        if (u != null) {
          chatModel.updateUser(u)
        }
      } catch (e: Exception) {
        Log.e(TAG, "UserAddressView apiSetProfileAddress: ${e.stackTraceToString()}")
      } finally {
        progressIndicator.value = false
      }
    }
  }

  fun createAddress() {
    withBGApi {
      progressIndicator.value = true
      val connReqContact = chatModel.controller.apiCreateUserAddress(user.value?.remoteHostId)
      if (connReqContact != null) {
        val slDataSet = connReqContact.connShortLink != null
        chatModel.userAddress.value = UserContactLinkRec(
          connReqContact,
          shortLinkDataSet = slDataSet,
          shortLinkLargeDataSet = slDataSet,
          addressSettings = AddressSettings(businessAddress = false, autoAccept = null, autoReply = null)
        )

        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.share_address_with_contacts_question),
          text = generalGetString(MR.strings.add_address_to_your_profile),
          confirmText = generalGetString(MR.strings.share_verb),
          onConfirm = {
            setProfileAddress(true)
            shareViaProfile.value = true
          }
        )
      }
      progressIndicator.value = false
    }
  }

  fun share(userAddress: String) { clipboard.shareText(userAddress) }

  LaunchedEffect(autoCreateAddress) {
    if (chatModel.userAddress.value == null && autoCreateAddress) {
      createAddress()
    }
  }
  val userAddress = remember { chatModel.userAddress }
  val uriHandler = LocalUriHandler.current
  val showLayout = @Composable {
    UserAddressLayout(
      user = user.value,
      userAddress = userAddress.value,
      shareViaProfile,
      createAddress = ::createAddress,
      showAddShortLinkAlert = { shareAddress: (() -> Unit)? ->
        showAddShortLinkAlert(progressIndicator = progressIndicator, share = ::share, shareAddress = shareAddress)
      },
      learnMore = {
        ModalManager.start.showModal {
          UserAddressLearnMore()
        }
      },
      share = ::share,
      sendEmail = { userAddress ->
        uriHandler.sendEmail(
          generalGetString(MR.strings.email_invite_subject),
          generalGetString(MR.strings.email_invite_body).format(simplexChatLink(userAddress.connLinkContact.connFullLink)) // TODO [short links] replace with short link
        )
      },
      setProfileAddress = ::setProfileAddress,
      deleteAddress = {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.delete_address__question),
          text = if (shareViaProfile.value) generalGetString(MR.strings.all_your_contacts_will_remain_connected_update_sent) else generalGetString(MR.strings.all_your_contacts_will_remain_connected),
          confirmText = generalGetString(MR.strings.delete_verb),
          onConfirm = {
            progressIndicator.value = true
            withBGApi {
              val u = chatModel.controller.apiDeleteUserAddress(user.value?.remoteHostId)
              if (u != null) {
                chatModel.userAddress.value = null
                chatModel.updateUser(u)
                shareViaProfile.value = false
                progressIndicator.value = false
              }
            }
          },
          destructive = true,
        )
      },
      saveAddressSettings = { settings: AddressSettingsState, savedSettings: MutableState<AddressSettingsState> ->
        withBGApi {
          val address = chatModel.controller.apiSetUserAddressSettings(user.value?.remoteHostId, settings.addressSettings)
          if (address != null) {
            chatModel.userAddress.value = address
            savedSettings.value = settings
          }
        }
      },
    )
  }

  ModalView(close = close) {
    showLayout()
  }

  if (progressIndicator.value) {
    Box(
      Modifier.fillMaxSize(),
      contentAlignment = Alignment.Center
    ) {
      if (userAddress.value != null) {
        Surface(Modifier.size(50.dp), color = MaterialTheme.colors.background.copy(0.9f), contentColor = LocalContentColor.current, shape = RoundedCornerShape(50)){}
      }
      CircularProgressIndicator(
        Modifier
          .padding(horizontal = 2.dp)
          .size(30.dp),
        color = MaterialTheme.colors.secondary,
        strokeWidth = 3.dp
      )
    }
  }
}

private fun addShortLink(
  progressIndicator: MutableState<Boolean>,
  share: (String) -> Unit,
  shareOnCompletion: Boolean = false
) {
  withBGApi {
    progressIndicator.value = true
    val userAddress = chatModel.controller.apiAddMyAddressShortLink(chatModel.currentUser.value?.remoteHostId)
    if (userAddress != null) {
      chatModel.userAddress.value = userAddress
      if (shareOnCompletion) {
        share(userAddress.connLinkContact.simplexChatUri(short = true))
      }
    }
    progressIndicator.value = false
  }
}

fun showAddShortLinkAlert(
  progressIndicator: MutableState<Boolean>,
  share: (String) -> Unit,
  shareAddress: (() -> Unit)? = null
) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.share_profile_via_link),
    text = generalGetString(MR.strings.share_profile_via_link_alert_text),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          addShortLink(progressIndicator = progressIndicator, share = share, shareOnCompletion = shareAddress != null)
        }) {
          Text(
            generalGetString(MR.strings.share_profile_via_link_alert_confirm),
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.primary
          )
        }

        if (shareAddress != null) {
          SectionItemView({
            AlertManager.shared.hideAlert()
            shareAddress()
          }) {
            Text(
              generalGetString(MR.strings.share_old_address_alert_button),
              Modifier.fillMaxWidth(),
              textAlign = TextAlign.Center,
              color = MaterialTheme.colors.primary
            )
          }
        }
        // Cancel
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(
            stringResource(MR.strings.cancel_verb),
            Modifier.fillMaxWidth(),
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.primary
          )
        }
      }
    }
  )
}

@Composable
private fun UserAddressLayout(
  user: User?,
  userAddress: UserContactLinkRec?,
  shareViaProfile: MutableState<Boolean>,
  createAddress: () -> Unit,
  showAddShortLinkAlert: ((() -> Unit)?) -> Unit,
  learnMore: () -> Unit,
  share: (String) -> Unit,
  sendEmail: (UserContactLinkRec) -> Unit,
  setProfileAddress: (Boolean) -> Unit,
  deleteAddress: () -> Unit,
  saveAddressSettings: (AddressSettingsState, MutableState<AddressSettingsState>) -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.simplex_address), hostDevice(user?.remoteHostId))
    Column(
      Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING_HALF),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      if (userAddress == null) {
        SectionView(generalGetString(MR.strings.for_social_media).uppercase()) {
          CreateAddressButton(createAddress)
        }

        SectionDividerSpaced()
        SectionView(generalGetString(MR.strings.or_to_share_privately).uppercase()) {
          CreateOneTimeLinkButton()
        }

        SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
        SectionView {
          LearnMoreButton(learnMore)
        }
      } else {
        val addressSettingsState = remember { mutableStateOf(AddressSettingsState(settings = userAddress.addressSettings)) }
        val savedAddressSettingsState = remember { mutableStateOf(addressSettingsState.value) }
        val showShortLink = remember { mutableStateOf(true) }

        SectionViewWithButton(
          stringResource(MR.strings.for_social_media).uppercase(),
          titleButton = if (userAddress.connLinkContact.connShortLink != null) {{ ToggleShortLinkButton(showShortLink) }} else null
        ) {
          SimpleXCreatedLinkQRCode(userAddress.connLinkContact, short = showShortLink.value)
          if (userAddress.shouldBeUpgraded) {
            AddShortLinkButton(text = stringResource(MR.strings.add_short_link)) { showAddShortLinkAlert(null) }
          }
          ShareAddressButton {
            if (userAddress.shouldBeUpgraded) {
              showAddShortLinkAlert { share(userAddress.connLinkContact.simplexChatUri(short = showShortLink.value)) }
            } else {
              share(userAddress.connLinkContact.simplexChatUri(short = showShortLink.value))
            }
          }
          // ShareViaEmailButton { sendEmail(userAddress) }
          BusinessAddressToggle(addressSettingsState) { saveAddressSettings(addressSettingsState.value, savedAddressSettingsState) }
          AddressSettingsButton(user, userAddress, shareViaProfile, setProfileAddress, saveAddressSettings)

          if (addressSettingsState.value.businessAddress) {
            SectionTextFooter(stringResource(MR.strings.add_your_team_members_to_conversations))
          }
        }

        SectionDividerSpaced(maxTopPadding = addressSettingsState.value.businessAddress)
        SectionView(generalGetString(MR.strings.or_to_share_privately).uppercase()) {
          CreateOneTimeLinkButton()
        }
        SectionDividerSpaced(maxBottomPadding = false)
        SectionView {
          LearnMoreButton(learnMore)
        }

        SectionDividerSpaced(maxBottomPadding = false)
        SectionView {
          DeleteAddressButton(deleteAddress)
          SectionTextFooter(stringResource(MR.strings.your_contacts_will_remain_connected))
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun CreateAddressButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_qr_code),
    stringResource(MR.strings.create_simplex_address),
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
private fun AddShortLinkButton(text: String, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_arrow_upward),
    text,
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
private fun CreateOneTimeLinkButton() {
  val closeAll = { ModalManager.start.closeModals() }
  SettingsActionItem(
    painterResource(MR.images.ic_add_link),
    stringResource(MR.strings.create_1_time_link),
    click = {
      ModalManager.start.showModalCloseable(endButtons = { AddContactLearnMoreButton() }) { _ ->
        NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = closeAll)
      }
    },
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
private fun LearnMoreButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_info),
    stringResource(MR.strings.simplex_address_or_1_time_link),
    onClick,
  )
}

@Composable
fun ShareViaEmailButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_mail),
    stringResource(MR.strings.invite_friends),
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
private fun AddressSettingsButton(
  user: User?,
  userAddress: UserContactLinkRec,
  shareViaProfile: MutableState<Boolean>,
  setProfileAddress: (Boolean) -> Unit,
  saveAddressSettings: (AddressSettingsState, MutableState<AddressSettingsState>) -> Unit,
) {
  SettingsActionItem(
    painterResource(MR.images.ic_settings),
    stringResource(MR.strings.address_settings),
    click = {
      ModalManager.start.showCustomModal { close ->
        UserAddressSettings(user, userAddress, shareViaProfile, setProfileAddress, saveAddressSettings, close = close)
      }
    }
  )
}

@Composable
private fun ModalData.UserAddressSettings(
  user: User?,
  userAddress: UserContactLinkRec,
  shareViaProfile: MutableState<Boolean>,
  setProfileAddress: (Boolean) -> Unit,
  saveAddressSettings: (AddressSettingsState, MutableState<AddressSettingsState>) -> Unit,
  close: () -> Unit
) {
  val addressSettingsState = remember { stateGetOrPut("autoAcceptState") { (AddressSettingsState(userAddress.addressSettings)) } }
  val savedAddressSettingsState = remember { stateGetOrPut("autoAcceptStateSaved") { (addressSettingsState.value) } }

  fun onClose(close: () -> Unit): Boolean = if (addressSettingsState.value == savedAddressSettingsState.value) {
    chatModel.centerPanelBackgroundClickHandler = null
    close()
    false
  } else {
    showUnsavedChangesAlert(
      save = {
        saveAddressSettings(addressSettingsState.value, savedAddressSettingsState)
        chatModel.centerPanelBackgroundClickHandler = null
        close()
      },
      revert = {
        chatModel.centerPanelBackgroundClickHandler = null
        close()
      }
    )
    true
  }

  LaunchedEffect(Unit) {
    // Enables unsaved changes alert on this view.
    chatModel.centerPanelBackgroundClickHandler = {
      onClose(close = { ModalManager.start.closeModals() })
    }
  }

  ModalView(close = { onClose(close) }) {
    ColumnWithScrollBar {
      AppBarTitle(stringResource(MR.strings.address_settings), hostDevice(user?.remoteHostId))
      Column(
        Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING_HALF),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.SpaceEvenly
      ) {
        SectionView {
          ShareWithContactsButton(shareViaProfile, setProfileAddress)
          AutoAcceptToggle(addressSettingsState) { saveAddressSettings(addressSettingsState.value, savedAddressSettingsState) }
          if (addressSettingsState.value.autoAccept && !chatModel.addressShortLinkDataSet() && !addressSettingsState.value.businessAddress) {
            AcceptIncognitoToggle(addressSettingsState)
          }
        }
        SectionDividerSpaced()

        SectionView(stringResource(MR.strings.address_welcome_message).uppercase()) {
          AutoReplyEditor(addressSettingsState)
        }
        SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)

        saveAddressSettingsButton(addressSettingsState.value == savedAddressSettingsState.value) {
          saveAddressSettings(addressSettingsState.value, savedAddressSettingsState)
        }
      }
    }
  }
}

@Composable
fun ShareWithContactsButton(shareViaProfile: MutableState<Boolean>, setProfileAddress: (Boolean) -> Unit) {
  PreferenceToggleWithIcon(
    stringResource(MR.strings.share_with_contacts),
    painterResource(MR.images.ic_person),
    checked = shareViaProfile.value,
  ) { on ->
    shareViaProfile.value = on
    if (on) {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.share_address_with_contacts_question),
        text = generalGetString(MR.strings.profile_update_will_be_sent_to_contacts),
        confirmText = generalGetString(MR.strings.share_verb),
        onConfirm = {
          setProfileAddress(on)
        },
        onDismiss = {
          shareViaProfile.value = !on
        },
        onDismissRequest = {
          shareViaProfile.value = !on
        })
    } else {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.stop_sharing_address),
        text = generalGetString(MR.strings.profile_update_will_be_sent_to_contacts),
        confirmText = generalGetString(MR.strings.stop_sharing),
        onConfirm = {
          setProfileAddress(on)
        },
        onDismiss = {
          shareViaProfile.value = !on
        },
        onDismissRequest = {
          shareViaProfile.value = !on
        })
    }
  }
}

@Composable
private fun BusinessAddressToggle(addressSettingsState: MutableState<AddressSettingsState>, saveAddressSettings: (AddressSettingsState) -> Unit) {
  PreferenceToggleWithIcon(
    stringResource(MR.strings.business_address),
    painterResource(MR.images.ic_work),
    checked = addressSettingsState.value.businessAddress,
  ) { businessToggle ->
    addressSettingsState.value = if (businessToggle)
      AddressSettingsState(
        businessAddress = true,
        autoAccept = true,
        autoAcceptIncognito = false,
        autoReply = addressSettingsState.value.autoReply
      )
    else
      AddressSettingsState(
        businessAddress = false,
        autoAccept = addressSettingsState.value.autoAccept,
        autoAcceptIncognito = addressSettingsState.value.autoAcceptIncognito,
        autoReply = addressSettingsState.value.autoReply
      )
    saveAddressSettings(addressSettingsState.value)
  }
}

@Composable
private fun AutoAcceptToggle(addressSettingsState: MutableState<AddressSettingsState>, saveAddressSettings: (AddressSettingsState) -> Unit) {
  PreferenceToggleWithIcon(
    stringResource(MR.strings.auto_accept_contact),
    painterResource(MR.images.ic_check),
    disabled = addressSettingsState.value.businessAddress,
    checked = addressSettingsState.value.autoAccept
  ) { autoAcceptToggle ->
    addressSettingsState.value = if (autoAcceptToggle)
      AddressSettingsState(
        businessAddress = addressSettingsState.value.businessAddress,
        autoAccept = true,
        autoAcceptIncognito = false,
        autoReply = ""
      )
    else
      AddressSettingsState(
        businessAddress = false,
        autoAccept = false,
        autoAcceptIncognito = false,
        autoReply = ""
      )
    saveAddressSettings(addressSettingsState.value)
  }
}

@Composable
private fun DeleteAddressButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_delete),
    stringResource(MR.strings.delete_address),
    onClick,
    iconColor = MaterialTheme.colors.error,
    textColor = MaterialTheme.colors.error,
  )
}

private class AddressSettingsState {
  var businessAddress: Boolean = false
    private set
  var autoAccept: Boolean = false
    private set
  var autoAcceptIncognito: Boolean = false
    private set
  var autoReply: String = ""
    private set

  constructor(businessAddress: Boolean = false, autoAccept: Boolean = false, autoAcceptIncognito: Boolean = false, autoReply: String = "") {
    this.businessAddress = businessAddress
    this.autoAccept = autoAccept
    this.autoAcceptIncognito = autoAcceptIncognito
    this.autoReply = autoReply
  }

  constructor(settings: AddressSettings) {
    this.businessAddress = settings.businessAddress
    this.autoAccept = settings.autoAccept != null
    this.autoAcceptIncognito = settings.autoAccept?.acceptIncognito == true
    this.autoReply = settings.autoReply?.text ?: ""
  }

  val addressSettings: AddressSettings
    get() {
      return AddressSettings(
        businessAddress = this.businessAddress,
        autoAccept = if (this.autoAccept) AutoAccept(acceptIncognito = this.autoAcceptIncognito) else null,
        autoReply = if (this.autoReply.isEmpty()) null else MsgContent.MCText(this.autoReply)
      )
    }

  override fun equals(other: Any?): Boolean {
    if (other !is AddressSettingsState) return false
    return (
        this.businessAddress == other.businessAddress
            && this.autoAccept == other.autoAccept
            && this.autoAcceptIncognito == other.autoAcceptIncognito
            && this.autoReply == other.autoReply
    )
  }

  override fun hashCode(): Int {
    var result = businessAddress.hashCode()
    result = 31 * result + autoAccept.hashCode()
    result = 31 * result + autoAcceptIncognito.hashCode()
    result = 31 * result + autoReply.hashCode()
    return result
  }
}

@Composable
private fun AcceptIncognitoToggle(addressSettingsState: MutableState<AddressSettingsState>) {
  PreferenceToggleWithIcon(
    stringResource(MR.strings.accept_contact_incognito_button),
    if (addressSettingsState.value.autoAcceptIncognito) painterResource(MR.images.ic_theater_comedy_filled) else painterResource(MR.images.ic_theater_comedy),
    if (addressSettingsState.value.autoAcceptIncognito) Indigo else MaterialTheme.colors.secondary,
    checked = addressSettingsState.value.autoAcceptIncognito,
  ) { incognitoToggle ->
    addressSettingsState.value = AddressSettingsState(
      businessAddress = addressSettingsState.value.businessAddress,
      autoAccept = addressSettingsState.value.autoAccept,
      autoAcceptIncognito = incognitoToggle,
      autoReply = addressSettingsState.value.autoReply
    )
  }
}

@Composable
private fun AutoReplyEditor(addressSettingsState: MutableState<AddressSettingsState>) {
  val autoReply = rememberSaveable { mutableStateOf(addressSettingsState.value.autoReply) }
  TextEditor(autoReply, Modifier.height(100.dp), placeholder = stringResource(MR.strings.enter_welcome_message_optional))
  LaunchedEffect(autoReply.value) {
    if (autoReply.value != addressSettingsState.value.autoReply) {
      addressSettingsState.value = AddressSettingsState(
        businessAddress = addressSettingsState.value.businessAddress,
        autoAccept = addressSettingsState.value.autoAccept,
        autoAcceptIncognito = addressSettingsState.value.autoAcceptIncognito,
        autoReply = autoReply.value
      )
    }
  }
}

@Composable
private fun saveAddressSettingsButton(disabled: Boolean, onClick: () -> Unit) {
  SectionItemView(onClick, disabled = disabled) {
    Text(stringResource(MR.strings.save_verb), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewUserAddressLayoutNoAddress() {
  SimpleXTheme {
    UserAddressLayout(
      user = User.sampleData,
      userAddress = null,
      createAddress = {},
      showAddShortLinkAlert = {},
      share = { _ -> },
      deleteAddress = {},
      saveAddressSettings = { _, _ -> },
      setProfileAddress = { _ -> },
      learnMore = {},
      shareViaProfile = remember { mutableStateOf(false) },
      sendEmail = {},
    )
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_settings_question),
    confirmText = generalGetString(MR.strings.save_auto_accept_settings),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewUserAddressLayoutAddressCreated() {
  SimpleXTheme {
    UserAddressLayout(
      user = User.sampleData,
      userAddress = UserContactLinkRec(
        CreatedConnLink("https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D", null),
        shortLinkDataSet = false,
        shortLinkLargeDataSet = false,
        addressSettings = AddressSettings(businessAddress = false, autoAccept = null, autoReply = null)
      ),
      createAddress = {},
      showAddShortLinkAlert = {},
      share = { _ -> },
      deleteAddress = {},
      saveAddressSettings = { _, _ -> },
      setProfileAddress = { _ -> },
      learnMore = {},
      shareViaProfile = remember { mutableStateOf(false) },
      sendEmail = {},
    )
  }
}
