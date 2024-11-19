package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
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
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.ShareAddressButton
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR

@Composable
fun UserAddressView(
  chatModel: ChatModel,
  viaCreateLinkView: Boolean = false,
  shareViaProfile: Boolean = false,
  autoCreateAddress: Boolean = false,
  close: () -> Unit
) {
  // TODO close when remote host changes
  val shareViaProfile = remember { mutableStateOf(shareViaProfile) }
  var progressIndicator by remember { mutableStateOf(false) }
  val onCloseHandler: MutableState<(close: () -> Unit) -> Unit> = remember { mutableStateOf({ _ -> }) }
  val user = remember { chatModel.currentUser }
  KeyChangeEffect(user.value?.remoteHostId, user.value?.userId) {
    close()
  }
  fun setProfileAddress(on: Boolean) {
    progressIndicator = true
    withBGApi {
      try {
        val u = chatModel.controller.apiSetProfileAddress(user?.value?.remoteHostId, on)
        if (u != null) {
          chatModel.updateUser(u)
        }
      } catch (e: Exception) {
        Log.e(TAG, "UserAddressView apiSetProfileAddress: ${e.stackTraceToString()}")
      } finally {
        progressIndicator = false
      }
    }
  }

  fun createAddress() {
    withBGApi {
      progressIndicator = true
      val connReqContact = chatModel.controller.apiCreateUserAddress(user?.value?.remoteHostId)
      if (connReqContact != null) {
        chatModel.userAddress.value = UserContactLinkRec(connReqContact)

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
      progressIndicator = false
    }
  }

  LaunchedEffect(autoCreateAddress) {
    if (autoCreateAddress) {
      createAddress()
    }
  }
  val userAddress = remember { chatModel.userAddress }
  val clipboard = LocalClipboardManager.current
  val uriHandler = LocalUriHandler.current
  val showLayout = @Composable {
    UserAddressLayout(
      user = user.value,
      userAddress = userAddress.value,
      shareViaProfile,
      onCloseHandler,
      createAddress = { createAddress() },
      learnMore = {
        ModalManager.start.showModal {
          UserAddressLearnMore()
        }
      },
      share = { userAddress: String -> clipboard.shareText(userAddress) },
      sendEmail = { userAddress ->
        uriHandler.sendEmail(
          generalGetString(MR.strings.email_invite_subject),
          generalGetString(MR.strings.email_invite_body).format(simplexChatLink( userAddress.connReqContact))
        )
      },
      setProfileAddress = ::setProfileAddress,
      deleteAddress = {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.delete_address__question),
          text = if (shareViaProfile.value) generalGetString(MR.strings.all_your_contacts_will_remain_connected_update_sent) else generalGetString(MR.strings.all_your_contacts_will_remain_connected),
          confirmText = generalGetString(MR.strings.delete_verb),
          onConfirm = {
            progressIndicator = true
            withBGApi {
              val u = chatModel.controller.apiDeleteUserAddress(user?.value?.remoteHostId)
              if (u != null) {
                chatModel.userAddress.value = null
                chatModel.updateUser(u)
                shareViaProfile.value = false
                progressIndicator = false
              }
            }
          },
          destructive = true,
        )
      },
      saveAas = { aas: AutoAcceptState, savedAAS: MutableState<AutoAcceptState> ->
        withBGApi {
          val address = chatModel.controller.userAddressAutoAccept(user?.value?.remoteHostId, aas.autoAccept)
          if (address != null) {
            chatModel.userAddress.value = address
            savedAAS.value = aas
          }
        }
      },
    )
  }

  if (viaCreateLinkView) {
    showLayout()
  } else {
    ModalView(close = { onCloseHandler.value(close) }) {
      showLayout()
    }
  }

  if (progressIndicator) {
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

@Composable
private fun UserAddressLayout(
  user: User?,
  userAddress: UserContactLinkRec?,
  shareViaProfile: MutableState<Boolean>,
  onCloseHandler: MutableState<(close: () -> Unit) -> Unit>,
  createAddress: () -> Unit,
  learnMore: () -> Unit,
  share: (String) -> Unit,
  sendEmail: (UserContactLinkRec) -> Unit,
  setProfileAddress: (Boolean) -> Unit,
  deleteAddress: () -> Unit,
  saveAas: (AutoAcceptState, MutableState<AutoAcceptState>) -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.simplex_address), hostDevice(user?.remoteHostId))
    Column(
      Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING_HALF),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      if (userAddress == null) {
        SectionView {
          CreateAddressButton(createAddress)
          SectionTextFooter(stringResource(MR.strings.create_address_and_let_people_connect))
        }
        SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
        SectionView {
          LearnMoreButton(learnMore)
        }
        LaunchedEffect(Unit) {
          onCloseHandler.value = { close -> close() }
        }
      } else {
        val autoAcceptState = remember { mutableStateOf(AutoAcceptState(userAddress)) }
        val autoAcceptStateSaved = remember { mutableStateOf(autoAcceptState.value) }
        SectionView(stringResource(MR.strings.address_section_title).uppercase()) {
          SimpleXLinkQRCode(userAddress.connReqContact)
          ShareAddressButton { share(simplexChatLink(userAddress.connReqContact)) }
          ShareViaEmailButton { sendEmail(userAddress) }
          ShareWithContactsButton(shareViaProfile, setProfileAddress)
          AutoAcceptToggle(autoAcceptState) { saveAas(autoAcceptState.value, autoAcceptStateSaved) }
          LearnMoreButton(learnMore)
        }
        if (autoAcceptState.value.enable) {
          SectionDividerSpaced()
          AutoAcceptSection(autoAcceptState, autoAcceptStateSaved, saveAas)
        }

        SectionDividerSpaced(maxBottomPadding = false)

        SectionView {
          DeleteAddressButton(deleteAddress)
          SectionTextFooter(stringResource(MR.strings.your_contacts_will_remain_connected))
        }
        LaunchedEffect(Unit) {
          onCloseHandler.value = { close ->
            if (autoAcceptState.value == autoAcceptStateSaved.value) close()
            else showUnsavedChangesAlert({ saveAas(autoAcceptState.value, autoAcceptStateSaved); close() }, close)
          }
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
private fun LearnMoreButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_info),
    stringResource(MR.strings.learn_more_about_address),
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
private fun AutoAcceptToggle(autoAcceptState: MutableState<AutoAcceptState>, saveAas: (AutoAcceptState) -> Unit) {
  PreferenceToggleWithIcon(stringResource(MR.strings.auto_accept_contact), painterResource(MR.images.ic_check), checked = autoAcceptState.value.enable) {
    autoAcceptState.value = if (!it)
      AutoAcceptState()
    else
      AutoAcceptState(it, autoAcceptState.value.incognito, autoAcceptState.value.welcomeText)
    saveAas(autoAcceptState.value)
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

private class AutoAcceptState {
  var enable: Boolean = false
    private set
  var incognito: Boolean = false
    private set
  var welcomeText: String = ""
    private set

  constructor(enable: Boolean = false, incognito: Boolean = false, welcomeText: String = "") {
    this.enable = enable
    this.incognito = incognito
    this.welcomeText = welcomeText
  }

  constructor(contactLink: UserContactLinkRec) {
    contactLink.autoAccept?.let { aa ->
      enable = true
      incognito = aa.acceptIncognito
      aa.autoReply?.let { msg ->
        welcomeText = msg.text
      } ?: run {
        welcomeText = ""
      }
    }
  }

  val autoAccept: AutoAccept?
    get() {
      if (enable) {
        var autoReply: MsgContent? = null
        val s = welcomeText.trim()
        if (s != "") {
          autoReply = MsgContent.MCText(s)
        }
        return AutoAccept(incognito, autoReply)
      }
      return null
    }

  override fun equals(other: Any?): Boolean {
    if (other !is AutoAcceptState) return false
    return this.enable == other.enable && this.incognito == other.incognito && this.welcomeText == other.welcomeText
  }

  override fun hashCode(): Int {
    var result = enable.hashCode()
    result = 31 * result + incognito.hashCode()
    result = 31 * result + welcomeText.hashCode()
    return result
  }
}

@Composable
private fun AutoAcceptSection(
  autoAcceptState: MutableState<AutoAcceptState>,
  savedAutoAcceptState: MutableState<AutoAcceptState>,
  saveAas: (AutoAcceptState, MutableState<AutoAcceptState>) -> Unit
) {
  SectionView(stringResource(MR.strings.auto_accept_contact).uppercase()) {
    AcceptIncognitoToggle(autoAcceptState)
    WelcomeMessageEditor(autoAcceptState)
    SaveAASButton(autoAcceptState.value == savedAutoAcceptState.value) { saveAas(autoAcceptState.value, savedAutoAcceptState) }
  }
}

@Composable
private fun AcceptIncognitoToggle(autoAcceptState: MutableState<AutoAcceptState>) {
  PreferenceToggleWithIcon(
    stringResource(MR.strings.accept_contact_incognito_button),
    if (autoAcceptState.value.incognito) painterResource(MR.images.ic_theater_comedy_filled) else painterResource(MR.images.ic_theater_comedy),
    if (autoAcceptState.value.incognito) Indigo else MaterialTheme.colors.secondary,
    autoAcceptState.value.incognito,
  ) {
    autoAcceptState.value = AutoAcceptState(autoAcceptState.value.enable, it, autoAcceptState.value.welcomeText)
  }
}

@Composable
private fun WelcomeMessageEditor(autoAcceptState: MutableState<AutoAcceptState>) {
  val welcomeText = rememberSaveable { mutableStateOf(autoAcceptState.value.welcomeText) }
  TextEditor(welcomeText, Modifier.height(100.dp), placeholder = stringResource(MR.strings.enter_welcome_message_optional))
  LaunchedEffect(welcomeText.value) {
    if (welcomeText.value != autoAcceptState.value.welcomeText) {
      autoAcceptState.value = AutoAcceptState(autoAcceptState.value.enable, autoAcceptState.value.incognito, welcomeText.value)
    }
  }
}

@Composable
private fun SaveAASButton(disabled: Boolean, onClick: () -> Unit) {
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
      share = { _ -> },
      deleteAddress = {},
      saveAas = { _, _ -> },
      setProfileAddress = { _ -> },
      learnMore = {},
      shareViaProfile = remember { mutableStateOf(false) },
      onCloseHandler = remember { mutableStateOf({}) },
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
      userAddress = UserContactLinkRec("https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"),
      createAddress = {},
      share = { _ -> },
      deleteAddress = {},
      saveAas = { _, _ -> },
      setProfileAddress = { _ -> },
      learnMore = {},
      shareViaProfile = remember { mutableStateOf(false) },
      onCloseHandler = remember { mutableStateOf({}) },
      sendEmail = {},
    )
  }
}
