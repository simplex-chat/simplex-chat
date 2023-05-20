package chat.simplex.app.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import android.content.res.Configuration
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.ShareAddressButton
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCode

@Composable
fun UserAddressView(
  chatModel: ChatModel,
  viaCreateLinkView: Boolean = false,
  shareViaProfile: Boolean = false,
  close: () -> Unit
) {
  val context = LocalContext.current
  val shareViaProfile = remember { mutableStateOf(shareViaProfile) }
  var progressIndicator by remember { mutableStateOf(false) }
  val onCloseHandler: MutableState<(close: () -> Unit) -> Unit> = remember { mutableStateOf({ _ -> }) }

  fun setProfileAddress(on: Boolean) {
    progressIndicator = true
    withBGApi {
      try {
        val u = chatModel.controller.apiSetProfileAddress(on)
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
  val userAddress = remember { chatModel.userAddress }
  val showLayout = @Composable {
    UserAddressLayout(
      userAddress = userAddress.value,
      shareViaProfile,
      onCloseHandler,
      createAddress = {
        withApi {
          progressIndicator = true
          val connReqContact = chatModel.controller.apiCreateUserAddress()
          if (connReqContact != null) {
            chatModel.userAddress.value = UserContactLinkRec(connReqContact)

//            TODO uncomment in v5.2
//            AlertManager.shared.showAlertDialog(
//              title = generalGetString(R.string.share_address_with_contacts_question),
//              text = generalGetString(R.string.add_address_to_your_profile),
//              confirmText = generalGetString(R.string.share_verb),
//              onConfirm = {
//                setProfileAddress(true)
//                shareViaProfile.value = true
//              }
//            )
          }
          progressIndicator = false
        }
      },
      learnMore = {
        ModalManager.shared.showModal {
          Column(
            Modifier
              .fillMaxHeight()
              .padding(horizontal = DEFAULT_PADDING),
            verticalArrangement = Arrangement.SpaceBetween
          ) {
            UserAddressLearnMore()
          }
        }
      },
      share = { userAddress: String -> shareText(context, userAddress) },
      sendEmail = { userAddress ->
        sendEmail(
        context,
          generalGetString(R.string.email_invite_subject),
          generalGetString(R.string.email_invite_body).format(userAddress.connReqContact)
        )
      },
      setProfileAddress = ::setProfileAddress,
      deleteAddress = {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(R.string.delete_address__question),
          text = if (shareViaProfile.value) generalGetString(R.string.all_your_contacts_will_remain_connected_update_sent) else generalGetString(R.string.all_your_contacts_will_remain_connected),
          confirmText = generalGetString(R.string.delete_verb),
          onConfirm = {
            progressIndicator = true
            withApi {
              val u = chatModel.controller.apiDeleteUserAddress()
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
          val address = chatModel.controller.userAddressAutoAccept(aas.autoAccept)
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
        Surface(Modifier.size(50.dp), color = MaterialTheme.colors.background.copy(0.9f), shape = RoundedCornerShape(50)){}
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
  Column(
    Modifier.verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.simplex_address), false)
    Column(
      Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING_HALF),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      if (userAddress == null) {
        SectionView {
          CreateAddressButton(createAddress)
          SectionTextFooter(stringResource(R.string.create_address_and_let_people_connect))
        }
        SectionDividerSpaced(maxBottomPadding = false)
        SectionView {
          LearnMoreButton(learnMore)
        }
        LaunchedEffect(Unit) {
          onCloseHandler.value = { close -> close() }
        }
      } else {
        val autoAcceptState = remember { mutableStateOf(AutoAcceptState(userAddress)) }
        val autoAcceptStateSaved = remember { mutableStateOf(autoAcceptState.value) }
        SectionView(stringResource(R.string.address_section_title).uppercase()) {
          QRCode(userAddress.connReqContact, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF).aspectRatio(1f))
          ShareAddressButton { share(userAddress.connReqContact) }
          ShareViaEmailButton { sendEmail(userAddress) }
//          TODO uncomment in v5.2
//          ShareWithContactsButton(shareViaProfile, setProfileAddress)
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
          SectionTextFooter(stringResource(R.string.your_contacts_will_remain_connected))
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
    painterResource(R.drawable.ic_qr_code),
    stringResource(R.string.create_simplex_address),
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
private fun LearnMoreButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(R.drawable.ic_info),
    stringResource(R.string.learn_more_about_address),
    onClick,
  )
}

@Composable
fun ShareViaEmailButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(R.drawable.ic_mail),
    stringResource(R.string.invite_friends),
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun ShareWithContactsButton(shareViaProfile: MutableState<Boolean>, setProfileAddress: (Boolean) -> Unit) {
  PreferenceToggleWithIcon(
    stringResource(R.string.share_with_contacts),
    painterResource(R.drawable.ic_person),
    checked = shareViaProfile.value,
  ) { on ->
    shareViaProfile.value = on
    if (on) {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(R.string.share_address_with_contacts_question),
        text = generalGetString(R.string.profile_update_will_be_sent_to_contacts),
        confirmText = generalGetString(R.string.share_verb),
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
        title = generalGetString(R.string.stop_sharing_address),
        text = generalGetString(R.string.profile_update_will_be_sent_to_contacts),
        confirmText = generalGetString(R.string.stop_sharing),
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
  PreferenceToggleWithIcon(stringResource(R.string.auto_accept_contact), painterResource(R.drawable.ic_check), checked = autoAcceptState.value.enable) {
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
    painterResource(R.drawable.ic_delete),
    stringResource(R.string.delete_address),
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
  SectionView(stringResource(R.string.auto_accept_contact).uppercase()) {
    AcceptIncognitoToggle(autoAcceptState)
    WelcomeMessageEditor(autoAcceptState)
    SaveAASButton(autoAcceptState.value == savedAutoAcceptState.value) { saveAas(autoAcceptState.value, savedAutoAcceptState) }
  }
}

@Composable
private fun AcceptIncognitoToggle(autoAcceptState: MutableState<AutoAcceptState>) {
  PreferenceToggleWithIcon(
    stringResource(R.string.accept_contact_incognito_button),
    if (autoAcceptState.value.incognito) painterResource(R.drawable.ic_theater_comedy_filled) else painterResource(R.drawable.ic_theater_comedy),
    if (autoAcceptState.value.incognito) Indigo else MaterialTheme.colors.secondary,
    autoAcceptState.value.incognito,
  ) {
    autoAcceptState.value = AutoAcceptState(autoAcceptState.value.enable, it, autoAcceptState.value.welcomeText)
  }
}

@Composable
private fun WelcomeMessageEditor(autoAcceptState: MutableState<AutoAcceptState>) {
  val welcomeText = rememberSaveable { mutableStateOf(autoAcceptState.value.welcomeText) }
  TextEditor(welcomeText, Modifier.height(100.dp), placeholder = stringResource(R.string.enter_welcome_message_optional))
  LaunchedEffect(welcomeText.value) {
    if (welcomeText.value != autoAcceptState.value.welcomeText) {
      autoAcceptState.value = AutoAcceptState(autoAcceptState.value.enable, autoAcceptState.value.incognito, welcomeText.value)
    }
  }
}

@Composable
private fun SaveAASButton(disabled: Boolean, onClick: () -> Unit) {
  SectionItemView(onClick, disabled = disabled) {
    Text(stringResource(R.string.save_verb), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewUserAddressLayoutNoAddress() {
  SimpleXTheme {
    UserAddressLayout(
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
    title = generalGetString(R.string.save_settings_question),
    confirmText = generalGetString(R.string.save_auto_accept_settings),
    dismissText = generalGetString(R.string.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewUserAddressLayoutAddressCreated() {
  SimpleXTheme {
    UserAddressLayout(
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
