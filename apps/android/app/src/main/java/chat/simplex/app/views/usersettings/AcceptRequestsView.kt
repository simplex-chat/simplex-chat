package chat.simplex.app.views.usersettings

import SectionCustomFooter
import SectionDivider
import SectionItemView
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.TheaterComedy
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun AcceptRequestsView(m: ChatModel, contactLink: UserContactLinkRec) {
  var contactLink by remember { mutableStateOf(contactLink) }
  AcceptRequestsLayout(
    contactLink,
    saveState = { new: MutableState<AutoAcceptState>, old: MutableState<AutoAcceptState> ->
      withApi {
        val link = m.controller.userAddressAutoAccept(new.value.autoAccept)
        if (link != null) {
          contactLink = link
          m.userAddress.value = link
          old.value = new.value
        }
      }
    }
  )
}

@Composable
private fun AcceptRequestsLayout(
  contactLink: UserContactLinkRec,
  saveState: (new: MutableState<AutoAcceptState>, old: MutableState<AutoAcceptState>) -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.contact_requests))
    val autoAcceptState = remember { mutableStateOf(AutoAcceptState(contactLink)) }
    val autoAcceptStateSaved = remember { mutableStateOf(autoAcceptState.value) }
    SectionView(stringResource(R.string.accept_requests).uppercase()) {
      SectionItemView {
        PreferenceToggleWithIcon(stringResource(R.string.accept_automatically), Icons.Outlined.Check, checked = autoAcceptState.value.enable) {
          autoAcceptState.value = if (!it)
            AutoAcceptState()
          else
            AutoAcceptState(it, autoAcceptState.value.incognito, autoAcceptState.value.welcomeText)
        }
      }
      if (autoAcceptState.value.enable) {
        SectionDivider()
        SectionItemView {
          PreferenceToggleWithIcon(
            stringResource(R.string.incognito),
            if (autoAcceptState.value.incognito) Icons.Filled.TheaterComedy else Icons.Outlined.TheaterComedy,
            if (autoAcceptState.value.incognito) Indigo else HighOrLowlight,
            autoAcceptState.value.incognito,
          ) {
            autoAcceptState.value = AutoAcceptState(autoAcceptState.value.enable, it, autoAcceptState.value.welcomeText)
          }
        }
      }
    }
    val welcomeText = remember { mutableStateOf(autoAcceptState.value.welcomeText) }
    SectionCustomFooter(PaddingValues(horizontal = DEFAULT_PADDING)) {
      ButtonsFooter(
        cancel = {
          autoAcceptState.value = autoAcceptStateSaved.value
          welcomeText.value = autoAcceptStateSaved.value.welcomeText
        },
        save = { saveState(autoAcceptState, autoAcceptStateSaved) },
        disabled = autoAcceptState.value == autoAcceptStateSaved.value
      )
    }
    Spacer(Modifier.height(DEFAULT_PADDING))
    if (autoAcceptState.value.enable) {
      Text(
        stringResource(R.string.section_title_welcome_message), color = HighOrLowlight, style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), fontSize = 12.sp
      )
      TextEditor(Modifier.padding(horizontal = DEFAULT_PADDING).height(160.dp), text = welcomeText)
      LaunchedEffect(welcomeText.value) {
        if (welcomeText.value != autoAcceptState.value.welcomeText) {
          autoAcceptState.value = AutoAcceptState(autoAcceptState.value.enable, autoAcceptState.value.incognito, welcomeText.value)
        }
      }
    }
  }
}

@Composable
private fun ButtonsFooter(cancel: () -> Unit, save: () -> Unit, disabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    FooterButton(Icons.Outlined.Replay, stringResource(R.string.cancel_verb), cancel, disabled)
    FooterButton(Icons.Outlined.Check, stringResource(R.string.save_verb), save, disabled)
  }
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
