package chat.simplex.app.views.newchat

import SectionDivider
import SectionView
import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.LocalAliasEditor
import chat.simplex.app.views.chatlist.deleteContactConnectionAlert
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.SettingsActionItem

@Composable
fun ContactConnectionInfoView(
  chatModel: ChatModel,
  connReqInvitation: String?,
  contactConnection: PendingContactConnection,
  focusAlias: Boolean,
  close: () -> Unit
) {
  LaunchedEffect(connReqInvitation) {
    chatModel.connReqInv.value = connReqInvitation
  }
  /** When [AddContactView] is open, we don't need to drop [chatModel.connReqInv].
   * Otherwise, it will be called here AFTER [AddContactView] is launched and will clear the value too soon.
   * It will be dropped automatically when connection established or when user goes away from this screen.
   **/
  DisposableEffect(Unit) {
    onDispose {
      if (!ModalManager.shared.hasModalsOpen()) {
        chatModel.connReqInv.value = null
      }
    }
  }
  ContactConnectionInfoLayout(
    connReq = connReqInvitation,
    contactConnection,
    focusAlias,
    deleteConnection = { deleteContactConnectionAlert(contactConnection, chatModel, close) },
    onLocalAliasChanged = { setContactAlias(contactConnection, it, chatModel) },
    showQr = {
      ModalManager.shared.showModal {
        Column(
          Modifier
            .fillMaxHeight()
            .padding(horizontal = DEFAULT_PADDING),
          verticalArrangement = Arrangement.SpaceBetween
        ) {
          AddContactView(connReqInvitation ?: return@showModal, contactConnection.incognito)
        }
      }
    }
  )
}

@Composable
private fun ContactConnectionInfoLayout(
  connReq: String?,
  contactConnection: PendingContactConnection,
  focusAlias: Boolean,
  deleteConnection: () -> Unit,
  onLocalAliasChanged: (String) -> Unit,
  showQr: () -> Unit,
) {
  Column(
    Modifier
      .verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(
      stringResource(
        if (contactConnection.initiated) R.string.you_invited_your_contact
        else R.string.you_accepted_connection
      )
    )
    if (contactConnection.groupLinkId == null) {
      Row(Modifier.padding(bottom = DEFAULT_PADDING)) {
        LocalAliasEditor(contactConnection.localAlias, center = false, leadingIcon = true, focus = focusAlias, updateValue = onLocalAliasChanged)
      }
    }
    Text(
      stringResource(
        if (contactConnection.viaContactUri)
          if (contactConnection.groupLinkId != null) R.string.you_will_be_connected_when_group_host_device_is_online
          else R.string.you_will_be_connected_when_your_connection_request_is_accepted
        else R.string.you_will_be_connected_when_your_contacts_device_is_online
      ),
      Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING)
    )
    SectionView {
      if (!connReq.isNullOrEmpty() && contactConnection.initiated) {
        ShowQrButton(contactConnection.incognito, showQr)
        SectionDivider()
      }
      DeleteButton(deleteConnection)
    }
  }
}

@Composable
fun ShowQrButton(incognito: Boolean, onClick: () -> Unit) {
  SettingsActionItem(
    Icons.Outlined.QrCode,
    stringResource(R.string.show_QR_code),
    click = onClick,
    textColor = if (incognito) Indigo else MaterialTheme.colors.primary,
    iconColor = if (incognito) Indigo else MaterialTheme.colors.primary,
  )
}

@Composable
fun DeleteButton(onClick: () -> Unit) {
  SettingsActionItem(
    Icons.Outlined.Delete,
    stringResource(R.string.delete_verb),
    click = onClick,
    textColor = Color.Red,
    iconColor = Color.Red,
  )
}

private fun setContactAlias(contactConnection: PendingContactConnection, localAlias: String, chatModel: ChatModel) = withApi {
  chatModel.controller.apiSetConnectionAlias(contactConnection.pccConnId, localAlias)?.let {
    chatModel.updateContactConnection(it)
  }
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
private fun PreviewContactConnectionInfoView() {
  SimpleXTheme {
    ContactConnectionInfoLayout(
      connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D",
      PendingContactConnection.getSampleData(),
      focusAlias = false,
      deleteConnection = {},
      onLocalAliasChanged = {},
      showQr = {},
    )
  }
}
