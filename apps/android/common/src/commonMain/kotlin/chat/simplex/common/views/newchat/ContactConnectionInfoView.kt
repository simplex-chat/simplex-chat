package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.LocalAliasEditor
import chat.simplex.common.views.chatlist.deleteContactConnectionAlert
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.PendingContactConnection
import chat.simplex.common.platform.shareText

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
  val clipboard = LocalClipboardManager.current
  ContactConnectionInfoLayout(
    connReq = connReqInvitation,
    contactConnection,
    connIncognito = contactConnection.incognito,
    focusAlias,
    deleteConnection = { deleteContactConnectionAlert(contactConnection, chatModel, close) },
    onLocalAliasChanged = { setContactAlias(contactConnection, it, chatModel) },
    share = { if (connReqInvitation != null) clipboard.shareText(connReqInvitation) },
    learnMore = {
      ModalManager.shared.showModal {
        Column(
          Modifier
            .fillMaxHeight()
            .padding(horizontal = DEFAULT_PADDING),
          verticalArrangement = Arrangement.SpaceBetween
        ) {
          AddContactLearnMore()
        }
      }
    }
  )
}

@Composable
private fun ContactConnectionInfoLayout(
  connReq: String?,
  contactConnection: PendingContactConnection,
  connIncognito: Boolean,
  focusAlias: Boolean,
  deleteConnection: () -> Unit,
  onLocalAliasChanged: (String) -> Unit,
  share: () -> Unit,
  learnMore: () -> Unit,
) {
  Column(
    Modifier
      .verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(
      stringResource(
        if (contactConnection.initiated) MR.strings.you_invited_your_contact
        else MR.strings.you_accepted_connection
      )
    )
    Text(
      stringResource(
        if (contactConnection.viaContactUri)
          if (contactConnection.groupLinkId != null) MR.strings.you_will_be_connected_when_group_host_device_is_online
          else MR.strings.you_will_be_connected_when_your_connection_request_is_accepted
        else MR.strings.you_will_be_connected_when_your_contacts_device_is_online
      ),
      Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = DEFAULT_PADDING)
    )
    OneTimeLinkProfileText(connIncognito)

    if (contactConnection.groupLinkId == null) {
      LocalAliasEditor(contactConnection.localAlias, center = false, leadingIcon = true, focus = focusAlias, updateValue = onLocalAliasChanged)
    }

    SectionView {
      if (!connReq.isNullOrEmpty() && contactConnection.initiated) {
        OneTimeLinkSection(connReq, share, learnMore)
      } else {
        OneTimeLinkLearnMoreButton(learnMore)
      }
    }

    SectionDividerSpaced(maxBottomPadding = false)

    DeleteButton(deleteConnection)

    SectionBottomSpacer()
  }
}

@Composable
fun DeleteButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_delete),
    stringResource(MR.strings.delete_verb),
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

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
private fun PreviewContactConnectionInfoView() {
  SimpleXTheme {
    ContactConnectionInfoLayout(
      connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D",
      PendingContactConnection.getSampleData(),
      connIncognito = false,
      focusAlias = false,
      deleteConnection = {},
      onLocalAliasChanged = {},
      share = {},
      learnMore = {}
    )
  }
}
