package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionDividerSpaced
import SectionTextFooter
import SectionView
import SectionViewWithButton
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.background
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.LocalAliasEditor
import chat.simplex.common.views.chatlist.deleteContactConnectionAlert
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.PendingContactConnection
import chat.simplex.common.platform.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.*
import kotlinx.coroutines.*

@Composable
fun ContactConnectionInfoView(
  chatModel: ChatModel,
  rhId: Long?,
  connLinkInvitation: CreatedConnLink?,
  contactConnection: PendingContactConnection,
  focusAlias: Boolean,
  close: () -> Unit
) {
  LaunchedEffect(connLinkInvitation) {
    if (connLinkInvitation != null) {
      chatModel.showingInvitation.value = ShowingInvitation(contactConnection.id, connLinkInvitation, false, conn = contactConnection)
    }
  }
  /** When [AddContactLearnMore] is open, we don't need to drop [ChatModel.showingInvitation].
   * Otherwise, it will be called here AFTER [AddContactLearnMore] is launched and will clear the value too soon.
   * It will be dropped automatically when connection established or when user goes away from this screen.
   * It applies only to Android because on Desktop center space will not be overlapped by [AddContactLearnMore]
   **/
  DisposableEffect(Unit) {
    onDispose {
      if (!ModalManager.center.hasModalsOpen() || appPlatform.isDesktop) {
        chatModel.showingInvitation.value = null
      }
    }
  }
  val showShortLink = remember { mutableStateOf(true) }
  ContactConnectionInfoLayout(
    chatModel = chatModel,
    connLink = connLinkInvitation,
    showShortLink = showShortLink,
    contactConnection = contactConnection,
    focusAlias = focusAlias,
    rhId = rhId,
    deleteConnection = { deleteContactConnectionAlert(rhId, contactConnection, chatModel, close) },
    onLocalAliasChanged = { setContactAlias(rhId, contactConnection, it, chatModel) },
    learnMore = {
      if (appPlatform.isDesktop) ModalManager.end.closeModals()
      ModalManager.end.showModalCloseable { close ->
        key(this) { AddContactLearnMore(close) }
      }
    }
  )
}

@Composable
private fun ContactConnectionInfoLayout(
  chatModel: ChatModel,
  connLink: CreatedConnLink?,
  showShortLink: MutableState<Boolean>,
  contactConnection: PendingContactConnection,
  focusAlias: Boolean,
  rhId: Long?,
  deleteConnection: () -> Unit,
  onLocalAliasChanged: (String) -> Unit,
  learnMore: () -> Unit,
) {
  @Composable fun incognitoEnabled() {
    if (contactConnection.incognito) {
      SettingsActionItemWithContent(
        icon = painterResource(MR.images.ic_theater_comedy_filled),
        text = null,
        click = {
          if (appPlatform.isDesktop) ModalManager.end.closeModals()
          ModalManager.end.showModal { key(this) { IncognitoView() } }
        },
        iconColor = Indigo,
        extraPadding = false
      ) {
        Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
          Text(stringResource(MR.strings.incognito), Modifier.padding(end = 4.dp))
          Icon(
            painterResource(MR.images.ic_info),
            null,
            tint = MaterialTheme.colors.primary
          )
        }
      }
    }
  }

  ColumnWithScrollBar {
    AppBarTitle(
      stringResource(
        if (contactConnection.initiated) MR.strings.you_invited_a_contact
        else MR.strings.you_accepted_connection
      ),
      hostDevice(rhId)
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

    if (contactConnection.groupLinkId == null) {
      LocalAliasEditor(contactConnection.id, contactConnection.localAlias, center = false, leadingIcon = true, focus = focusAlias, updateValue = onLocalAliasChanged)
    }

    if (connLink != null && connLink.connFullLink.isNotEmpty() && contactConnection.initiated) {
      Spacer(Modifier.height(DEFAULT_PADDING))
      SectionViewWithButton(
        stringResource(MR.strings.one_time_link),
        titleButton = if (connLink.connShortLink == null) null else {{ ToggleShortLinkButton(showShortLink) }}
      ) {
        SimpleXCreatedLinkQRCode(connLink, short = showShortLink.value)
        incognitoEnabled()
        ShareLinkButton(connLink.simplexChatUri(short = showShortLink.value))
        OneTimeLinkLearnMoreButton(learnMore)
      }
    } else {
      SectionView {
        incognitoEnabled()
        OneTimeLinkLearnMoreButton(learnMore)
      }
    }
    SectionTextFooter(sharedProfileInfo(chatModel, contactConnection.incognito))

    SectionDividerSpaced()

    DeleteButton(deleteConnection)

    SectionBottomSpacer()
  }
}

@Composable
fun ShareLinkButton(linkUri: String) {
  val clipboard = LocalClipboardManager.current
  SettingsActionItem(
    painterResource(MR.images.ic_share),
    stringResource(MR.strings.share_invitation_link),
    click = {
      chatModel.showingInvitation.value = chatModel.showingInvitation.value?.copy(connChatUsed = true)
      clipboard.shareText(simplexChatLink(linkUri))
    },
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun OneTimeLinkLearnMoreButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_info),
    stringResource(MR.strings.learn_more),
    onClick,
  )
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

private fun setContactAlias(rhId: Long?, contactConnection: PendingContactConnection, localAlias: String, chatModel: ChatModel) = withBGApi {
  chatModel.controller.apiSetConnectionAlias(rhId, contactConnection.pccConnId, localAlias)?.let {
    withContext(Dispatchers.Main) {
      chatModel.chatsContext.updateContactConnection(rhId, it)
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
internal fun PreviewContactConnectionInfoView() {
  SimpleXTheme {
    ContactConnectionInfoLayout(
      chatModel = ChatModel,
      connLink = CreatedConnLink("https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FFe5ICmvrm4wkrr6X1LTMii-lhBqLeB76%23MCowBQYDK2VuAyEAdhZZsHpuaAk3Hh1q0uNb_6hGTpuwBIrsp2z9U2T0oC0%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAcz6jJk71InuxA0bOX7OUhddfB8Ov7xwQIlIDeXBRZaOntUU4brU5Y3rBzroZBdQJi0FKdtt_D7I%3D%2CMEIwBQYDK2VvAzkA-hDvk1duBi1hlOr08VWSI-Ou4JNNSQjseY69QyKm7Kgg1zZjbpGfyBqSZ2eqys6xtoV4ZtoQUXQ%3D", "https://smp4.simplex.im/i#Wu_NH3_F5YphQrIMSCSfWhvqz7tjpZsu/A2dCDGGVkh0-doRPIZVTM-TflHes2uYMVRgA_tK_uCs"),
      showShortLink = remember { mutableStateOf(true) },
      contactConnection = PendingContactConnection.getSampleData(),
      focusAlias = false,
      rhId = null,
      deleteConnection = {},
      onLocalAliasChanged = {},
      learnMore = {}
    )
  }
}
