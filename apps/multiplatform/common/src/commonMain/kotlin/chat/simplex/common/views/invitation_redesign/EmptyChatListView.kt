package chat.simplex.common.views.invitation_redesign

import SectionBottomSpacer
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR

@Composable
fun BoxScope.NowYouCanView() {
  var showInviteSomeone by remember { mutableStateOf(false) }

  if (showInviteSomeone) {
    BackHandler { showInviteSomeone = false }
    Column(
      Modifier
        .fillMaxSize()
        .verticalScroll(rememberScrollState()),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Row(Modifier.fillMaxWidth()) {
        NavigationButtonBack(onButtonClicked = { showInviteSomeone = false })
      }
      Text(
        stringResource(MR.strings.invite_someone),
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Bold),
      )
      Spacer(Modifier.height(DEFAULT_PADDING))
      SectionBottomSpacer()
    }
  } else {
    val closeAll = { ModalManager.start.closeModals() }
    Column(
      Modifier
        .align(Alignment.Center)
        .fillMaxWidth(),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Text(
        stringResource(MR.strings.now_you_can),
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Bold),
        modifier = Modifier.padding(horizontal = DEFAULT_PADDING)
      )
      Spacer(Modifier.height(DEFAULT_PADDING))
      InviteCardComponent(
        image = painterResource(MR.images.ic_invitation_card_invite_someone),
        titleIcon = painterResource(MR.images.ic_repeat_one),
        title = stringResource(MR.strings.invite_someone_to_chat),
        onClick = {
          ModalManager.start.showModalCloseable(endButtons = { AddContactLearnMoreButton() }) { _ ->
            InviteSomeoneView {}
          }
        }
      )
      Spacer(Modifier.height(DEFAULT_PADDING))
      InviteCardComponent(
        image = painterResource(MR.images.ic_invitation_card_one_time_link),
        titleIcon = painterResource(MR.images.ic_qr_code),
        title = stringResource(MR.strings.connect_via_link_or_qr),
        onClick = {
          ModalManager.start.showModalCloseable(endButtons = { AddContactLearnMoreButton() }) { _ ->
            ConnectViewLinkOrQrModal(chatModel.currentRemoteHost.value?.remoteHostId, {})
          }
        }
      )
    }
  }
}

@Preview
@Composable
fun PreviewEmptyChatListView() {
  SimpleXTheme {
    Box(Modifier.fillMaxSize()) {
      NowYouCanView()
    }
  }
}
