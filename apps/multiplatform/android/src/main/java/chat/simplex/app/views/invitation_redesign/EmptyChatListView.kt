package chat.simplex.app.views.invitation_redesign

import SectionBottomSpacer
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR

@Composable
fun BoxScope.EmptyChatListView() {
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
      InviteSomeoneContent()
      SectionBottomSpacer()
    }
  } else {
    val closeAll = { ModalManager.start.closeModals() }
    Column(
      Modifier
        .align(Alignment.Center)
        .fillMaxWidth()
        .padding(horizontal = DEFAULT_PADDING),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Text(
        stringResource(MR.strings.now_you_can),
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Bold),
      )
      Spacer(Modifier.height(DEFAULT_PADDING))
      Surface(
        shape = RoundedCornerShape(18.dp),
        color = MaterialTheme.appColors.sentMessage,
        modifier = Modifier.fillMaxWidth().clickable {
          showInviteSomeone = true
        }
      ) {
        Column(
          Modifier.fillMaxWidth().padding(DEFAULT_PADDING),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          Image(
            painterResource(MR.images.ic_invitation_card_invite_someone),
            contentDescription = null,
            modifier = Modifier.fillMaxWidth()
          )
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))
          Row(verticalAlignment = Alignment.CenterVertically) {
            Icon(
              painterResource(MR.images.ic_repeat_one),
              contentDescription = null,
              modifier = Modifier.size(18.dp),
              tint = MaterialTheme.colors.secondary
            )
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Text(
              stringResource(MR.strings.invite_someone_to_chat),
              style = MaterialTheme.typography.body1.copy(fontWeight = FontWeight.Medium),
            )
          }
        }
      }
      Spacer(Modifier.height(DEFAULT_PADDING))
      Surface(
        shape = RoundedCornerShape(18.dp),
        color = MaterialTheme.appColors.sentMessage,
        modifier = Modifier.fillMaxWidth().clickable {
          ModalManager.start.showModalCloseable(endButtons = { AddContactLearnMoreButton() }) { _ ->
            NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = appPlatform.isAndroid, close = closeAll)
          }
        }
      ) {
        Column(
          Modifier.fillMaxWidth().padding(DEFAULT_PADDING),
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          Image(
            painterResource(MR.images.ic_invitation_card_one_time_link),
            contentDescription = null,
            modifier = Modifier.fillMaxWidth()
          )
          Spacer(Modifier.height(DEFAULT_PADDING_HALF))
          Row(verticalAlignment = Alignment.CenterVertically) {
            Icon(
              painterResource(MR.images.ic_qr_code),
              contentDescription = null,
              modifier = Modifier.size(18.dp),
              tint = MaterialTheme.colors.secondary
            )
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Text(
              stringResource(MR.strings.connect_via_link_or_qr),
              style = MaterialTheme.typography.body1.copy(fontWeight = FontWeight.Medium),
            )
          }
        }
      }
    }
  }
}

@Preview
@Composable
fun PreviewEmptyChatListView() {
  SimpleXTheme {
    Box(Modifier.fillMaxSize()) {
      EmptyChatListView()
    }
  }
}
