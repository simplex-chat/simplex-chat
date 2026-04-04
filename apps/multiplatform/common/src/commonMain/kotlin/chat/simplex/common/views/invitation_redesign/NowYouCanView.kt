package chat.simplex.common.views.invitation_redesign

import SectionBottomSpacer
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun EmptyChatListView(onConnectClick: () -> Unit, onOneTimeLinkClick: () -> Unit) {
  var showInviteSomeone by remember { mutableStateOf(false) }

  if (showInviteSomeone) {
    BackHandler { showInviteSomeone = false }
    Column(
      Modifier
        .fillMaxSize()
        .background(MaterialTheme.colors.background)
        .verticalScroll(rememberScrollState()),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Row(Modifier.fillMaxWidth()) {
        NavigationButtonBack(onButtonClicked = { showInviteSomeone = false })
      }
      Spacer(Modifier.height(DEFAULT_PADDING))
      Text(
        stringResource(MR.strings.invite_someone),
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Bold),
      )
      Spacer(Modifier.height(DEFAULT_PADDING))
      if (fullInvitationArtAvailable) {
        InviteSomeoneWithPicturesContent(onOneTimeLinkClick = onOneTimeLinkClick)
      } else {
        InviteSomeoneContent()
      }
      SectionBottomSpacer()
    }
  } else {
    Column(
      Modifier
        .fillMaxWidth()
        .padding(horizontal = DEFAULT_PADDING)
        .padding(top = AppBarHeight)
        .verticalScroll(rememberScrollState()),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Text(
        stringResource(MR.strings.now_you_can),
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Bold),
      )
      Spacer(Modifier.height(DEFAULT_PADDING))

      InvitationCardView(
        mainImageResource = MR.images.ic_invitation_card_invite_someone,
        iconResource = MR.images.ic_repeat_one,
        title = stringResource(MR.strings.invite_someone_to_chat),
        modifier = Modifier.fillMaxWidth().clickable { showInviteSomeone = true }
      )
      Spacer(Modifier.height(DEFAULT_PADDING))
      InvitationCardView(
        mainImageResource = MR.images.ic_invitation_connected_link_qr,
        iconResource = MR.images.ic_qr_code,
        title = stringResource(MR.strings.connect_via_link_or_qr),
        modifier = Modifier.fillMaxWidth().clickable { onConnectClick() }
      )

      Spacer(Modifier.height(DEFAULT_BIG_PADDING * 3))
    }

  }
}

@Preview
@Composable
fun PreviewEmptyChatListView() {
  SimpleXTheme {
    Box(Modifier.fillMaxSize()) {
      EmptyChatListView(onConnectClick = {}, onOneTimeLinkClick = {})
    }
  }
}
