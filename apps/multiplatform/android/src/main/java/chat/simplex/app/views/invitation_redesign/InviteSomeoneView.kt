package chat.simplex.app.views.invitation_redesign

import SectionBottomSpacer
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.invitation_redesign.OneTimeLinkView
import chat.simplex.common.views.usersettings.UserAddressView
import chat.simplex.res.MR

@Composable
fun InviteSomeoneView(close: () -> Unit) {
  ModalView(close) {
    ColumnWithScrollBar(
      Modifier.fillMaxSize().background(Color.White),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      AppBarTitle(stringResource(MR.strings.invite_someone), withPadding = false)
      Spacer(Modifier.height(DEFAULT_PADDING))
      InviteSomeoneContent()
      SectionBottomSpacer()
    }
  }
}

@Composable
fun InviteSomeoneContent() {
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = MaterialTheme.appColors.sentMessage,
    modifier = Modifier
      .fillMaxSize().background(Color.White)
      .padding(horizontal = DEFAULT_PADDING)
      .clickable {
        ModalManager.start.showModalCloseable { close ->
          OneTimeLinkView(rhId = chatModel.currentRemoteHost.value?.remoteHostId, close = close)
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
    }
  }

  Spacer(Modifier.height(DEFAULT_PADDING_HALF))

  Row(
    Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      painterResource(MR.images.ic_repeat_one),
      contentDescription = null,
      modifier = Modifier.size(20.dp),
      tint = MaterialTheme.colors.secondary
    )
    Spacer(Modifier.width(DEFAULT_PADDING_HALF))
    Column {
      Text(
        stringResource(MR.strings.create_private_1_time_link),
        style = MaterialTheme.typography.h3.copy(fontWeight = FontWeight.Bold),
      )
      Text(
        stringResource(MR.strings.contact_can_use_link_or_scan_qr),
        style = MaterialTheme.typography.body2,
        color = MaterialTheme.colors.secondary
      )
    }
  }

  Spacer(Modifier.height(DEFAULT_PADDING * 1.5f))

  Surface(
    shape = RoundedCornerShape(18.dp),
    color = MaterialTheme.appColors.sentMessage,
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING)
      .clickable {
        ModalManager.start.showModalCloseable { closeAddress ->
          UserAddressView(chatModel = chatModel, shareViaProfile = false, autoCreateAddress = true, close = closeAddress)
        }
      }
  ) {
    Column(
      Modifier.fillMaxWidth().padding(DEFAULT_PADDING),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Image(
        painterResource(MR.images.ic_invitation_card_public_address),
        contentDescription = null,
        modifier = Modifier.fillMaxWidth()
      )
    }
  }

  Spacer(Modifier.height(DEFAULT_PADDING_HALF))

  Row(
    Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      painterResource(MR.images.ic_qr_code),
      contentDescription = null,
      modifier = Modifier.size(20.dp),
      tint = MaterialTheme.colors.secondary
    )
    Spacer(Modifier.width(DEFAULT_PADDING_HALF))
    Column {
      Text(
        stringResource(MR.strings.create_public_simplex_address),
        style = MaterialTheme.typography.h3.copy(fontWeight = FontWeight.Bold),
      )
      Text(
        stringResource(MR.strings.public_link_for_social_media_email_or_website),
        style = MaterialTheme.typography.body2,
        color = MaterialTheme.colors.secondary
      )
    }
  }
}

@Preview
@Composable
fun PreviewInviteSomeoneView() {
  SimpleXTheme {
    ColumnWithScrollBar(
      Modifier.fillMaxSize().background(Color.White),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      AppBarTitle(stringResource(MR.strings.invite_someone), withPadding = false)
      Spacer(Modifier.height(DEFAULT_PADDING))
      InviteSomeoneContent()
      SectionBottomSpacer()
    }
  }
}
