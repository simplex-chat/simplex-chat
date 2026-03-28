package chat.simplex.common.views.invitation_redesign

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
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.UserAddressView
import chat.simplex.res.MR

@Composable
fun InviteSomeoneWithPicturesView(close: () -> Unit) {
  ModalView(close) {
    ColumnWithScrollBar(
      Modifier.fillMaxSize().padding(bottom = DEFAULT_BOTTOM_PADDING).background(Color.White),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      AppBarTitle(stringResource(MR.strings.invite_someone), withPadding = false)
      Spacer(Modifier.height(DEFAULT_PADDING))
      InviteSomeoneWithPicturesContent()
      SectionBottomSpacer()
    }
  }
}

@Composable
fun InviteSomeoneWithPicturesContent() {

  InvitationCardView(
    mainImageResource = MR.images.ic_invitation_card_one_time_link,
    iconResource = MR.images.ic_repeat_one,
    title = stringResource(MR.strings.create_private_1_time_link),
    description = stringResource(MR.strings.contact_can_use_link_or_scan_qr),
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING)
      .clickable {
      ModalManager.start.showModalCloseable { close ->
        OneTimeLinkView(rhId = chatModel.currentRemoteHost.value?.remoteHostId, close = close)
      }
    }
  )

  Spacer(Modifier.height(DEFAULT_PADDING))

  InvitationCardView(
    mainImageResource = MR.images.ic_invitation_card_public_address,
    iconResource = MR.images.ic_qr_code,
    title = stringResource(MR.strings.create_public_simplex_address),
    description = stringResource(MR.strings.public_link_for_social_media_email_or_website),
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING)
      .clickable {
        // TODO, Hayk replace with page when data will be available
        ModalManager.start.showModalCloseable { close ->
          OneTimeLinkView(rhId = chatModel.currentRemoteHost.value?.remoteHostId, close = close)
        }
      }
  )
}

@Preview
@Composable
private fun PreviewInviteSomeoneView() {
  SimpleXTheme {
    ColumnWithScrollBar(
      Modifier.fillMaxSize().background(MaterialTheme.colors.background),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      AppBarTitle(stringResource(MR.strings.invite_someone), withPadding = false)
      Spacer(Modifier.height(DEFAULT_PADDING))
      InviteSomeoneWithPicturesContent()
      SectionBottomSpacer()
    }
  }
}
