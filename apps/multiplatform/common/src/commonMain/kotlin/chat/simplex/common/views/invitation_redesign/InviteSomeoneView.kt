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
import androidx.compose.ui.unit.sp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.UserAddressView
import chat.simplex.res.MR

@Composable
fun InviteSomeoneView(close: () -> Unit) {
  ModalView(close) {
    ColumnWithScrollBar(
      Modifier.fillMaxSize().background(MaterialTheme.colors.background),
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
  InviteCardComponent(
    image = painterResource(MR.images.ic_invitation_card_one_time_link),
    titleIcon = painterResource(MR.images.ic_add_link),
    title = stringResource(MR.strings.create_private_1_time_link),
    description = stringResource(MR.strings.contact_can_use_link_or_scan_qr),
    bulletPoints = listOf(
      stringResource(MR.strings.one_time_link_only_one_person),
      stringResource(MR.strings.one_time_link_once_connected_removed),
      stringResource(MR.strings.one_time_link_secure_to_pass)
    ),
    buttonText = stringResource(MR.strings.create_1_time_link),
    onDismiss = {},
    onClick = {
      ModalManager.start.showModalCloseable { close ->
        OneTimeLinkView(rhId = chatModel.currentRemoteHost.value?.remoteHostId, close = close)
      }
    }
  )

  Spacer(Modifier.height(DEFAULT_PADDING))

  InviteCardComponent(
    image = painterResource(MR.images.ic_invitation_card_public_address),
    titleIcon = painterResource(MR.images.ic_qr_code),
    title = stringResource(MR.strings.create_public_simplex_address),
    description = stringResource(MR.strings.public_link_for_social_media_email_or_website),
    bulletPoints = listOf(
      stringResource(MR.strings.address_many_people_can_connect),
      stringResource(MR.strings.address_removed_without_losing_contacts),
      stringResource(MR.strings.address_servers_cant_see_profile)
    ),
    buttonText = stringResource(MR.strings.create_simplex_address),
    onDismiss = {},
    onClick = {
      ModalManager.start.showModalCloseable { close ->
        OneTimeLinkView(rhId = chatModel.currentRemoteHost.value?.remoteHostId, close = close)
      }
    }
  )
}

@Preview
@Composable
fun PreviewInviteSomeoneView() {
  SimpleXTheme {
    ColumnWithScrollBar(
      Modifier.fillMaxSize().background(MaterialTheme.colors.background),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      AppBarTitle(stringResource(MR.strings.invite_someone), withPadding = false)
      Spacer(Modifier.height(DEFAULT_PADDING))
      InviteSomeoneContent()
      SectionBottomSpacer()
    }
  }
}
