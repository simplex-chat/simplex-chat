package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun UserAddressLearnMore(showCreateAddressButton: Boolean = false) {
  ColumnWithScrollBar(Modifier .padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.address_or_1_time_link), withPadding = false)

    Row {
      Icon(painterResource(MR.images.ic_mail), null, tint = MaterialTheme.colors.secondary)
      Spacer(Modifier.width(DEFAULT_PADDING_HALF))
      ReadableText(MR.strings.share_address_publicly, style = MaterialTheme.typography.h3.copy(fontWeight = FontWeight.Bold))
    }
    ReadableText(MR.strings.share_simplex_address_on_social_media)
    ReadableText(MR.strings.you_wont_lose_your_contacts_if_delete_address)

    Row(Modifier.padding(top = DEFAULT_PADDING_HALF)) {
      Icon(painterResource(MR.images.ic_add_link), null, tint = MaterialTheme.colors.secondary)
      Spacer(Modifier.width(DEFAULT_PADDING_HALF))
      ReadableText(MR.strings.share_1_time_link_with_a_friend, style = MaterialTheme.typography.h3.copy(fontWeight = FontWeight.Bold))
    }
    ReadableText(MR.strings.one_time_link_can_be_used_with_one_contact_only)
    ReadableText(MR.strings.you_can_set_connection_name_to_remember)

    if (!showCreateAddressButton) {
      Row(Modifier.padding(top = DEFAULT_PADDING_HALF)) {
        Icon(painterResource(MR.images.ic_shield), null, tint = MaterialTheme.colors.secondary)
        Spacer(Modifier.width(DEFAULT_PADDING_HALF))
        ReadableText(MR.strings.connection_security, style = MaterialTheme.typography.h3.copy(fontWeight = FontWeight.Bold))
      }
      ReadableText(MR.strings.simplex_address_and_1_time_links_are_safe_to_share)
      ReadableText(MR.strings.to_protect_against_your_link_replaced_compare_codes)
      ReadableTextWithLink(MR.strings.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/making-connections.html#comparison-of-1-time-invitation-links-and-simplex-contact-addresses")
    }

    if (showCreateAddressButton) {
      Spacer(Modifier.weight(1f))
      Column(Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING * 2), horizontalAlignment = Alignment.CenterHorizontally) {
        Button(
          onClick = {
            ModalManager.start.showModalCloseable { close ->
              UserAddressView(chatModel = chatModel, shareViaProfile = false, autoCreateAddress = true, close = { ModalManager.start.closeModals() })
            }
          },
          shape = CircleShape,
          contentPadding = PaddingValues(horizontal =  DEFAULT_PADDING * 2, vertical = DEFAULT_PADDING),
          colors = ButtonDefaults.buttonColors(MaterialTheme.colors.primary, disabledBackgroundColor = MaterialTheme.colors.secondary)
        ) {
          Text(stringResource(MR.strings.create_simplex_address), style = MaterialTheme.typography.h2, color = Color.White, fontSize = 18.sp, fontWeight = FontWeight.Medium)
        }

        val closeAll = { ModalManager.start.closeModals() }
        TextButton(
          onClick = {
            ModalManager.start.showModalCloseable(endButtons = { AddContactLearnMoreButton() }) { _ ->
              NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = closeAll)
            }
          },
          Modifier.padding(top = DEFAULT_PADDING, bottom = DEFAULT_PADDING * 2).clip(CircleShape)
        ) {
          Text(
            stringResource(MR.strings.create_1_time_link),
            Modifier.padding(start = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING_HALF, bottom = 5.dp),
            color = MaterialTheme.colors.primary,
            fontWeight = FontWeight.Medium,
            textAlign = TextAlign.Center
          )
        }
      }
    }
  }
}
