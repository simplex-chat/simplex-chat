package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.views.onboarding.ReadableTextWithLink
import chat.simplex.res.MR

@Composable
fun UserAddressLearnMore(showCreateAddressButton: Boolean = false) {
  ColumnWithScrollBar(Modifier .padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.simplex_address), withPadding = false)
    ReadableText(MR.strings.you_can_share_your_address)
    ReadableText(MR.strings.you_wont_lose_your_contacts_if_delete_address)
    ReadableText(MR.strings.you_can_accept_or_reject_connection)
    ReadableTextWithLink(MR.strings.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/app-settings.html#your-simplex-contact-address")

    if (showCreateAddressButton) {
      Spacer(Modifier.weight(1f))
      Column(Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING * 2), horizontalAlignment = Alignment.CenterHorizontally) {
        Button(
          onClick = {
            ModalManager.start.showModalCloseable { close ->
              UserAddressView(chatModel = chatModel, shareViaProfile = false, autoCreateAddress = true, close = close)
            }
          },
          shape = CircleShape,
          contentPadding = PaddingValues(horizontal =  DEFAULT_PADDING * 2, vertical = DEFAULT_PADDING),
          colors = ButtonDefaults.buttonColors(MaterialTheme.colors.primary, disabledBackgroundColor = MaterialTheme.colors.secondary)
        ) {
          Text(stringResource(MR.strings.create_simplex_address), style = MaterialTheme.typography.h2, color = Color.White, fontSize = 18.sp, fontWeight = FontWeight.Medium)
        }
      }
    }
  }
}
