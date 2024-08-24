package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ComposeContextInvitingContactMemberView() {
  val sentColor = MaterialTheme.appColors.sentMessage
  Row(
    Modifier
      .height(60.dp)
      .fillMaxWidth()
      .padding(top = 8.dp)
      .background(sentColor),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      painterResource(MR.images.ic_chat),
      stringResource(MR.strings.button_send_direct_message),
      modifier = Modifier
        .padding(start = 12.dp, end = 8.dp)
        .height(20.dp)
        .width(20.dp),
      tint = MaterialTheme.colors.secondary
    )
    Text(generalGetString(MR.strings.compose_send_direct_message_to_connect))
  }
}
