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

@Composable
fun ComposeContextInvitingContactMemberView() {
  val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
  Row(
    Modifier
      .height(60.dp)
      .fillMaxWidth()
      .padding(top = 8.dp)
      .background(sentColor),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.Center
  ) {
    Text(generalGetString(MR.strings.compose_send_invitation_to_connect_directly))
  }
}
