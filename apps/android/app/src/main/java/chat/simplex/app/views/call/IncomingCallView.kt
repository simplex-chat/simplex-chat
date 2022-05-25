package chat.simplex.app.views.call

import android.media.Image
import androidx.annotation.StringRes
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Contact
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.usersettings.ProfilePreview

@Composable
fun IncomingCallView(invitation: CallInvitation, chatModel: ChatModel) {
  val cm = chatModel.callManager
  IncomingCallLayout(
    invitation,
    rejectCall = { cm.endCall(invitation = invitation) },
    ignoreCall = { chatModel.activeCallInvitation.value = null },
    acceptCall = {
      val call = chatModel.activeCall.value
      if (call == null) {
        cm.answerIncomingCall(invitation = invitation)
      } else {
        withApi {
          cm.endCall(call = call)
          cm.answerIncomingCall(invitation = invitation)
        }
      }
    }
  )
}

@Composable
fun IncomingCallLayout(
  invitation: CallInvitation,
  rejectCall: () -> Unit,
  ignoreCall: () -> Unit,
  acceptCall: () -> Unit
) {
  Column(
    Modifier
      .background(if (isSystemInDarkTheme()) IncomingCallDark else IncomingCallLight)
      .padding(top = 16.dp)
      .padding(bottom = 16.dp)
      .padding(start = 16.dp)
      .padding(end = 8.dp)
  ) {
//  VStack(alignment: .leading, spacing: 6) {
    Row {
      @Composable fun CallIcon(icon: ImageVector, @StringRes descrId: Int) = Icon(icon, stringResource(descrId), tint = SimplexGreen)
      if (invitation.peerMedia == CallMediaType.Video) CallIcon(Icons.Filled.Videocam, R.string.icon_descr_video_call)
      else CallIcon(Icons.Filled.Phone, R.string.icon_descr_audio_call)
      Spacer(Modifier.width(4.dp))
      Text(invitation.callTypeText)
    }
    Spacer(Modifier.height(8.dp))
    Row(verticalAlignment = Alignment.CenterVertically) {
      ProfilePreview(profileOf = invitation.contact, size = 64.dp, color = Color.White)
      Spacer(
        Modifier
          .fillMaxWidth()
          .weight(1f))
      CallButton(R.string.reject, Icons.Filled.CallEnd, Color.Red, rejectCall)
      CallButton(R.string.ignore, Icons.Filled.Close, MaterialTheme.colors.primary, ignoreCall)
      CallButton(R.string.accept, Icons.Filled.Check, SimplexGreen, acceptCall)
    }
  }
}

@Composable
private fun CallButton(@StringRes textId: Int, icon: ImageVector, color: Color, action: () -> Unit) {
  Surface(
    shape = RoundedCornerShape(10.dp),
    color = Color.Transparent
  ) {
    Column(
      Modifier
        .clickable(onClick = action)
        .defaultMinSize(minWidth = 50.dp)
        .padding(4.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Icon(icon, stringResource(textId), tint = color, modifier = Modifier.scale(1.2f))
      Text(stringResource(textId), style = MaterialTheme.typography.body2, color = HighOrLowlight)
    }
  }
}

@Preview
@Composable
fun PreviewIncomingCallLayout() {
  SimpleXTheme {
    IncomingCallLayout(
      invitation = CallInvitation(
        contact = Contact.sampleData,
        peerMedia = CallMediaType.Audio,
        sharedKey = null
      ),
      rejectCall = {},
      ignoreCall = {},
      acceptCall = {}
    )
  }
}
