package chat.simplex.app.views.call

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Contact
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.usersettings.ProfilePreview
import kotlinx.datetime.Clock

@Composable
fun IncomingCallAlertView(invitation: RcvCallInvitation, chatModel: ChatModel) {
  val cm = chatModel.callManager
  val cxt = LocalContext.current
  val scope = rememberCoroutineScope()
  LaunchedEffect(true) { SoundPlayer.shared.start(cxt, scope, sound = !chatModel.showCallView.value) }
  DisposableEffect(true) { onDispose { SoundPlayer.shared.stop() } }
  IncomingCallAlertLayout(
    invitation,
    rejectCall = { cm.endCall(invitation = invitation) },
    ignoreCall = { chatModel.activeCallInvitation.value = null },
    acceptCall = { cm.acceptIncomingCall(invitation = invitation) }
  )
}

@Composable
fun IncomingCallAlertLayout(
  invitation: RcvCallInvitation,
  rejectCall: () -> Unit,
  ignoreCall: () -> Unit,
  acceptCall: () -> Unit
) {
  val color = if (isInDarkTheme()) IncomingCallDark else IncomingCallLight
  Column(Modifier.background(color).padding(top = 16.dp, bottom = 16.dp, start = 16.dp, end = 8.dp)) {
    IncomingCallInfo(invitation)
    Spacer(Modifier.height(8.dp))
    Row(verticalAlignment = Alignment.CenterVertically) {
      ProfilePreview(profileOf = invitation.contact, size = 64.dp, color = Color.White)
      Spacer(Modifier.fillMaxWidth().weight(1f))
      CallButton(stringResource(R.string.reject), Icons.Filled.CallEnd, Color.Red, rejectCall)
      CallButton(stringResource(R.string.ignore), Icons.Filled.Close, MaterialTheme.colors.primary, ignoreCall)
      CallButton(stringResource(R.string.accept), Icons.Filled.Check, SimplexGreen, acceptCall)
    }
  }
}

@Composable
fun IncomingCallInfo(invitation: RcvCallInvitation) {
  @Composable fun CallIcon(icon: ImageVector, descr: String) = Icon(icon, descr, tint = SimplexGreen)
  Row {
    if (invitation.callType.media == CallMediaType.Video) CallIcon(Icons.Filled.Videocam, stringResource(R.string.icon_descr_video_call))
    else CallIcon(Icons.Filled.Phone, stringResource(R.string.icon_descr_audio_call))
    Spacer(Modifier.width(4.dp))
    Text(invitation.callTypeText)
  }
}

@Composable
private fun CallButton(text: String, icon: ImageVector, color: Color, action: () -> Unit) {
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
      Icon(icon, text, tint = color, modifier = Modifier.scale(1.2f))
      Text(text, style = MaterialTheme.typography.body2, color = HighOrLowlight)
    }
  }
}

@Preview
@Composable
fun PreviewIncomingCallAlertLayout() {
  SimpleXTheme {
    IncomingCallAlertLayout(
      invitation = RcvCallInvitation(
        contact = Contact.sampleData,
        callType = CallType(media = CallMediaType.Audio, capabilities = CallCapabilities(encryption = false)),
        sharedKey = null,
        callTs = Clock.System.now()
      ),
      rejectCall = {},
      ignoreCall = {},
      acceptCall = {}
    )
  }
}
