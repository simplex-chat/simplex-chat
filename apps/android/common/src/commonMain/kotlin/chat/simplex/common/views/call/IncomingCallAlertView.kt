package chat.simplex.common.views.call

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.ProfileImage
import chat.simplex.common.views.usersettings.ProfilePreview
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.platform.SoundPlayer
import kotlinx.datetime.Clock

@Composable
fun IncomingCallAlertView(invitation: RcvCallInvitation, chatModel: ChatModel) {
  val cm = chatModel.callManager
  val scope = rememberCoroutineScope()
  LaunchedEffect(true) { SoundPlayer.start(scope, sound = !chatModel.showCallView.value) }
  DisposableEffect(true) { onDispose { SoundPlayer.stop() } }
  IncomingCallAlertLayout(
    invitation,
    chatModel,
    rejectCall = { cm.endCall(invitation = invitation) },
    ignoreCall = {
      chatModel.activeCallInvitation.value = null
      ntfManager.cancelCallNotification()
    },
    acceptCall = { cm.acceptIncomingCall(invitation = invitation) }
  )
}

@Composable
fun IncomingCallAlertLayout(
  invitation: RcvCallInvitation,
  chatModel: ChatModel,
  rejectCall: () -> Unit,
  ignoreCall: () -> Unit,
  acceptCall: () -> Unit
) {
  val color = if (isInDarkTheme()) MaterialTheme.colors.surface else IncomingCallLight
  Column(Modifier.fillMaxWidth().background(color).padding(top = DEFAULT_PADDING, bottom = DEFAULT_PADDING, start = DEFAULT_PADDING, end = 8.dp)) {
    IncomingCallInfo(invitation, chatModel)
    Spacer(Modifier.height(8.dp))
    Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.SpaceBetween) {
      Row(Modifier.fillMaxWidth().weight(1f), verticalAlignment = Alignment.CenterVertically) {
        ProfilePreview(profileOf = invitation.contact, size = 64.dp)
      }
      Row(verticalAlignment = Alignment.CenterVertically) {
        CallButton(stringResource(MR.strings.reject), painterResource(MR.images.ic_call_end_filled), Color.Red, rejectCall)
        CallButton(stringResource(MR.strings.ignore), painterResource(MR.images.ic_close), MaterialTheme.colors.primary, ignoreCall)
        CallButton(stringResource(MR.strings.accept), painterResource(MR.images.ic_check_filled), SimplexGreen, acceptCall)
      }
    }
  }
}

@Composable
fun IncomingCallInfo(invitation: RcvCallInvitation, chatModel: ChatModel) {
  @Composable fun CallIcon(icon: Painter, descr: String) = Icon(icon, descr, tint = SimplexGreen)
  Row(verticalAlignment = Alignment.CenterVertically) {
    if (chatModel.users.size > 1) {
      ProfileImage(size = 32.dp, image = invitation.user.profile.image, color = MaterialTheme.colors.secondaryVariant)
      Spacer(Modifier.width(4.dp))
    }
    if (invitation.callType.media == CallMediaType.Video) CallIcon(painterResource(MR.images.ic_videocam_filled), stringResource(MR.strings.icon_descr_video_call))
    else CallIcon(painterResource(MR.images.ic_call_filled), stringResource(MR.strings.icon_descr_audio_call))
    Spacer(Modifier.width(4.dp))
    Text(invitation.callTypeText, color = MaterialTheme.colors.onBackground)
  }
}

@Composable
private fun CallButton(text: String, icon: Painter, color: Color, action: () -> Unit) {
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
      Text(text, style = MaterialTheme.typography.body2, color = MaterialTheme.colors.secondary)
    }
  }
}

@Preview
@Composable
fun PreviewIncomingCallAlertLayout() {
  SimpleXTheme {
    IncomingCallAlertLayout(
      invitation = RcvCallInvitation(
        user = User.sampleData,
        contact = Contact.sampleData,
        callType = CallType(media = CallMediaType.Audio, capabilities = CallCapabilities(encryption = false)),
        sharedKey = null,
        callTs = Clock.System.now()
      ),
      chatModel = ChatModel,
      rejectCall = {},
      ignoreCall = {},
      acceptCall = {}
    )
  }
}
