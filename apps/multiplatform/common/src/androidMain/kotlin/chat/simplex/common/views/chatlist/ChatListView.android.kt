package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.Contact
import chat.simplex.common.model.durationText
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.datetime.Clock

@Composable
actual fun ActiveCallIntractableArea(newChatSheetState: MutableStateFlow<AnimatedViewState>) {
  val call = remember { chatModel.activeCall }.value// ?: Call(remoteHostId = null, contact = Contact.sampleData, callState = CallState.Connected, localMedia = CallMediaType.Audio)
  if (call != null) {
    val media = call.peerMedia ?: call.localMedia
    Row(
      Modifier
        .fillMaxWidth()
        .background(SimplexGreen)
        .combinedClickable(onClick = {
          val chat = chatModel.getChat(call.contact.id)
          if (chat != null) {
            withBGApi {
              chatModel.activeCallViewIsCollapsed.value = false
              openChat(chat.remoteHostId, chat.chatInfo, chatModel)
            }
          }
        })
        .padding(vertical = DEFAULT_PADDING_HALF, horizontal = DEFAULT_PADDING),
      verticalAlignment = Alignment.CenterVertically
    ) {
      if (chatModel.users.size > 1) {
        ProfileImage(size = 32.dp, image = chatModel.activeCallInvitation.value?.user?.profile?.image, color = MaterialTheme.colors.secondaryVariant)
        Spacer(Modifier.width(4.dp))
      }
      if (media == CallMediaType.Video) CallIcon(painterResource(MR.images.ic_videocam_filled), stringResource(MR.strings.icon_descr_video_call))
      else CallIcon(painterResource(MR.images.ic_call_filled), stringResource(MR.strings.icon_descr_audio_call))
      Spacer(Modifier.width(4.dp))
      ProfileImage(size = 32.dp, image = call.contact.profile.image)
      Spacer(Modifier.weight(1f))
      CallDuration(call)
    }
  }
  DisposableEffectOnGone {
    chatModel.activeCallViewIsCollapsed.value = false
  }
}

@Composable
private fun CallIcon(icon: Painter, descr: String) = Icon(icon, descr, tint = Color.White)

@Composable
private fun CallDuration(call: Call) {
  val connectedAt = call.connectedAt
  if (connectedAt != null) {
    val time = remember { mutableStateOf(durationText(0)) }
    LaunchedEffect(Unit) {
      while (true) {
        time.value = durationText((Clock.System.now() - connectedAt).inWholeSeconds.toInt())
        delay(250)
      }
    }
    val sp50 = with(LocalDensity.current) { 50.sp.toDp() }
    Text(time.value, Modifier.widthIn(min = sp50), color = Color.White)
  }
}
