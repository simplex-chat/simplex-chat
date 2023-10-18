package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
actual fun DesktopActiveCallOverlayLayout(newChatSheetState: MutableStateFlow<AnimatedViewState>) {
  val call = remember { chatModel.activeCall}.value
  //  if (call?.callState == CallState.Connected && !newChatSheetState.collectAsState().value.isVisible()) {
  if (call != null && !newChatSheetState.collectAsState().value.isVisible()) {
      val showMenu = remember { mutableStateOf(false) }
      val media = call.peerMedia ?: call.localMedia
      CompositionLocalProvider(
        LocalIndication provides NoIndication
      ) {
        Box(
          Modifier
            .fillMaxSize(),
          contentAlignment = Alignment.BottomEnd
        ) {
          Box(
            Modifier
              .padding(end = 84.dp, bottom = 96.dp)
              .background(
                MaterialTheme.colors.primary.mixWith(MaterialTheme.colors.background, 0.5f),
                RoundedCornerShape(50)
              )
              .combinedClickable(onClick = {
                val chat = chatModel.getChat(call.contact.id)
                if (chat != null) {
                  withApi {
                    openChat(chat.chatInfo, chatModel)
                  }
                }
              },
                onLongClick = { showMenu.value = true })
              .onRightClick { showMenu.value = true },
            contentAlignment = Alignment.BottomEnd
          ) {
            ProfileImage(size = 40.dp, image = call.contact.profile.image)
            if (media == CallMediaType.Video) {
              Icon(painterResource(MR.images.ic_videocam_filled), stringResource(MR.strings.icon_descr_video_call), Modifier.size(12.dp), tint = SimplexGreen)
            } else {
              Icon(painterResource(MR.images.ic_call_filled), stringResource(MR.strings.icon_descr_audio_call), Modifier.size(12.dp), tint = SimplexGreen)
            }
            DefaultDropdownMenu(showMenu) {
              ItemAction(stringResource(MR.strings.icon_descr_hang_up), painterResource(MR.images.ic_call_end_filled), color = MaterialTheme.colors.error, onClick = {
                withBGApi { chatModel.callManager.endCall(call) }
                showMenu.value = false
              })
            }
          }
        }
      }
    }
}
