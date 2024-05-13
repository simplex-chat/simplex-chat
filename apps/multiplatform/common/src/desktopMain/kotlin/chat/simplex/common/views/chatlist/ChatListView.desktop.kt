package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.Call
import chat.simplex.common.views.call.CallMediaType
import chat.simplex.common.views.chat.item.ItemAction
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
actual fun ActiveCallInteractiveArea(call: Call, newChatSheetState: MutableStateFlow<AnimatedViewState>) {
  //  if (call.callState == CallState.Connected && !newChatSheetState.collectAsState().value.isVisible()) {
  if (!newChatSheetState.collectAsState().value.isVisible()) {
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
              .padding(end = 71.dp, bottom = 92.dp)
              .size(67.dp)
              .combinedClickable(onClick = {
                val chat = chatModel.getChat(call.contact.id)
                if (chat != null) {
                  withBGApi {
                    openChat(chat.remoteHostId, chat.chatInfo, chatModel)
                  }
                }
              },
                onLongClick = { showMenu.value = true })
              .onRightClick { showMenu.value = true },
            contentAlignment = Alignment.Center
          ) {
            Box(Modifier.background(MaterialTheme.colors.background, CircleShape)) {
              ProfileImageForActiveCall(size = 56.dp, image = call.contact.profile.image)
            }
            Box(Modifier.padding().background(SimplexGreen, CircleShape).padding(4.dp).align(Alignment.TopEnd)) {
              if (media == CallMediaType.Video) {
                Icon(painterResource(MR.images.ic_videocam_filled), stringResource(MR.strings.icon_descr_video_call), Modifier.size(18.dp), tint = Color.White)
              } else {
                Icon(painterResource(MR.images.ic_call_filled), stringResource(MR.strings.icon_descr_audio_call), Modifier.size(18.dp), tint = Color.White)
              }
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
