package chat.simplex.common.views.call

import androidx.compose.runtime.Composable
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import kotlinx.coroutines.*

@Composable
expect fun ActiveCallView()

fun activeCallWaitDeliveryReceipt(scope: CoroutineScope) = scope.launch(Dispatchers.Default) {
  for (apiResp in controller.messagesChannel) {
    val call = chatModel.activeCall.value
    if (call == null || call.callState > CallState.InvitationSent) break
    val msg = apiResp.resp
    if (apiResp.remoteHostId == call.remoteHostId &&
      msg is CR.ChatItemsStatusesUpdated &&
      msg.chatItems.any {
        it.chatInfo.id == call.contact.id && it.chatItem.content is CIContent.SndCall && it.chatItem.meta.itemStatus is CIStatus.SndRcvd
      }
    ) {
      CallSoundsPlayer.startInCallSound(scope)
      break
    }
  }
}
