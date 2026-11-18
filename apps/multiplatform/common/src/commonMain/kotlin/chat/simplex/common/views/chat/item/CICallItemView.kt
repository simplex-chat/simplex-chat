package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.*
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.SimpleButton
import chat.simplex.res.*

@Composable
fun CICallItemView(
  cInfo: ChatInfo,
  cItem: ChatItem,
  status: CICallStatus,
  duration: Int,
  showTimestamp: Boolean,
  acceptCall: (Contact) -> Unit,
  timedMessagesTTL: Int?
) {
  val sent = cItem.chatDir.sent
  ItemWithMeta(meta = { CIMetaView(cItem, timedMessagesTTL, showStatus = false, showEdited = false, showViaProxy = false, showTimestamp = showTimestamp) }) { inlineMeta ->
    Column(
      Modifier
        // Заменили CHAT_ITEM_TEXT_PADDING на 4.dp
        .padding(horizontal = 4.dp)
        .padding(bottom = 8.dp), horizontalAlignment = Alignment.CenterHorizontally) {
      @Composable fun ConnectingCallIcon() = Icon(painterResource(MR.images.ic_settings_phone), stringResource(MR.strings.icon_descr_call_connecting), tint = SimplexGreen)
      when (status) {
        CICallStatus.Pending -> if (sent) {
          Icon(painterResource(MR.images.ic_call), stringResource(MR.strings.icon_descr_call_pending_sent))
        } else {
          AcceptCallButton(cInfo, acceptCall)
        }
        CICallStatus.Missed -> Icon(painterResource(MR.images.ic_call), stringResource(MR.strings.icon_descr_call_missed), tint = Color.Red)
        CICallStatus.Rejected -> Icon(painterResource(MR.images.ic_call_end), stringResource(MR.strings.icon_descr_call_rejected), tint = Color.Red)
        CICallStatus.Accepted -> ConnectingCallIcon()
        CICallStatus.Negotiated -> ConnectingCallIcon()
        CICallStatus.Progress -> Icon(painterResource(MR.images.ic_phone_in_talk_filled), stringResource(MR.strings.icon_descr_call_progress), tint = SimplexGreen)
        CICallStatus.Ended -> Row {
          Icon(painterResource(MR.images.ic_call_end), stringResource(MR.strings.icon_descr_call_ended), tint = MaterialTheme.colors.secondary, modifier = Modifier.padding(end = 4.dp))
          Text(durationText(duration), color = MaterialTheme.colors.secondary)
        }
        CICallStatus.Error -> {}
      }

      inlineMeta?.invoke()
    }
  }
}

@Composable
fun AcceptCallButton(cInfo: ChatInfo, acceptCall: (Contact) -> Unit) {
  if (cInfo is ChatInfo.Direct) {
    SimpleButton(stringResource(MR.strings.answer_call), painterResource(MR.images.ic_ring_volume)) { acceptCall(cInfo.contact) }
  } else {
    Icon(painterResource(MR.images.ic_ring_volume), stringResource(MR.strings.answer_call), tint = MaterialTheme.colors.secondary)
  }
}
