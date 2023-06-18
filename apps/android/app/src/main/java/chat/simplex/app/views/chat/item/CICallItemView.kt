package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*

@Composable
fun CICallItemView(cInfo: ChatInfo, cItem: ChatItem, status: CICallStatus, duration: Int, acceptCall: (Contact) -> Unit) {
  val sent = cItem.chatDir.sent
  Column(
    Modifier
      .padding(horizontal = 4.dp)
      .padding(bottom = 8.dp), horizontalAlignment = Alignment.CenterHorizontally) {
    @Composable fun ConnectingCallIcon() = Icon(painterResource(R.drawable.ic_settings_phone), stringResource(R.string.icon_descr_call_connecting), tint = SimplexGreen)
    when (status) {
      CICallStatus.Pending -> if (sent) {
        Icon(painterResource(R.drawable.ic_call), stringResource(R.string.icon_descr_call_pending_sent))
      } else {
        AcceptCallButton(cInfo, acceptCall)
      }
      CICallStatus.Missed -> Icon(painterResource(R.drawable.ic_call), stringResource(R.string.icon_descr_call_missed), tint = Color.Red)
      CICallStatus.Rejected -> Icon(painterResource(R.drawable.ic_call_end), stringResource(R.string.icon_descr_call_rejected), tint = Color.Red)
      CICallStatus.Accepted -> ConnectingCallIcon()
      CICallStatus.Negotiated -> ConnectingCallIcon()
      CICallStatus.Progress -> Icon(painterResource(R.drawable.ic_phone_in_talk_filled), stringResource(R.string.icon_descr_call_progress), tint = SimplexGreen)
      CICallStatus.Ended -> Row {
        Icon(painterResource(R.drawable.ic_call_end), stringResource(R.string.icon_descr_call_ended), tint = MaterialTheme.colors.secondary, modifier = Modifier.padding(end = 4.dp))
        Text(durationText(duration), color = MaterialTheme.colors.secondary)
      }
      CICallStatus.Error -> {}
    }

    Text(
      cItem.timestampText,
      color = MaterialTheme.colors.secondary,
      fontSize = 14.sp,
      modifier = Modifier.padding(start = 3.dp)
    )
  }
}

@Composable
fun AcceptCallButton(cInfo: ChatInfo, acceptCall: (Contact) -> Unit) {
  if (cInfo is ChatInfo.Direct) {
    SimpleButton(stringResource(R.string.answer_call), painterResource(R.drawable.ic_ring_volume)) { acceptCall(cInfo.contact) }
  } else {
    Icon(painterResource(R.drawable.ic_ring_volume), stringResource(R.string.answer_call), tint = MaterialTheme.colors.secondary)
  }
//    if case let .direct(contact) = chatInfo {
//      Button {
//        if let invitation = m.callInvitations.removeValue(forKey: contact.id) {
//        m.activeCallInvitation = nil
//        m.activeCall = Call(
//          contact: contact,
//          callState: .invitationReceived,
//        localMedia: invitation.peerMedia,
//        sharedKey: invitation.sharedKey
//        )
//        m.showCallView = true
//        m.callCommand = .start(media: invitation.peerMedia, aesKey: invitation.sharedKey, useWorker: true)
//      } else {
//        AlertManager.shared.showAlertMsg(title: "Call already ended!")
//      }
//      } label: {
//        Label("Answer call", systemImage: "phone.arrow.down.left")
//      }
//    } else {
//      Image(systemName: "phone.arrow.down.left").foregroundColor(.secondary)
//    }

}

//struct CICallItemView: View {
//  @EnvironmentObject var m: ChatModel
//  var chatInfo: ChatInfo
//  var chatItem: ChatItem
//  var status: CICallStatus
//  var duration: Int
//
//  var body: some View {
//    switch status {
//      case .pending:
//      if sent {
//        Image(systemName: "phone.arrow.up.right").foregroundColor(.secondary)
//      } else {
//        acceptCallButton()
//      }
//      case .missed: missedCallIcon(sent).foregroundColor(.red)
//      case .rejected: Image(systemName: "phone.down").foregroundColor(.secondary)
//      case .accepted: connectingCallIcon()
//      case .negotiated: connectingCallIcon()
//      case .progress: Image(systemName: "phone.and.waveform.fill").foregroundColor(.green)
//      case .ended: endedCallIcon(sent)
//      case .error: missedCallIcon(sent).foregroundColor(.orange)
//    }
//
//    chatItem.timestampText
//      .font(.caption)
//    .foregroundColor(.secondary)
//    .padding(.bottom, 8)
//    .padding(.horizontal, 12)
//  }
//  }
//
//  private func missedCallIcon(_ sent: Bool) -> some View {
//    Image(systemName: sent ? "phone.arrow.up.right" : "phone.arrow.down.left")
//  }
//
//  private func connectingCallIcon() -> some View {
//    Image(systemName: "phone.connection").foregroundColor(.green)
//  }
//
//  @ViewBuilder private func endedCallIcon(_ sent: Bool) -> some View {
//    HStack {
//      Image(systemName: "phone.down")
//      Text(CICallStatus.durationText(duration)).foregroundColor(.secondary)
//    }
//  }
//
//
//  @ViewBuilder private func acceptCallButton() -> some View {
//    if case let .direct(contact) = chatInfo {
//      Button {
//        if let invitation = m.callInvitations.removeValue(forKey: contact.id) {
//        m.activeCallInvitation = nil
//        m.activeCall = Call(
//          contact: contact,
//          callState: .invitationReceived,
//        localMedia: invitation.peerMedia,
//        sharedKey: invitation.sharedKey
//        )
//        m.showCallView = true
//        m.callCommand = .start(media: invitation.peerMedia, aesKey: invitation.sharedKey, useWorker: true)
//      } else {
//        AlertManager.shared.showAlertMsg(title: "Call already ended!")
//      }
//      } label: {
//        Label("Answer call", systemImage: "phone.arrow.down.left")
//      }
//    } else {
//      Image(systemName: "phone.arrow.down.left").foregroundColor(.secondary)
//    }
//  }
//}
