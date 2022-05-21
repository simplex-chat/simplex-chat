package chat.simplex.app.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.Icon
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.PhoneInTalk
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleButton

@Composable
fun CICallItemView(cInfo: ChatInfo, cItem: ChatItem, status: CICallStatus, duration: Int, acceptCall: (Contact) -> Unit) {
  val sent = cItem.chatDir.sent
  Column(
    Modifier
      .padding(horizontal = 4.dp)
      .padding(bottom = 8.dp), horizontalAlignment = Alignment.CenterHorizontally) {
    @Composable fun ConnectingCallIcon() = Icon(Icons.Outlined.SettingsPhone, stringResource(R.string.icon_descr_call_connecting), tint = Color.Green)
    when (status) {
      CICallStatus.Pending -> if (sent) {
        Icon(Icons.Outlined.Call, stringResource(R.string.icon_descr_call_pending_sent))
      } else {
        AcceptCallButton(cInfo, acceptCall)
      }
      CICallStatus.Missed -> Icon(Icons.Outlined.Call, stringResource(R.string.icon_descr_call_missed), tint = Color.Red)
      CICallStatus.Rejected -> Icon(Icons.Outlined.CallEnd, stringResource(R.string.icon_descr_call_rejected), tint = HighOrLowlight)
      CICallStatus.Accepted -> ConnectingCallIcon()
      CICallStatus.Negotiated -> ConnectingCallIcon()
      CICallStatus.Progress -> Icon(Icons.Filled.PhoneInTalk, stringResource(R.string.icon_descr_call_progress), tint = Color.Green)
      CICallStatus.Ended -> Row {
        Icon(Icons.Outlined.CallEnd, stringResource(R.string.icon_descr_call_ended), tint = HighOrLowlight, modifier = Modifier.padding(end = 4.dp))
        Text(status.duration(duration), color = HighOrLowlight)
      }
    }

    Text(
      cItem.timestampText,
      color = HighOrLowlight,
      fontSize = 14.sp,
      modifier = Modifier.padding(start = 3.dp)
    )
  }
}

@Composable
fun AcceptCallButton(cInfo: ChatInfo, acceptCall: (Contact) -> Unit) {
  if (cInfo is ChatInfo.Direct) {
    SimpleButton(stringResource(R.string.answer_call), Icons.Outlined.RingVolume) { acceptCall(cInfo.contact) }
  } else {
    Icon(Icons.Outlined.RingVolume, stringResource(R.string.answer_call), tint = HighOrLowlight)
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