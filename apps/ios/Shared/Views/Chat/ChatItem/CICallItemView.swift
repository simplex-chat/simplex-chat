//
//  CICallItemView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 20/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CICallItemView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var MaterialTheme: MaterialTheme
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    var status: CICallStatus
    var duration: Int

    var body: some View {
        let sent = chatItem.chatDir.sent
        VStack(spacing: 4) {
            switch status {
            case .pending:
                if sent {
                    Image(systemName: "phone.arrow.up.right").foregroundColor(.secondary)
                } else {
                    acceptCallButton()
                }
            case .missed: missedCallIcon(sent).foregroundColor(.red)
            case .rejected: Image(systemName: "phone.down").foregroundColor(.red)
            case .accepted: connectingCallIcon()
            case .negotiated: connectingCallIcon()
            case .progress: Image(systemName: "phone.and.waveform.fill").foregroundColor(.green)
            case .ended: endedCallIcon(sent)
            case .error: missedCallIcon(sent).foregroundColor(.orange)
            }

            CIMetaView(chat: chat, chatItem: chatItem, metaColor: MaterialTheme.colors.secondary, showStatus: false, showEdited: false)
                .padding(.bottom, 8)
                .padding(.horizontal, 12)
        }
    }

    private func missedCallIcon(_ sent: Bool) -> some View {
        Image(systemName: sent ? "phone.arrow.up.right" : "phone.arrow.down.left")
    }

    private func connectingCallIcon() -> some View {
        Image(systemName: "phone.connection").foregroundColor(.green)
    }

    @ViewBuilder private func endedCallIcon(_ sent: Bool) -> some View {
        HStack {
            Image(systemName: "phone.down")
            Text(durationText(duration)).foregroundColor(.secondary)
        }
    }


    @ViewBuilder private func acceptCallButton() -> some View {
        if case let .direct(contact) = chat.chatInfo {
            Button {
                if let invitation = m.callInvitations[contact.id] {
                    CallController.shared.answerCall(invitation: invitation)
                    logger.debug("acceptCallButton call answered")
                } else {
                    AlertManager.shared.showAlertMsg(title: "Call already ended!")
                }
            } label: {
                Label("Answer call", systemImage: "phone.arrow.down.left")
            }
        } else {
            Image(systemName: "phone.arrow.down.left").foregroundColor(.secondary)
        }
    }
}

//struct CICallItemView_Previews: PreviewProvider {
//    static var previews: some View {
//        CICallItemView()
//    }
//}
