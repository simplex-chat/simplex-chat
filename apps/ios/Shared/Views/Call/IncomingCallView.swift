//
//  IncomingCallView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct IncomingCallView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var cc = CallController.shared

    var body: some View {
        let sp = SoundPlayer.shared
        if let invitation = cc.activeCallInvitation {
            if m.showCallView {
                incomingCall(invitation)
            } else {
                incomingCall(invitation)
                .onAppear { sp.startRingtone() }
                .onDisappear { sp.stopRingtone() }
            }
        }
    }

    private func incomingCall(_ invitation: RcvCallInvitation) -> some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack {
                if m.users.count > 1 {
                    ProfileImage(imageStr: invitation.user.image, size: 24, color: .white)
                }
                Image(systemName: invitation.callType.media == .video ? "video.fill" : "phone.fill").foregroundColor(.green)
                Text(invitation.callTypeText)
            }
            HStack {
                ProfilePreview(profileOf: invitation.contact, color: .white)
                Spacer()

                callButton("Reject", "phone.down.fill", .red) {
                    cc.endCall(invitation: invitation)
                }

                callButton("Ignore", "multiply", theme.colors.primary) {
                    cc.activeCallInvitation = nil
                }

                callButton("Accept", "checkmark", .green) {
                    if let call = m.activeCall {
                        cc.endCall(call: call) {
                            DispatchQueue.main.async {
                                cc.answerCall(invitation: invitation)
                            }
                        }
                    } else {
                        cc.answerCall(invitation: invitation)
                    }
                }
            }
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 12)
        .frame(maxWidth: .infinity)
        .modifier(ThemedBackground())
        .onAppear { dismissAllSheets() }
    }

    private func callButton(_ text: LocalizedStringKey, _ image: String, _ color: Color, action: @escaping () -> Void) -> some View {
        Button(action: action, label: {
            VStack(spacing: 2) {
                Image(systemName: image)
                    .scaleEffect(1.24)
                    .foregroundColor(color)
                    .frame(width: 24, height: 24)
                Text(text)
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            .frame(minWidth: 44)
        })
    }
}

struct IncomingCallView_Previews: PreviewProvider {
    static var previews: some View {
        CallController.shared.activeCallInvitation = RcvCallInvitation.sampleData
        let m = ChatModel()
        m.users = [UserInfo.sampleData, UserInfo.sampleData]
        return IncomingCallView().environmentObject(m)
    }
}
