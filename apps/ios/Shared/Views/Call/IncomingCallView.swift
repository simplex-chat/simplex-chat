//
//  IncomingCallView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct IncomingCallView: View {
    @ObservedObject var callController = CallController.shared

    var body: some View {
        if let invitation = callController.activeCallInvitation {
            VStack(alignment: .leading, spacing: 6) {
                HStack {
                    Image(systemName: invitation.peerMedia == .video ? "video.fill" : "phone.fill").foregroundColor(.green)
                    Text(invitation.callTypeText)
                }
                HStack {
                    ProfilePreview(profileOf: invitation.contact, color: .white)
                    Spacer()

                    callButton("Reject", "phone.down.fill", .red) {
                        callController.endCall(invitation: invitation)
                    }

                    callButton("Ignore", "multiply", .accentColor) {
                        callController.activeCallInvitation = nil
                    }

                    callButton("Accept", "checkmark", .green) {
                        callController.answerCall(invitation: invitation)
                    }
                }
            }
            .onAppear {
                SoundPlayer.shared.startRingtone()
            }
            .onDisappear {
                SoundPlayer.shared.stopRingtone()
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 12)
            .frame(maxWidth: .infinity)
//            .background(.secondary)
            .background(Color(uiColor: .tertiarySystemGroupedBackground))
        }
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
        CallController.shared.activeCallInvitation = CallInvitation.sampleData
        return IncomingCallView()
    }
}
