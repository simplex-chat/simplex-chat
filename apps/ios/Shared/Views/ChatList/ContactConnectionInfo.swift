//
//  ContactConnectionInfo.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 30/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactConnectionInfo: View {
    @EnvironmentObject var m: ChatModel
    var contactConnection: PendingContactConnection
    var connReqInvitation: String

    var body: some View {
        ScrollView {
            VStack(alignment: .leading) {
                Text("Shared one-time link")
                    .font(.largeTitle)
                    .bold()
                    .padding(.vertical)

                EditContactConnectionAlias(contactConnection: contactConnection)
                    .padding(.bottom, 6)

                if (contactConnection.customUserProfileId != nil) {
                    HStack {
                        Image(systemName: "theatermasks").foregroundColor(.indigo).font(.footnote)
                        Spacer().frame(width: 8)
                        Text("A random profile will be sent to your contact").font(.footnote)
                    }
                    .padding(.bottom)
                } else {
                    HStack {
                        Image(systemName: "info.circle").foregroundColor(.secondary).font(.footnote)
                        Spacer().frame(width: 8)
                        Text("Your chat profile will be sent to your contact").font(.footnote)
                    }
                    .padding(.bottom)
                }
                QRCode(uri: connReqInvitation).padding(.bottom)

                Text("If you can't meet in person, **show QR code in the video call**, or share the link.")
                    .padding(.bottom)
                Button {
                    showShareSheet(items: [connReqInvitation])
                } label: {
                    Label("Share invitation link", systemImage: "square.and.arrow.up")
                }
                .frame(maxWidth: .infinity, alignment: .center)
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
            .onAppear { m.connReqInv = connReqInvitation }
            .onDisappear { m.connReqInv = nil }
        }
    }
}

struct ContactConnectionInfo_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionInfo(contactConnection: PendingContactConnection.getSampleData(), connReqInvitation: "")
    }
}
