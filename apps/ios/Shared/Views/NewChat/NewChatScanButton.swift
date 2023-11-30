//
//  NewChatScanButton.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 30.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct NewChatScanButton: View {
    @State private var showNewChatSheet = false

    var body: some View {
        Button {
            showNewChatSheet = true
        } label: {
            Image(systemName: "qrcode")
                .resizable()
                .scaledToFit()
                .frame(width: 20, height: 20)
        }
        .sheet(isPresented: $showNewChatSheet) {
            NewChatView(selection: .connect, showQRCodeScanner: true)
                .environment(\EnvironmentValues.refresh as! WritableKeyPath<EnvironmentValues, RefreshAction?>, nil) // fixes .refreshable in ChatListView affecting nested view
        }
    }
}

//#Preview {
//    NewChatScanButton()
//}
