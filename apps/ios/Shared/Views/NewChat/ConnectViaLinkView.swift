//
//  ConnectViaLinkView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

enum ConnectViaLinkTab: String {
    case scan
    case paste
}

struct ConnectViaLinkView: View {
    @State private var selection: ConnectViaLinkTab = connectViaLinkTabDefault.get()

    var body: some View {
        TabView(selection: $selection) {
            ScanToConnectView()
                .tabItem {
                    Label("Scan QR code", systemImage: "qrcode")
                }
                .tag(ConnectViaLinkTab.scan)
            PasteToConnectView()
                .tabItem {
                    Label("Paste received link", systemImage: "doc.plaintext")
                }
                .tag(ConnectViaLinkTab.paste)
        }
        .onChange(of: selection) { _ in
            connectViaLinkTabDefault.set(selection)
        }
    }
}

struct ConnectViaLinkView_Previews: PreviewProvider {
    static var previews: some View {
        ConnectViaLinkView()
    }
}
