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
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var selection: ConnectViaLinkTab = connectViaLinkTabDefault.get()

    private let minDragTranslationForSwipe: CGFloat = 50

    var body: some View {
        TabView(selection: $selection) {
            ScanToConnectView()
                .tabItem {
                    Label("Scan QR code", systemImage: "qrcode")
                }
                .tag(ConnectViaLinkTab.scan)
                .highPriorityGesture(DragGesture().onEnded({
                    handleSwipe(translation: $0.translation)
                }))
            PasteToConnectView()
                .tabItem {
                    Label("Paste received link", systemImage: "doc.plaintext")
                }
                .tag(ConnectViaLinkTab.paste)
                .highPriorityGesture(DragGesture().onEnded({
                    handleSwipe(translation: $0.translation)
                }))
        }
        .onChange(of: selection) { _ in
            connectViaLinkTabDefault.set(selection)
        }
    }

    private func handleSwipe(translation: CGSize) {
        if translation.width > minDragTranslationForSwipe && selection == .paste {
            selection = .scan
        } else if translation.width < -minDragTranslationForSwipe && selection == .scan {
            selection = .paste
        } else if translation.height > minDragTranslationForSwipe {
            dismiss()
        }
    }
}

struct ConnectViaLinkView_Previews: PreviewProvider {
    static var previews: some View {
        ConnectViaLinkView()
    }
}
