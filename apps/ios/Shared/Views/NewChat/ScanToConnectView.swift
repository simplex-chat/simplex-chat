//
//  ConnectContactView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CodeScanner

struct ScanToConnectView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction

    var body: some View {
        ScrollView {
            VStack(alignment: .leading) {
                Text("Scan QR code")
                    .font(.largeTitle)
                    .bold()
                    .padding(.vertical)
                if (chatModel.incognito) {
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
                ZStack {
                    CodeScannerView(codeTypes: [.qr], completion: processQRCode)
                        .aspectRatio(1, contentMode: .fit)
                        .border(.gray)
                }
                .padding(.bottom)
                Text("If you cannot meet in person, you can **scan QR code in the video call**, or your contact can share an invitation link.")
                    .padding(.bottom)
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        }
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            if let crData = parseLinkQueryData(r.string),
               checkCRDataGroup(crData) {
                dismiss()
                AlertManager.shared.showAlert(groupLinkAlert(r.string))
            } else {
                Task { connectViaLink(r.string, dismiss) }
            }
        case let .failure(e):
            logger.error("ConnectContactView.processQRCode QR code error: \(e.localizedDescription)")
            dismiss()
        }
    }
}

struct ConnectContactView_Previews: PreviewProvider {
    static var previews: some View {
        ScanToConnectView()
    }
}
