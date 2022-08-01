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
    @Environment(\.dismiss) var dismiss: DismissAction

    var body: some View {
        VStack(alignment: .leading) {
            Text("Scan QR code")
                .font(.title)
                .padding(.vertical)
            Text("Your chat profile will be sent to your contact")
                .padding(.bottom)
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
        .frame(maxHeight: .infinity, alignment: .top)
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            Task { connectViaLink(r.string, dismiss) }
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
