//
//  ConnectContactView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CodeScanner

struct ConnectContactView: View {
    var completed: ((Error?) -> Void)

    var body: some View {
        VStack {
            Text("Scan QR code")
                .font(.title)
                .padding(.bottom)
            Text("Your chat profile will be sent to your contact.")
                .font(.title2)
                .multilineTextAlignment(.center)
                .padding()
            ZStack {
                CodeScannerView(codeTypes: [.qr], completion: processQRCode)
                    .aspectRatio(1, contentMode: .fit)
                    .border(.gray)
            }
            .padding(13.0)
        }
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            Task {
                do {
                    try await apiConnect(connReq: r.string)
                    completed(nil)
                } catch {
                    logger.error("ConnectContactView.processQRCode apiConnect error: \(error.localizedDescription)")
                    completed(error)
                }
            }
        case let .failure(e):
            logger.error("ConnectContactView.processQRCode QR code error: \(e.localizedDescription)")
            completed(e)
        }
    }
}

struct ConnectContactView_Previews: PreviewProvider {
    static var previews: some View {
        return ConnectContactView(completed: {_ in })
    }
}
