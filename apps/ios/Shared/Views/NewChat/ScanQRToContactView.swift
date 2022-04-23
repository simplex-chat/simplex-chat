//
//  ConnectContactView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CodeScanner

struct ScanQRToContactView: View {
    var completed: ((Result<Bool, Error>) -> Void)

    var body: some View {
        VStack {
            Text("Scan QR code")
                .font(.title)
                .padding(.bottom)
            Text("Your chat profile will be sent to your contact")
                .font(.title2)
                .multilineTextAlignment(.center)
                .padding()
            ZStack {
                CodeScannerView(codeTypes: [.qr], completion: processQRCode)
                    .aspectRatio(1, contentMode: .fit)
                    .border(.gray)
            }
            .padding(12)
            Text("If you cannot meet in person, you can **scan QR code in the video call**, or your contact can share an invitation link.")
                .font(.subheadline)
                .multilineTextAlignment(.center)
                .padding(.horizontal)
        }
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            Task {
                do {
                    let ok = try await apiConnect(connReq: r.string)
                    completed(.success(ok))
                } catch {
                    logger.error("ConnectContactView.processQRCode apiConnect error: \(error.localizedDescription)")
                    completed(.failure(error))
                }
            }
        case let .failure(e):
            logger.error("ConnectContactView.processQRCode QR code error: \(e.localizedDescription)")
            completed(.failure(e))
        }
    }
}

struct ConnectContactView_Previews: PreviewProvider {
    static var previews: some View {
        return ScanQRToContactView(completed: {_ in })
    }
}
