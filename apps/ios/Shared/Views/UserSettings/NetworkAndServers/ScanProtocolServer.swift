//
//  ScanProtocolServer.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import CodeScanner

struct ScanProtocolServer: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]

    var body: some View {
        VStack(alignment: .leading) {
            Text("Scan server QR code")
                .font(.largeTitle)
                .bold()
                .padding(.vertical)
            CodeScannerView(codeTypes: [.qr], scanMode: .oncePerCode, completion: processQRCode)
                .aspectRatio(1, contentMode: .fit)
                .cornerRadius(12)
                .padding(.top)
        }
        .padding()
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            var server: UserServer = .empty
            server.server = r.string
            addServer(server, $userServers, $serverErrors, dismiss)
        case let .failure(e):
            logger.error("ScanProtocolServer.processQRCode QR code error: \(e.localizedDescription)")
            dismiss()
        }
    }
}

struct ScanProtocolServer_Previews: PreviewProvider {
    static var previews: some View {
        ScanProtocolServer(
            userServers: Binding.constant([UserOperatorServers.sampleDataNilOperator]),
            serverErrors: Binding.constant([])
        )
    }
}
