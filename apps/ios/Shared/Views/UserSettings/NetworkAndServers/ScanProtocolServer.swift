//
//  ScanProtocolServer.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/architecture.md

import SwiftUI
import SimpleXChat
import CodeScanner

struct ScanProtocolServer: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    @Binding var serverWarnings: [UserServersWarning]
    @State private var scanAlert: SomeAlert?

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
        .alert(item: $scanAlert) { $0.alert }
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            if parseServerAddress(r.string) != nil {
                var server: UserServer = .empty
                server.server = r.string
                addServer(server, $userServers, $serverErrors, $serverWarnings, dismiss)
            } else {
                scanAlert = SomeAlert(
                    alert: wrongQRCodeAlert(wrongQRCodeMessage(r.string) ?? notSimplexQRCodeMessage()),
                    id: "wrongQRCode"
                )
            }
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
            serverErrors: Binding.constant([]),
            serverWarnings: Binding.constant([])
        )
    }
}
