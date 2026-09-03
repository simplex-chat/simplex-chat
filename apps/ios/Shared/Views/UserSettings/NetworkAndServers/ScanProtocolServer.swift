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
        NavigationView {
            CodeScannerView(codeTypes: [.qr], scanMode: .oncePerCode, completion: processQRCode)
                .aspectRatio(1, contentMode: .fit)
                .cornerRadius(12)
                .padding()
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
                .navigationTitle("Scan server QR code")
                .navigationBarTitleDisplayMode(.large)
                .modifier(ThemedBackground(grouped: true))
                .toolbar {
                    ToolbarItem(placement: .cancellationAction) {
                        Button("Cancel") { dismiss() }
                    }
                }
                .alert(item: $scanAlert) { $0.alert }
        }
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            let trimmed = r.string.trimmingCharacters(in: .whitespacesAndNewlines)
            switch checkLink(trimmed) {
            case .server?:
                var server: UserServer = .empty
                server.server = trimmed
                addServer(server, $userServers, $serverErrors, $serverWarnings, dismiss)
            case nil:
                scanAlert = SomeAlert(
                    alert: mkAlert(title: "Invalid server address!", message: "Check server address and try again."),
                    id: "invalidServerAddress"
                )
            case let type?:
                scanAlert = SomeAlert(alert: wrongQRCodeAlert(wrongQRCodeMessage(type)), id: "wrongQRCode")
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
        .environmentObject(CurrentColors.toAppTheme())
    }
}
