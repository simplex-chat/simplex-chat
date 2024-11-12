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
    @Binding var smpServers: [UserServer]
    @Binding var xftpServers: [UserServer]
    @State private var showAddressError = false

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
        .alert(isPresented: $showAddressError) {
            Alert(
                title: Text("Invalid server address!"),
                message: Text("Check server address and try again.")
            )
        }
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            if let serverAddress = parseServerAddress(r.string) {
                if serverAddress.serverProtocol == .smp {
                    smpServers.append(UserServer(server: r.string, preset: false, tested: nil, enabled: false))
                } else {
                    xftpServers.append(UserServer(server: r.string, preset: false, tested: nil, enabled: false))
                }
                dismiss()
            } else {
                showAddressError = true
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
            smpServers: Binding.constant([]),
            xftpServers: Binding.constant([])
        )
    }
}
