//
//  ScanCodeView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CodeScanner

struct ScanCodeView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var connectionVerified: Bool
    var verify: (String?) async -> (Bool, String)?
    @State private var scanAlert: SomeAlert?

    var body: some View {
        VStack(alignment: .leading) {
            CodeScannerView(codeTypes: [.qr], scanMode: .oncePerCode, completion: processQRCode)
                .aspectRatio(1, contentMode: .fit)
                .cornerRadius(12)
            Text("Scan security code from your contact's app.")
                .padding(.top)
        }
        .padding()
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $scanAlert) { $0.alert }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            Task {
                if let (ok, _) = await verify(r.string) {
                    await MainActor.run {
                        connectionVerified = ok
                        if ok {
                            dismiss()
                        } else if let msg = wrongQRCodeMessage(r.string, detectSecurityCode: false) {
                            scanAlert = SomeAlert(alert: wrongQRCodeAlert(msg), id: "wrongQRCode")
                        } else {
                            scanAlert = SomeAlert(alert: Alert(title: Text("Incorrect security code!")), id: "incorrectCode")
                        }
                    }
                }
            }
        case let .failure(e):
            logger.error("ScanCodeView.processQRCode QR code error: \(e.localizedDescription)")
            dismiss()
        }
    }
}

struct ScanCodeView_Previews: PreviewProvider {
    static var previews: some View {
        ScanCodeView(connectionVerified: Binding.constant(true), verify: {_ in nil})
    }
}
