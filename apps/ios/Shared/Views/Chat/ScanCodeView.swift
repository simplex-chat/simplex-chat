//
//  ScanCodeView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CodeScanner
import SimpleXChat

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
            let trimmed = r.string.trimmingCharacters(in: .whitespacesAndNewlines)
            switch checkLink(trimmed) {
            case .verificationCode?, nil:
                // a security code (or unrecognised text): run the existing verify path
                Task {
                    if let (ok, _) = await verify(trimmed) {
                        await MainActor.run {
                            connectionVerified = ok
                            if ok {
                                dismiss()
                            } else {
                                scanAlert = SomeAlert(alert: Alert(title: Text("Incorrect security code!")), id: "incorrectCode")
                            }
                        }
                    }
                }
            case let type?:
                // valid SimpleX code of another kind: tell the user what it is
                scanAlert = SomeAlert(alert: wrongQRCodeAlert(wrongQRCodeMessage(type)), id: "wrongQRCode")
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
