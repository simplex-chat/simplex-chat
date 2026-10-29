//
//  ScanCodeView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CodeScanner
import AVFoundation

struct ScanCodeView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var connectionVerified: Bool
    var verify: (String?) async -> (Bool, String)?
    // do not pass
    var previewCameraStatus: AVAuthorizationStatus? = nil
    @State private var showCodeError = false

    var body: some View {
        VStack(alignment: .leading) {
            Group {
                if let previewCameraStatus {
                    ScannerInView(showQRCodeScanner: .constant(false), processQRCode: processQRCode, previewCameraStatus: previewCameraStatus)
                } else {
                    CodeScannerView(codeTypes: [.qr], scanMode: .oncePerCode, completion: processQRCode)
                }
            }
            .aspectRatio(1, contentMode: .fit)
            .cornerRadius(12)
            Text("Scan security code from your contact's app.")
                .padding(.top)
        }
        .padding()
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(isPresented: $showCodeError) {
            Alert(title: Text("Incorrect security code!"))
        }
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
                        } else {
                            showCodeError = true
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
        ScanCodeView(connectionVerified: Binding.constant(true), verify: {_ in nil}, previewCameraStatus: .denied)
    }
}
