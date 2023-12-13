//
//  ConnectContactView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import CodeScanner

struct ScanToConnectView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false
    @State private var alert: PlanAndConnectAlert?
    @State private var sheet: PlanAndConnectActionSheet?

    var body: some View {
        ScrollView {
            VStack(alignment: .leading) {
                Text("Scan QR code")
                    .font(.largeTitle)
                    .bold()
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.vertical)

                CodeScannerView(codeTypes: [.qr], scanMode: .continuous, completion: processQRCode)
                    .aspectRatio(1, contentMode: .fit)
                    .cornerRadius(12)

                IncognitoToggle(incognitoEnabled: $incognitoDefault)
                    .padding(.horizontal)
                    .padding(.vertical, 6)
                    .background(
                        RoundedRectangle(cornerRadius: 12, style: .continuous)
                            .fill(Color(uiColor: .systemBackground))
                    )
                    .padding(.top)

                VStack(alignment: .leading, spacing: 4) {
                    sharedProfileInfo(incognitoDefault)
                    Text("If you cannot meet in person, you can **scan QR code in the video call**, or your contact can share an invitation link.")
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                .font(.footnote)
                .foregroundColor(.secondary)
                .padding(.horizontal)
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        }
        .background(Color(.systemGroupedBackground))
        .alert(item: $alert) { a in planAndConnectAlert(a, dismiss: true) }
        .actionSheet(item: $sheet) { s in planAndConnectActionSheet(s, dismiss: true) }
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            planAndConnect(
                r.string,
                showAlert: { alert = $0 },
                showActionSheet: { sheet = $0 },
                dismiss: true,
                incognito: incognitoDefault
            )
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
