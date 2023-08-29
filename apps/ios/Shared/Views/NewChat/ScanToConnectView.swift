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

    var body: some View {
        ScrollView {
            VStack(alignment: .leading) {
                Text("Scan QR code")
                    .font(.largeTitle)
                    .bold()
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.vertical)

                CodeScannerView(codeTypes: [.qr], completion: processQRCode)
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

                Group {
                    sharedProfileInfo(incognitoDefault)
                    + Text(String("\n\n"))
                    + Text("If you cannot meet in person, you can **scan QR code in the video call**, or your contact can share an invitation link.")
                }
                .font(.footnote)
                .foregroundColor(.secondary)
                .padding(.horizontal)
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        }
        .background(Color(.systemGroupedBackground))
    }

    func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            if let crData = parseLinkQueryData(r.string),
               checkCRDataGroup(crData) {
                dismiss()
                AlertManager.shared.showAlert(groupLinkAlert(r.string, incognito: incognitoDefault))
            } else {
                Task { connectViaLink(r.string, dismiss: dismiss, incognito: incognitoDefault) }
            }
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
