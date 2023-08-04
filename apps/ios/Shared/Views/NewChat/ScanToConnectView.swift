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
                    Text(incognitoDefault ? "A new randomly generated profile will be shared." : "Current profile will be shared.")
                    + Text("\n\n")
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
                AlertManager.shared.showAlert(groupLinkAlert(incognitoEnabled: incognitoDefault, connectionLink: r.string))
            } else {
                Task { connectViaLink(incognitoEnabled: incognitoDefault, connectionLink: r.string, dismiss) }
            }
        case let .failure(e):
            logger.error("ConnectContactView.processQRCode QR code error: \(e.localizedDescription)")
            dismiss()
        }
    }
}

struct IncognitoToggle: View {
    @Binding var incognitoEnabled: Bool
    @State private var incognitoToggleSheet: IncognitoToggleSheet?

    private enum IncognitoToggleSheet: Identifiable {
        case incognitoInfo

        var id: IncognitoToggleSheet { get { self } }
    }

    var body: some View {
        ZStack(alignment: .leading) {
            Image(systemName: incognitoEnabled ? "theatermasks.fill" : "theatermasks")
                .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                .foregroundColor(incognitoEnabled ? Color.indigo : .secondary)
            Toggle(isOn: $incognitoEnabled) {
                HStack(spacing: 6) {
                    Text("Incognito")
                    Image(systemName: "info.circle")
                        .foregroundColor(.accentColor)
                        .font(.system(size: 14))
                }
                .onTapGesture {
                    incognitoToggleSheet = .incognitoInfo
                }
            }
            .padding(.leading, 36)
        }
        .sheet(item: $incognitoToggleSheet) { sheet in
            switch sheet {
            case .incognitoInfo:
                IncognitoHelp()
            }
        }
    }
}

struct ConnectContactView_Previews: PreviewProvider {
    static var previews: some View {
        ScanToConnectView()
    }
}
