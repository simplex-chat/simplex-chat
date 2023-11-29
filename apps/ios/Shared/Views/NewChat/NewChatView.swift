//
//  NewChatView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import CodeScanner

enum SomeAlert: Identifiable {
    case someAlert(alert: Alert, id: String)

    var id: String {
        switch self {
        case let .someAlert(_, id): return id
        }
    }
}

enum NewChatOption: Identifiable {
    case invite
    case connect

    var id: Self { self }
}

struct NewChatView: View {
    @EnvironmentObject var m: ChatModel
    @State var selection: NewChatOption
    @State var showScanQRCodeSheet = false
    @State private var connReqInvitation: String = ""
    @State private var contactConnection: PendingContactConnection? = nil
    @State private var creatingConnReq = false
    @State private var someAlert: SomeAlert?

    var body: some View {
        List {
            Group {
                HStack {
                    Text("New chat")
                        .font(.largeTitle)
                        .bold()
                        .fixedSize(horizontal: false, vertical: true)
                        .padding(.vertical)
                    Spacer()
                    InfoSheetButton {
                        AddContactLearnMore(showTitle: true)
                    }
                }

                Picker("New chat", selection: $selection) {
                    Label("Share link", systemImage: "link")
                        .tag(NewChatOption.invite)
                    Label("Connect via link", systemImage: "qrcode")
                        .tag(NewChatOption.connect)
                }
                .pickerStyle(.segmented)
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

            switch selection {
            case .invite:
                if connReqInvitation != "" {
                    InviteView(contactConnection: $contactConnection, connReqInvitation: connReqInvitation)
                } else if creatingConnReq {
                    creatingLinkProgressView()
                } else {
                    retryButton()
                }
            case .connect:
                ConnectView(showScanQRCodeSheet: showScanQRCodeSheet)
            }
        }
        .onChange(of: selection) { sel in
            createInvitation(sel)
        }
        .onAppear {
            createInvitation(selection)
        }
        .onDisappear { m.connReqInv = nil }
        .alert(item: $someAlert) { a in
            switch a {
            case let .someAlert(alert, _): alert
            }
        }
    }

    private func createInvitation(_ selection: NewChatOption) {
        if case .invite = selection,
           connReqInvitation == "" && contactConnection == nil && !creatingConnReq {
            creatingConnReq = true
            Task {
                let (r, alert) = await apiAddContact(incognito: incognitoGroupDefault.get())
                if let (connReq, pcc) = r {
                    await MainActor.run {
                        connReqInvitation = connReq
                        contactConnection = pcc
                        m.connReqInv = connReq
                    }
                } else {
                    await MainActor.run {
                        creatingConnReq = false
                        if let alert = alert {
                            someAlert = .someAlert(alert: alert, id: "createInvitation error")
                        }
                    }
                }
            }
        }
    }

    private func creatingLinkProgressView() -> some View {
        ProgressView("Creating link…")
            .progressViewStyle(.circular)
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .center)
            .padding(.top)
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
    }

    private func retryButton() -> some View {
        Button {
            createInvitation(selection)
        } label: {
            Text("Retry")
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .center)
        .padding(.top)
        .listRowBackground(Color.clear)
        .listRowSeparator(.hidden)
    }
}

private struct InviteView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var contactConnection: PendingContactConnection?
    var connReqInvitation: String
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        viewBody()
            .onChange(of: incognitoDefault) { incognito in
                Task {
                    do {
                        if let contactConn = contactConnection,
                           let conn = try await apiSetConnectionIncognito(connId: contactConn.pccConnId, incognito: incognito) {
                            await MainActor.run {
                                contactConnection = conn
                                chatModel.updateContactConnection(conn)
                            }
                        }
                    } catch {
                        logger.error("apiSetConnectionIncognito error: \(responseError(error))")
                    }
                }
            }
    }

    @ViewBuilder private func viewBody() -> some View {
        Section("Share this 1-time invite link") {
            shareLinkView()
        }

        qrCodeView()

        Section {
            IncognitoToggle(incognitoEnabled: $incognitoDefault)
        } footer: {
            sharedProfileInfo(incognitoDefault)
        }
    }

    private func shareLinkView() -> some View {
        HStack {
            let link = simplexChatLink(connReqInvitation)
            Text(link)
                .lineLimit(1)
                .font(.caption)
                .truncationMode(.middle)
            Button {
                showShareSheet(items: [link])
            } label: {
                Image(systemName: "square.and.arrow.up")
            }
        }
    }

    private func qrCodeView() -> some View {
        Section("Or show this code") {
            SimpleXLinkQRCode(uri: connReqInvitation)
                .padding()
                .background(
                    RoundedRectangle(cornerRadius: 12, style: .continuous)
                        .fill(Color(uiColor: .systemBackground))
                )
                .padding(.horizontal)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
        }
    }
}

private struct ConnectView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @State var showScanQRCodeSheet = false
    @State private var connectionLink: String = ""
    @State private var alert: PlanAndConnectAlert?
    @State private var sheet: PlanAndConnectActionSheet?
    @State private var scannedLink: String = ""

    var body: some View {
        viewBody()
            .alert(item: $alert) { a in planAndConnectAlert(a, dismiss: true) }
            .actionSheet(item: $sheet) { s in planAndConnectActionSheet(s, dismiss: true) }
            .sheet(isPresented: $showScanQRCodeSheet) {
                if #available(iOS 16.0, *) {
                    ScanConnectionCodeView(scannedLink: $scannedLink)
                        .presentationDetents([.fraction(0.8)])
                } else {
                    ScanConnectionCodeView(scannedLink: $scannedLink)
                }
            }
            .onChange(of: scannedLink) { link in
                connect(link)
            }
    }

    @ViewBuilder private func viewBody() -> some View {
        Section {
            // TODO clear link button
            pasteLinkView()
        } header: {
            Text("Paste the link you received")
        }

        Section {
            scanQRCodeButton()
        } header: {
            Text("Or scan QR code")
        }
    }

    @ViewBuilder private func pasteLinkView() -> some View {
        if connectionLink == "" {
            Button {
                if let link = UIPasteboard.general.string {
                    // TODO test pasted text is a link, alert if not
                    connectionLink = link.trimmingCharacters(in: .whitespaces)
                    connect(connectionLink)
                }
            } label: {
                settingsRow("doc.plaintext") {
                    Text("Tap to paste link")
                }
            }
        } else {
            Text(connectionLink)
                .lineLimit(1)
                .font(.caption)
                .truncationMode(.middle)
        }
    }

    private func connect(_ link: String) {
        planAndConnect(
            link,
            showAlert: { alert = $0 },
            showActionSheet: { sheet = $0 },
            dismiss: true,
            incognito: nil
        )
    }

    private func scanQRCodeButton() -> some View {
        Button {
            showScanQRCodeSheet = true
        } label: {
            settingsRow("qrcode") {
                Text("Scan code")
            }
        }
    }
}

private struct ScanConnectionCodeView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var scannedLink: String

    var body: some View {
        VStack(alignment: .leading) {
            Text("Scan QR code")
                .font(.largeTitle)
                .bold()
                .fixedSize(horizontal: false, vertical: true)
                .padding(.vertical)
            
            CodeScannerView(codeTypes: [.qr], completion: processQRCode)
                .aspectRatio(1, contentMode: .fit)
                .cornerRadius(12)
                .padding(.top)

            Text("If you cannot meet in person, you can **scan QR code in the video call**, or your contact can share an invitation link.")
                .padding(.top)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .padding()
    }

    private func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            scannedLink = r.string
            dismiss()
        case let .failure(e):
            logger.error("ConnectContactView.processQRCode QR code error: \(e.localizedDescription)")
            // TODO alert
            dismiss()
        }
    }
}

struct InfoSheetButton<Content: View>: View {
    @ViewBuilder let content: Content
    @State private var showInfoSheet = false

    var body: some View {
        Button {
            showInfoSheet = true
        } label: {
            Image(systemName: "info.circle")
                .resizable()
                .scaledToFit()
                .frame(width: 20, height: 20)
        }
        .sheet(isPresented: $showInfoSheet) {
            content
        }
    }
}

// TODO move IncognitoToggle here

// TODO move shareLinkButton to connection details

// TODO move planAndConnect here

// TODO delete NewChatButton, CreateLinkView, ScanToConnectView, PasteToConnectView, AddContactView

//#Preview {
//    NewChatView()
//}
