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

enum NewChatOption: Identifiable {
    case invite
    case connect

    var id: Self { self }
}

struct NewChatView: View {
    @EnvironmentObject var m: ChatModel
    @State var selection: NewChatOption
    @State var connReqInvitation: String
    @State var contactConnection: PendingContactConnection?
    @State private var creatingConnReq = false

    var body: some View {
        NavigationView {
            VStack(alignment: .leading) {
                Text("Start a New Chat")
                    .font(.largeTitle)
                    .bold()
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.vertical)

                Picker("New chat", selection: $selection) {
                    Label("Invite", systemImage: "link")
                        .tag(NewChatOption.invite)
                    Label("Connect", systemImage: "qrcode")
                        .tag(NewChatOption.connect)
                }
                .pickerStyle(.segmented)

                switch selection {
                case .invite: InviteView(
                    contactConnection: $contactConnection,
                    connReqInvitation: connReqInvitation
                )
                case .connect: ConnectView()
                }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
            .padding()
            .background(Color(.systemGroupedBackground))
            .onChange(of: selection) { sel in
                if case .connect = sel,
                   connReqInvitation == "" && contactConnection == nil && !creatingConnReq {
                    createInvitation()
                }
            }
            .onAppear { m.connReqInv = connReqInvitation }
            .onDisappear { m.connReqInv = nil }
        }
    }

    private func createInvitation() {
        creatingConnReq = true
        Task {
            if let (connReq, pcc) = await apiAddContact(incognito: incognitoGroupDefault.get()) {
                await MainActor.run {
                    connReqInvitation = connReq
                    contactConnection = pcc
                    m.connReqInv = connReq
                }
            } else {
                await MainActor.run {
                    creatingConnReq = false
                }
            }
        }
    }
}

struct InviteView: View {
    @EnvironmentObject private var chatModel: ChatModel
    @Binding var contactConnection: PendingContactConnection?
    var connReqInvitation: String
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        GeometryReader { geo in
            ScrollView {
                VStack(alignment: .leading, spacing: 8) {
                    if connReqInvitation != "" {
                        HStack {
                            Text("Share this unique invite link")
                                .textCase(.uppercase)
                                .font(.footnote)
                                .foregroundColor(.secondary)
                            Spacer()
                            Button {
                                copyLink()
                            } label: {
                                Text("Copy")
                                    .font(.footnote)
                            }
                        }
                        .padding(.horizontal)

                        Text(simplexChatLink(connReqInvitation))
                            .lineLimit(2)
                            .font(.callout)
                            .padding(.horizontal)
                            .padding(.vertical, 8)
                            .background(
                                RoundedRectangle(cornerRadius: 12, style: .continuous)
                                    .fill(Color(uiColor: .systemBackground))
                            )

                        Text("Or show this code")
                            .textCase(.uppercase)
                            .font(.footnote)
                            .foregroundColor(.secondary)
                            .padding(.horizontal)
                            .padding(.top, 8)

                        VStack(alignment: .center) {
                            SimpleXLinkQRCode(uri: connReqInvitation)
                                .padding(.horizontal)
                                .padding(.vertical, 10)
                                .frame(width: geo.size.width * 0.8)
                        }
                        .frame(maxWidth: .infinity, alignment: .center)
                        .background(
                            RoundedRectangle(cornerRadius: 12, style: .continuous)
                                .fill(Color(uiColor: .systemBackground))
                        )

                        VStack(alignment: .leading) {
                            IncognitoToggle(incognitoEnabled: $incognitoDefault)
                            Divider()
                            shareLinkButton2(connReqInvitation)
                        }
                        .padding(.horizontal)
                        .padding(.vertical, 8)
                        .frame(maxWidth: .infinity)
                        .background(
                            RoundedRectangle(cornerRadius: 12, style: .continuous)
                                .fill(Color(uiColor: .systemBackground))
                        )
                        .padding(.top)

                        VStack(alignment: .center) {
                            oneTimeLinkLearnMoreButton2()
                        }
                        .frame(maxWidth: .infinity, alignment: .center)
                        .padding(.top)
                    } else {
                        Text("Creating link…")
                            .textCase(.uppercase)
                            .font(.footnote)
                            .foregroundColor(.secondary)
                            .padding(.horizontal)

                        VStack(alignment: .center) {
                            ProgressView()
                                .progressViewStyle(.circular)
                                .scaleEffect(2)
                                .frame(maxWidth: .infinity)
                                .padding(.horizontal)
                                .padding(.vertical, 8)
                        }
                        .frame(maxWidth: .infinity, alignment: .center)
                        .background(
                            RoundedRectangle(cornerRadius: 12, style: .continuous)
                                .fill(Color(uiColor: .systemBackground))
                        )
                    }
                }
            }
            .padding(.vertical)
            .onAppear { chatModel.connReqInv = connReqInvitation }
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
    }

    private func copyLink() {
        UIPasteboard.general.string = simplexChatLink(connReqInvitation)
    }
}

struct ConnectView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var connectionLink: String = ""
    @State private var alert: PlanAndConnectAlert?
    @State private var sheet: PlanAndConnectActionSheet?
    @State private var showScanQRCodeSheet = false
    @State private var scannedLink: String = ""

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 8) {
                (
                    Text("Paste the URL your received from your contact.")
                    + Text("\n\n")
                    + Text("You'll be connected to begin a private conversation with them.")
                )
                .font(.footnote)
                .foregroundColor(.secondary)
                .padding(.horizontal)

                HStack {
                    Text("Unique invite link")
                        .textCase(.uppercase)
                        .font(.footnote)
                        .foregroundColor(.secondary)
                    Spacer()
                    if (connectionLink != "") {
                        Button {
                            clearLink()
                        } label: {
                            Text("Clear")
                                .font(.footnote)
                        }
                    }
                }
                .padding(.horizontal)
                .padding(.top)

                pasteLinkView()

                Text("Or scan QR code")
                    .textCase(.uppercase)
                    .font(.footnote)
                    .foregroundColor(.secondary)
                    .padding(.horizontal)
                    .padding(.top, 8)

                VStack(alignment: .center) {
                    scanQRCodeButton()
                }
                .padding(.horizontal)
                .padding(.vertical, 8)
                .frame(maxWidth: .infinity, alignment: .center)
                .background(
                    RoundedRectangle(cornerRadius: 12, style: .continuous)
                        .fill(Color(uiColor: .systemBackground))
                )

                VStack(alignment: .center) {
                    oneTimeLinkLearnMoreButton2()
                }
                .frame(maxWidth: .infinity, alignment: .center)
                .padding(.top)
            }
        }
        .padding(.vertical)
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

    private func clearLink() {
        connectionLink = ""
    }

    @ViewBuilder private func pasteLinkView() -> some View {
        if connectionLink == "" {
            VStack(alignment: .center) {
                ZStack {
                    Text("\n")
                        .font(.callout)
                        .padding(.horizontal)
                        .padding(.vertical, 8)
                        .opacity(0)
                    Button {
                        if let link = UIPasteboard.general.string {
                            // TODO test pasted text is a link, alert if not
                            connectionLink = link.trimmingCharacters(in: .whitespaces)
                            connect(connectionLink)
                        }
                    } label: {
                        Text("Click Here to Paste Link")
                            .foregroundColor(.accentColor)
                            .padding(.horizontal)
                            .padding(.vertical, 8)
                    }
                }
            }
            .frame(maxWidth: .infinity, alignment: .center)
            .background(
                RoundedRectangle(cornerRadius: 12, style: .continuous)
                    .fill(Color(uiColor: .systemBackground))
            )
        } else {
            VStack() {
                ZStack {
                    Text("\n")
                        .font(.callout)
                        .padding(.horizontal)
                        .padding(.vertical, 8)
                        .opacity(0)
                    Text(connectionLink)
                        .lineLimit(2)
                        .font(.callout)
                        .padding(.horizontal)
                        .padding(.vertical, 8)
                }
            }
            .frame(maxWidth: .infinity)
            .background(
                RoundedRectangle(cornerRadius: 12, style: .continuous)
                    .fill(Color(uiColor: .systemBackground))
            )
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

struct ScanConnectionCodeView: View {
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

// TODO move IncognitoToggle here

// TODO move shareLinkButton here
func shareLinkButton2(_ connReqInvitation: String) -> some View {
    Button {
        showShareSheet(items: [simplexChatLink(connReqInvitation)])
    } label: {
        settingsRow("square.and.arrow.up") {
            Text("Share link")
        }
    }
}

// TODO move oneTimeLinkLearnMoreButton here
func oneTimeLinkLearnMoreButton2() -> some View {
    NavigationLink {
        AddContactLearnMore()
            .navigationTitle("One-time invitation link")
            .navigationBarTitleDisplayMode(.large)
    } label: {
        settingsRow("info.circle") {
            Text("Need Guidance?")
                .underline()
        }
    }
}

// TODO move planAndConnect here

//#Preview {
//    NewChatView()
//}
