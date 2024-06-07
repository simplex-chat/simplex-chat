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
import AVFoundation

enum SomeAlert: Identifiable {
    case someAlert(alert: Alert, id: String)

    var id: String {
        switch self {
        case let .someAlert(_, id): return id
        }
    }
}

private enum NewChatViewAlert: Identifiable {
    case planAndConnectAlert(alert: PlanAndConnectAlert)
    case newChatSomeAlert(alert: SomeAlert)

    var id: String {
        switch self {
        case let .planAndConnectAlert(alert): return "planAndConnectAlert \(alert.id)"
        case let .newChatSomeAlert(alert): return "newChatSomeAlert \(alert.id)"
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
    @State var showQRCodeScanner = false
    @State private var invitationUsed: Bool = false
    @State private var contactConnection: PendingContactConnection? = nil
    @State private var connReqInvitation: String = ""
    @State private var creatingConnReq = false
    @State private var pastedLink: String = ""
    @State private var alert: NewChatViewAlert?

    var body: some View {
        VStack(alignment: .leading) {
            HStack {
                Text("New chat")
                    .font(.largeTitle)
                    .bold()
                    .fixedSize(horizontal: false, vertical: true)
                Spacer()
                InfoSheetButton {
                    AddContactLearnMore(showTitle: true)
                }
            }
            .padding()
            .padding(.top)

            Picker("New chat", selection: $selection) {
                Label("Add contact", systemImage: "link")
                    .tag(NewChatOption.invite)
                Label("Connect via link", systemImage: "qrcode")
                    .tag(NewChatOption.connect)
            }
            .pickerStyle(.segmented)
            .padding()

            VStack {
                // it seems there's a bug in iOS 15 if several views in switch (or if-else) statement have different transitions
                // https://developer.apple.com/forums/thread/714977?answerId=731615022#731615022
                if case .invite = selection {
                    prepareAndInviteView()
                        .transition(.move(edge: .leading))
                        .onAppear {
                            createInvitation()
                        }
                }
                if case .connect = selection {
                    ConnectView(showQRCodeScanner: $showQRCodeScanner, pastedLink: $pastedLink, alert: $alert)
                        .transition(.move(edge: .trailing))
                }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .background(
                // Rectangle is needed for swipe gesture to work on mostly empty views (creatingLinkProgressView and retryButton)
                Rectangle()
                    .fill(Color(uiColor: .systemGroupedBackground))
            )
            .animation(.easeInOut(duration: 0.3333), value: selection)
            .gesture(DragGesture(minimumDistance: 20.0, coordinateSpace: .local)
                .onChanged { value in
                    switch(value.translation.width, value.translation.height) {
                    case (...0, -30...30): // left swipe
                        if selection == .invite {
                            selection = .connect
                        }
                    case (0..., -30...30): // right swipe
                        if selection == .connect {
                            selection = .invite
                        }
                    default: ()
                    }
                }
            )
        }
        .background(Color(.systemGroupedBackground))
        .onChange(of: invitationUsed) { used in
            if used && !(m.showingInvitation?.connChatUsed ?? true) {
                m.markShowingInvitationUsed()
            }
        }
        .onDisappear {
            if !(m.showingInvitation?.connChatUsed ?? true),
               let conn = contactConnection {
                AlertManager.shared.showAlert(Alert(
                    title: Text("Keep unused invitation?"),
                    message: Text("You can view invitation link again in connection details."),
                    primaryButton: .default(Text("Keep")) {},
                    secondaryButton: .destructive(Text("Delete")) {
                        Task {
                            await deleteChat(Chat(
                                chatInfo: .contactConnection(contactConnection: conn),
                                chatItems: []
                            ))
                        }
                    }
                ))
            }
            m.showingInvitation = nil
        }
        .alert(item: $alert) { a in
            switch(a) {
            case let .planAndConnectAlert(alert):
                return planAndConnectAlert(alert, dismiss: true, cleanup: { pastedLink = "" })
            case let .newChatSomeAlert(.someAlert(alert, _)):
                return alert
            }
        }
    }

    private func prepareAndInviteView() -> some View {
        ZStack { // ZStack is needed for views to not make transitions between each other
            if connReqInvitation != "" {
                InviteView(
                    invitationUsed: $invitationUsed,
                    contactConnection: $contactConnection,
                    connReqInvitation: connReqInvitation
                )
            } else if creatingConnReq {
                creatingLinkProgressView()
            } else {
                retryButton()
            }
        }
    }

    private func createInvitation() {
        if connReqInvitation == "" && contactConnection == nil && !creatingConnReq {
            creatingConnReq = true
            Task {
                _ = try? await Task.sleep(nanoseconds: 250_000000)
                let (r, apiAlert) = await apiAddContact(incognito: incognitoGroupDefault.get())
                if let (connReq, pcc) = r {
                    await MainActor.run {
                        m.updateContactConnection(pcc)
                        m.showingInvitation = ShowingInvitation(connId: pcc.id, connChatUsed: false)
                        connReqInvitation = connReq
                        contactConnection = pcc
                    }
                } else {
                    await MainActor.run {
                        creatingConnReq = false
                        if let apiAlert = apiAlert {
                            alert = .newChatSomeAlert(alert: .someAlert(alert: apiAlert, id: "createInvitation error"))
                        }
                    }
                }
            }
        }
    }

    // Rectangle here and in retryButton are needed for gesture to work
    private func creatingLinkProgressView() -> some View {
        ProgressView("Creating link…")
            .progressViewStyle(.circular)
    }

    private func retryButton() -> some View {
        Button(action: createInvitation) {
            VStack(spacing: 6) {
                Image(systemName: "arrow.counterclockwise")
                Text("Retry")
            }
        }
    }
}

private struct InviteView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var invitationUsed: Bool
    @Binding var contactConnection: PendingContactConnection?
    var connReqInvitation: String
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        List {
            Section("Share this 1-time invite link") {
                shareLinkView()
            }
            .listRowInsets(EdgeInsets(top: 0, leading: 20, bottom: 0, trailing: 10))

            qrCodeView()

            Section {
                IncognitoToggle(incognitoEnabled: $incognitoDefault)
            } footer: {
                sharedProfileInfo(incognitoDefault)
            }
        }
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
            setInvitationUsed()
        }
    }

    private func shareLinkView() -> some View {
        HStack {
            let link = simplexChatLink(connReqInvitation)
            linkTextView(link)
            Button {
                showShareSheet(items: [link])
                setInvitationUsed()
            } label: {
                Image(systemName: "square.and.arrow.up")
                    .padding(.top, -7)
            }
        }
        .frame(maxWidth: .infinity)
    }

    private func qrCodeView() -> some View {
        Section("Or show this code") {
            SimpleXLinkQRCode(uri: connReqInvitation, onShare: setInvitationUsed)
                .padding()
                .background(
                    RoundedRectangle(cornerRadius: 12, style: .continuous)
                        .fill(Color(uiColor: .secondarySystemGroupedBackground))
                )
                .padding(.horizontal)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
        }
    }

    private func setInvitationUsed() {
        if !invitationUsed {
            invitationUsed = true
        }
    }
}

private struct ConnectView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var showQRCodeScanner: Bool
    @Binding var pastedLink: String
    @Binding var alert: NewChatViewAlert?
    @State private var sheet: PlanAndConnectActionSheet?

    var body: some View {
        List {
            Section("Paste the link you received") {
                pasteLinkView()
            }
            Section("Or scan QR code") {
                ScannerInView(showQRCodeScanner: $showQRCodeScanner, processQRCode: processQRCode)
            }
        }
        .actionSheet(item: $sheet) { s in
            planAndConnectActionSheet(s, dismiss: true, cleanup: { pastedLink = "" })
        }
    }

    @ViewBuilder private func pasteLinkView() -> some View {
        if pastedLink == "" {
            Button {
                if let str = UIPasteboard.general.string {
                    if let link = strHasSingleSimplexLink(str.trimmingCharacters(in: .whitespaces)) {
                        pastedLink = link.text
                        // It would be good to hide it, but right now it is not clear how to release camera in CodeScanner
                        // https://github.com/twostraws/CodeScanner/issues/121
                        // No known tricks worked (changing view ID, wrapping it in another view, etc.)
                        // showQRCodeScanner = false
                        connect(pastedLink)
                    } else {
                        alert = .newChatSomeAlert(alert: .someAlert(
                            alert: mkAlert(title: "Invalid link", message: "The text you pasted is not a SimpleX link."),
                            id: "pasteLinkView: code is not a SimpleX link"
                        ))
                    }
                }
            } label: {
                Text("Tap to paste link")
            }
            .disabled(!ChatModel.shared.pasteboardHasStrings)
            .frame(maxWidth: .infinity, alignment: .center)
        } else {
            linkTextView(pastedLink)
        }
    }

    private func processQRCode(_ resp: Result<ScanResult, ScanError>) {
        switch resp {
        case let .success(r):
            let link = r.string
            if strIsSimplexLink(r.string) {
                connect(link)
            } else {
                alert = .newChatSomeAlert(alert: .someAlert(
                    alert: mkAlert(title: "Invalid QR code", message: "The code you scanned is not a SimpleX link QR code."),
                    id: "processQRCode: code is not a SimpleX link"
                ))
            }
        case let .failure(e):
            logger.error("processQRCode QR code error: \(e.localizedDescription)")
            alert = .newChatSomeAlert(alert: .someAlert(
                alert: mkAlert(title: "Invalid QR code", message: "Error scanning code: \(e.localizedDescription)"),
                id: "processQRCode: failure"
            ))
        }
    }

    private func connect(_ link: String) {
        planAndConnect(
            link,
            showAlert: { alert = .planAndConnectAlert(alert: $0) },
            showActionSheet: { sheet = $0 },
            dismiss: true,
            incognito: nil
        )
    }
}

private func linkTextView(_ link: String) -> some View {
    Text(link)
        .lineLimit(1)
        .font(.caption)
        .truncationMode(.middle)
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
                .frame(width: 24, height: 24)
        }
        .sheet(isPresented: $showInfoSheet) {
            content
        }
    }
}

func strIsSimplexLink(_ str: String) -> Bool {
    if let parsedMd = parseSimpleXMarkdown(str),
       parsedMd.count == 1,
       case .simplexLink = parsedMd[0].format {
        return true
    } else {
        return false
    }
}

func strHasSingleSimplexLink(_ str: String) -> FormattedText? {
    if let parsedMd = parseSimpleXMarkdown(str) {
       let parsedLinks = parsedMd.filter({ $0.format?.isSimplexLink ?? false })
        if parsedLinks.count == 1 {
            return parsedLinks[0]
        } else {
            return nil
        }
    } else {
        return nil
    }
}

struct IncognitoToggle: View {
    @Binding var incognitoEnabled: Bool
    @State private var showIncognitoSheet = false

    var body: some View {
        ZStack(alignment: .leading) {
            Image(systemName: incognitoEnabled ? "theatermasks.fill" : "theatermasks")
                .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                .foregroundColor(incognitoEnabled ? Color.indigo : .secondary)
                .font(.system(size: 14))
            Toggle(isOn: $incognitoEnabled) {
                HStack(spacing: 6) {
                    Text("Incognito")
                    Image(systemName: "info.circle")
                        .foregroundColor(.accentColor)
                        .font(.system(size: 14))
                }
                .onTapGesture {
                    showIncognitoSheet = true
                }
            }
            .padding(.leading, 36)
        }
        .sheet(isPresented: $showIncognitoSheet) {
            IncognitoHelp()
        }
    }
}

func sharedProfileInfo(_ incognito: Bool) -> Text {
    let name = ChatModel.shared.currentUser?.displayName ?? ""
    return Text(
        incognito
        ? "A new random profile will be shared."
        : "Your profile **\(name)** will be shared."
    )
}

enum PlanAndConnectAlert: Identifiable {
    case ownInvitationLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case invitationLinkConnecting(connectionLink: String)
    case ownContactAddressConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case contactAddressConnectingConfirmReconnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConnectingConfirmReconnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConnecting(connectionLink: String, groupInfo: GroupInfo?)

    var id: String {
        switch self {
        case let .ownInvitationLinkConfirmConnect(connectionLink, _, _): return "ownInvitationLinkConfirmConnect \(connectionLink)"
        case let .invitationLinkConnecting(connectionLink): return "invitationLinkConnecting \(connectionLink)"
        case let .ownContactAddressConfirmConnect(connectionLink, _, _): return "ownContactAddressConfirmConnect \(connectionLink)"
        case let .contactAddressConnectingConfirmReconnect(connectionLink, _, _): return "contactAddressConnectingConfirmReconnect \(connectionLink)"
        case let .groupLinkConfirmConnect(connectionLink, _, _): return "groupLinkConfirmConnect \(connectionLink)"
        case let .groupLinkConnectingConfirmReconnect(connectionLink, _, _): return "groupLinkConnectingConfirmReconnect \(connectionLink)"
        case let .groupLinkConnecting(connectionLink, _): return "groupLinkConnecting \(connectionLink)"
        }
    }
}

func planAndConnectAlert(_ alert: PlanAndConnectAlert, dismiss: Bool, cleanup: (() -> Void)? = nil) -> Alert {
    switch alert {
    case let .ownInvitationLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Connect to yourself?"),
            message: Text("This is your own one-time link!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case .invitationLinkConnecting:
        return Alert(
            title: Text("Already connecting!"),
            message: Text("You are already connecting via this one-time link!"),
            dismissButton: .default(Text("OK")) { cleanup?() }
        )
    case let .ownContactAddressConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Connect to yourself?"),
            message: Text("This is your own SimpleX address!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .contactAddressConnectingConfirmReconnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Repeat connection request?"),
            message: Text("You have already requested connection via this address!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .groupLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Join group?"),
            message: Text("You will connect to all group members."),
            primaryButton: .default(
                Text(incognito ? "Join incognito" : "Join"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .groupLinkConnectingConfirmReconnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Repeat join request?"),
            message: Text("You are already joining the group via this link!"),
            primaryButton: .destructive(
                Text(incognito ? "Join incognito" : "Join"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) }
            ),
            secondaryButton: .cancel() { cleanup?() }
        )
    case let .groupLinkConnecting(_, groupInfo):
        if let groupInfo = groupInfo {
            return Alert(
                title: Text("Group already exists!"),
                message: Text("You are already joining the group \(groupInfo.displayName)."),
                dismissButton: .default(Text("OK")) { cleanup?() }
            )
        } else {
            return Alert(
                title: Text("Already joining the group!"),
                message: Text("You are already joining the group via this link."),
                dismissButton: .default(Text("OK")) { cleanup?() }
            )
        }
    }
}

enum PlanAndConnectActionSheet: Identifiable {
    case askCurrentOrIncognitoProfile(connectionLink: String, connectionPlan: ConnectionPlan?, title: LocalizedStringKey)
    case askCurrentOrIncognitoProfileDestructive(connectionLink: String, connectionPlan: ConnectionPlan, title: LocalizedStringKey)
    case askCurrentOrIncognitoProfileConnectContactViaAddress(contact: Contact)
    case ownGroupLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool?, groupInfo: GroupInfo)

    var id: String {
        switch self {
        case let .askCurrentOrIncognitoProfile(connectionLink, _, _): return "askCurrentOrIncognitoProfile \(connectionLink)"
        case let .askCurrentOrIncognitoProfileDestructive(connectionLink, _, _): return "askCurrentOrIncognitoProfileDestructive \(connectionLink)"
        case let .askCurrentOrIncognitoProfileConnectContactViaAddress(contact): return "askCurrentOrIncognitoProfileConnectContactViaAddress \(contact.contactId)"
        case let .ownGroupLinkConfirmConnect(connectionLink, _, _, _): return "ownGroupLinkConfirmConnect \(connectionLink)"
        }
    }
}

func planAndConnectActionSheet(_ sheet: PlanAndConnectActionSheet, dismiss: Bool, cleanup: (() -> Void)? = nil) -> ActionSheet {
    switch sheet {
    case let .askCurrentOrIncognitoProfile(connectionLink, connectionPlan, title):
        return ActionSheet(
            title: Text(title),
            buttons: [
                .default(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                .default(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                .cancel() { cleanup?() }
            ]
        )
    case let .askCurrentOrIncognitoProfileDestructive(connectionLink, connectionPlan, title):
        return ActionSheet(
            title: Text(title),
            buttons: [
                .destructive(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                .destructive(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                .cancel() { cleanup?() }
            ]
        )
    case let .askCurrentOrIncognitoProfileConnectContactViaAddress(contact):
        return ActionSheet(
            title: Text("Connect with \(contact.chatViewName)"),
            buttons: [
                .default(Text("Use current profile")) { connectContactViaAddress_(contact, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                .default(Text("Use new incognito profile")) { connectContactViaAddress_(contact, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                .cancel() { cleanup?() }
            ]
        )
    case let .ownGroupLinkConfirmConnect(connectionLink, connectionPlan, incognito, groupInfo):
        if let incognito = incognito {
            return ActionSheet(
                title: Text("Join your group?\nThis is your link for group \(groupInfo.displayName)!"),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss, showAlreadyExistsAlert: nil) },
                    .destructive(Text(incognito ? "Join incognito" : "Join with current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup) },
                    .cancel() { cleanup?() }
                ]
            )
        } else {
            return ActionSheet(
                title: Text("Join your group?\nThis is your link for group \(groupInfo.displayName)!"),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss, showAlreadyExistsAlert: nil) },
                    .destructive(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false, cleanup: cleanup) },
                    .destructive(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true, cleanup: cleanup) },
                    .cancel() { cleanup?() }
                ]
            )
        }
    }
}

func planAndConnect(
    _ connectionLink: String,
    showAlert: @escaping (PlanAndConnectAlert) -> Void,
    showActionSheet: @escaping (PlanAndConnectActionSheet) -> Void,
    dismiss: Bool,
    incognito: Bool?,
    cleanup: (() -> Void)? = nil,
    filterKnownContact: ((Contact) -> Void)? = nil,
    filterKnownGroup: ((GroupInfo) -> Void)? = nil
) {
    Task {
        do {
            let connectionPlan = try await apiConnectPlan(connReq: connectionLink)
            switch connectionPlan {
            case let .invitationLink(ilp):
                switch ilp {
                case .ok:
                    logger.debug("planAndConnect, .invitationLink, .ok, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect via one-time link"))
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .invitationLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.ownInvitationLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect to yourself?\nThis is your own one-time link!"))
                    }
                case let .connecting(contact_):
                    logger.debug("planAndConnect, .invitationLink, .connecting, incognito=\(incognito?.description ?? "nil")")
                    if let contact = contact_ {
                        if let f = filterKnownContact {
                            f(contact)
                        } else {
                            openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyConnectingAlert(contact)) }
                        }
                    } else {
                        showAlert(.invitationLinkConnecting(connectionLink: connectionLink))
                    }
                case let .known(contact):
                    logger.debug("planAndConnect, .invitationLink, .known, incognito=\(incognito?.description ?? "nil")")
                    if let f = filterKnownContact {
                        f(contact)
                    } else {
                        openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyExistsAlert(contact)) }
                    }
                }
            case let .contactAddress(cap):
                switch cap {
                case .ok:
                    logger.debug("planAndConnect, .contactAddress, .ok, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect via contact address"))
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .contactAddress, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.ownContactAddressConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect to yourself?\nThis is your own SimpleX address!"))
                    }
                case .connectingConfirmReconnect:
                    logger.debug("planAndConnect, .contactAddress, .connectingConfirmReconnect, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.contactAddressConnectingConfirmReconnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "You have already requested connection!\nRepeat connection request?"))
                    }
                case let .connectingProhibit(contact):
                    logger.debug("planAndConnect, .contactAddress, .connectingProhibit, incognito=\(incognito?.description ?? "nil")")
                    if let f = filterKnownContact {
                        f(contact)
                    } else {
                        openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyConnectingAlert(contact)) }
                    }
                case let .known(contact):
                    logger.debug("planAndConnect, .contactAddress, .known, incognito=\(incognito?.description ?? "nil")")
                    if let f = filterKnownContact {
                        f(contact)
                    } else {
                        openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyExistsAlert(contact)) }
                    }
                case let .contactViaAddress(contact):
                    logger.debug("planAndConnect, .contactAddress, .contactViaAddress, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectContactViaAddress_(contact, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileConnectContactViaAddress(contact: contact))
                    }
                }
            case let .groupLink(glp):
                switch glp {
                case .ok:
                    if let incognito = incognito {
                        showAlert(.groupLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Join group"))
                    }
                case let .ownLink(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    if let f = filterKnownGroup {
                        f(groupInfo)
                    }
                    showActionSheet(.ownGroupLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito, groupInfo: groupInfo))
                case .connectingConfirmReconnect:
                    logger.debug("planAndConnect, .groupLink, .connectingConfirmReconnect, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.groupLinkConnectingConfirmReconnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "You are already joining the group!\nRepeat join request?"))
                    }
                case let .connectingProhibit(groupInfo_):
                    logger.debug("planAndConnect, .groupLink, .connectingProhibit, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.groupLinkConnecting(connectionLink: connectionLink, groupInfo: groupInfo_))
                case let .known(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .known, incognito=\(incognito?.description ?? "nil")")
                    if let f = filterKnownGroup {
                        f(groupInfo)
                    } else {
                        openKnownGroup(groupInfo, dismiss: dismiss) { AlertManager.shared.showAlert(groupAlreadyExistsAlert(groupInfo)) }
                    }
                }
            }
        } catch {
            logger.debug("planAndConnect, plan error")
            if let incognito = incognito {
                connectViaLink(connectionLink, connectionPlan: nil, dismiss: dismiss, incognito: incognito, cleanup: cleanup)
            } else {
                showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: nil, title: "Connect via link"))
            }
        }
    }
}

private func connectContactViaAddress_(_ contact: Contact, dismiss: Bool, incognito: Bool, cleanup: (() -> Void)? = nil) {
    Task {
        if dismiss {
            DispatchQueue.main.async {
                dismissAllSheets(animated: true)
            }
        }
        _ = await connectContactViaAddress(contact.contactId, incognito)
        cleanup?()
    }
}

private func connectViaLink(
    _ connectionLink: String,
    connectionPlan: ConnectionPlan?,
    dismiss: Bool,
    incognito: Bool,
    cleanup: (() -> Void)?
) {
    Task {
        if let (connReqType, pcc) = await apiConnect(incognito: incognito, connReq: connectionLink) {
            await MainActor.run {
                ChatModel.shared.updateContactConnection(pcc)
            }
            let crt: ConnReqType
            if let plan = connectionPlan {
                crt = planToConnReqType(plan)
            } else {
                crt = connReqType
            }
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        AlertManager.shared.showAlert(connReqSentAlert(crt))
                    }
                } else {
                    AlertManager.shared.showAlert(connReqSentAlert(crt))
                }
            }
        } else {
            if dismiss {
                DispatchQueue.main.async {
                    dismissAllSheets(animated: true)
                }
            }
        }
        cleanup?()
    }
}

func openKnownContact(_ contact: Contact, dismiss: Bool, showAlreadyExistsAlert: (() -> Void)?) {
    Task {
        let m = ChatModel.shared
        if let c = m.getContactChat(contact.contactId) {
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        m.chatId = c.id
                        showAlreadyExistsAlert?()
                    }
                } else {
                    m.chatId = c.id
                    showAlreadyExistsAlert?()
                }
            }
        }
    }
}

func openKnownGroup(_ groupInfo: GroupInfo, dismiss: Bool, showAlreadyExistsAlert: (() -> Void)?) {
    Task {
        let m = ChatModel.shared
        if let g = m.getGroupChat(groupInfo.groupId) {
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        m.chatId = g.id
                        showAlreadyExistsAlert?()
                    }
                } else {
                    m.chatId = g.id
                    showAlreadyExistsAlert?()
                }
            }
        }
    }
}

func contactAlreadyConnectingAlert(_ contact: Contact) -> Alert {
    mkAlert(
        title: "Contact already exists",
        message: "You are already connecting to \(contact.displayName)."
    )
}

func groupAlreadyExistsAlert(_ groupInfo: GroupInfo) -> Alert {
    mkAlert(
        title: "Group already exists",
        message: "You are already in group \(groupInfo.displayName)."
    )
}

enum ConnReqType: Equatable {
    case invitation
    case contact
    case groupLink

    var connReqSentText: LocalizedStringKey {
        switch self {
        case .invitation: return "You will be connected when your contact's device is online, please wait or check later!"
        case .contact: return "You will be connected when your connection request is accepted, please wait or check later!"
        case .groupLink: return "You will be connected when group link host's device is online, please wait or check later!"
        }
    }
}

private func planToConnReqType(_ connectionPlan: ConnectionPlan) -> ConnReqType {
    switch connectionPlan {
    case .invitationLink: return .invitation
    case .contactAddress: return .contact
    case .groupLink: return .groupLink
    }
}

func connReqSentAlert(_ type: ConnReqType) -> Alert {
    return mkAlert(
        title: "Connection request sent!",
        message: type.connReqSentText
    )
}

#Preview {
    NewChatView(
        selection: .invite
    )
}
