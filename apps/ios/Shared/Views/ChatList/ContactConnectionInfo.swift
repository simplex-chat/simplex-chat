//
//  ContactConnectionInfo.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 06/10/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactConnectionInfo: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @State var contactConnection: PendingContactConnection
    @State private var alert: CCInfoAlert?
    @State private var localAlias = ""
    @FocusState private var aliasTextFieldFocused: Bool

    enum CCInfoAlert: Identifiable {
        case deleteInvitationAlert
        case error(title: LocalizedStringKey, error: LocalizedStringKey)

        var id: String {
            switch self {
            case .deleteInvitationAlert: return "deleteInvitationAlert"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        NavigationView {
            List {
                Group {
                    Text(contactConnection.initiated ? "You invited your contact" : "You accepted connection")
                        .font(.largeTitle)
                        .bold()
                        .padding(.bottom, 16)

                    Text(contactConnectionText(contactConnection))
                }
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .onTapGesture { aliasTextFieldFocused = false }

                Section {
                    if contactConnection.groupLinkId == nil {
                        HStack(spacing: 20) {
                            Image(systemName: "pencil")
                                .foregroundColor(.secondary)
                                .padding(.leading, 6)
                                .onTapGesture { aliasTextFieldFocused = true }
                            TextField("Set contact name…", text: $localAlias)
                                .autocapitalization(.none)
                                .autocorrectionDisabled(true)
                                .focused($aliasTextFieldFocused)
                                .submitLabel(.done)
                                .onSubmit(setConnectionAlias)
                        }
                    }

                    if contactConnection.initiated,
                       let connReqInv = contactConnection.connReqInv {
                        NavigationLink {
                            AddContactView(contactConnection: contactConnection, connReqInvitation: connReqInv)
                                .navigationTitle(CreateLinkTab.oneTime.title)
                                .navigationBarTitleDisplayMode(.large)
                        } label: {
                            Label("Show QR code", systemImage: "qrcode")
                                .foregroundColor(contactConnection.incognito ? .indigo : .accentColor)
                        }
                    }

                    Button(role: .destructive) {
                        alert = .deleteInvitationAlert
                    } label: {
                        Label("Delete connection", systemImage: "trash")
                            .foregroundColor(Color.red)
                    }
                }
            }
            .listStyle(.insetGrouped)
        }
        .alert(item: $alert) { _alert in
            switch _alert {
            case .deleteInvitationAlert:
                return deleteContactConnectionAlert(contactConnection) { a in
                    alert = .error(title: a.title, error: a.message)
                } success: {
                    dismiss()
                }
            case let .error(title, error): return Alert(title: Text(title), message: Text(error))
            }
        }
        .onAppear {
            localAlias = contactConnection.localAlias
            aliasTextFieldFocused = true
        }
    }

    private func setConnectionAlias() {
        if localAlias == contactConnection.localAlias {
            aliasTextFieldFocused = false
            return
        }
        Task {
            let prevAlias = contactConnection.localAlias
            contactConnection.localAlias = localAlias
            do {
                if let conn = try await apiSetConnectionAlias(connId: contactConnection.pccConnId, localAlias: localAlias) {
                    await MainActor.run {
                        contactConnection = conn
                        ChatModel.shared.updateContactConnection(conn)
                        dismiss()
                    }
                }
            } catch {
                logger.error("setContactAlias error: \(responseError(error))")
                contactConnection.localAlias = prevAlias
            }
        }
    }

    private func contactConnectionText(_ contactConnection: PendingContactConnection) -> LocalizedStringKey {
        contactConnection.viaContactUri
        ? (contactConnection.groupLinkId != nil
           ? "You will be connected to group when the group host's device is online, please wait or check later!"
           : "You will be connected when your connection request is accepted, please wait or check later!"
        )
        : "You will be connected when your contact's device is online, please wait or check later!"
    }
}

struct ContactConnectionInfo_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionInfo(contactConnection: PendingContactConnection.getSampleData())
    }
}
