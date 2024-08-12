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
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss: DismissAction
    @State var contactConnection: PendingContactConnection
    @State private var alert: CCInfoAlert?
    @State private var localAlias = ""
    @State private var showIncognitoSheet = false
    @FocusState private var aliasTextFieldFocused: Bool

    enum CCInfoAlert: Identifiable {
        case deleteInvitationAlert
        case error(title: LocalizedStringKey, error: LocalizedStringKey?)

        var id: String {
            switch self {
            case .deleteInvitationAlert: return "deleteInvitationAlert"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        NavigationView {
            let v = List {
                Group {
                    Text(contactConnection.initiated ? "You invited a contact" : "You accepted connection")
                        .font(.largeTitle)
                        .bold()
                        .padding(.bottom)

                    Text(contactConnectionText(contactConnection))
                }
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .onTapGesture { aliasTextFieldFocused = false }

                Section {
                    if contactConnection.groupLinkId == nil {
                        settingsRow("pencil", color: theme.colors.secondary) {
                            TextField("Set contact name…", text: $localAlias)
                                .autocapitalization(.none)
                                .autocorrectionDisabled(true)
                                .focused($aliasTextFieldFocused)
                                .submitLabel(.done)
                                .onSubmit(setConnectionAlias)
                        }
                        .onTapGesture { aliasTextFieldFocused = true }
                    }

                    if contactConnection.initiated,
                       let connReqInv = contactConnection.connReqInv {
                        SimpleXLinkQRCode(uri: simplexChatLink(connReqInv))
                        incognitoEnabled()
                        shareLinkButton(connReqInv, theme.colors.secondary)
                        oneTimeLinkLearnMoreButton(theme.colors.secondary)
                    } else {
                        incognitoEnabled()
                        oneTimeLinkLearnMoreButton(theme.colors.secondary)
                    }
                } footer: {
                    sharedProfileInfo(contactConnection.incognito)
                        .foregroundColor(theme.colors.secondary)
                }

                Section {
                    Button(role: .destructive) {
                        alert = .deleteInvitationAlert
                    } label: {
                        Label("Delete connection", systemImage: "trash")
                            .foregroundColor(Color.red)
                    }
                }
            }
            .modifier(ThemedBackground(grouped: true))
            if #available(iOS 16, *) {
                v
            } else {
                // navigationBarHidden is added conditionally,
                // because the view jumps in iOS 17 if this is added,
                // and on iOS 16+ it is hidden without it.
                v.navigationBarHidden(true)
            }
        }
        .alert(item: $alert) { _alert in
            switch _alert {
            case .deleteInvitationAlert:
                return deleteContactConnectionAlert(contactConnection) { a in
                    alert = .error(title: a.title, error: a.message)
                } success: {
                    dismiss()
                }
            case let .error(title, error): return mkAlert(title: title, message: error)
            }
        }
        .onAppear {
            localAlias = contactConnection.localAlias
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
                        m.updateContactConnection(conn)
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

    @ViewBuilder private func incognitoEnabled() -> some View {
        if contactConnection.incognito {
            ZStack(alignment: .leading) {
                Image(systemName: "theatermasks.fill")
                    .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                    .foregroundColor(Color.indigo)
                    .font(.system(size: 14))
                HStack(spacing: 6) {
                    Text("Incognito")
                    Image(systemName: "info.circle")
                        .foregroundColor(theme.colors.primary)
                        .font(.system(size: 14))
                }
                .onTapGesture {
                    showIncognitoSheet = true
                }
                .padding(.leading, 36)
            }
            .sheet(isPresented: $showIncognitoSheet) {
                IncognitoHelp()
            }
        }
    }
}

private func shareLinkButton(_ connReqInvitation: String, _ secondaryColor: Color) -> some View {
    Button {
        showShareSheet(items: [simplexChatLink(connReqInvitation)])
    } label: {
        settingsRow("square.and.arrow.up", color: secondaryColor) {
            Text("Share 1-time link")
        }
    }
}

private func oneTimeLinkLearnMoreButton(_ secondaryColor: Color) -> some View {
    NavigationLink {
        AddContactLearnMore(showTitle: false)
            .navigationTitle("One-time invitation link")
            .modifier(ThemedBackground())
            .navigationBarTitleDisplayMode(.large)
    } label: {
        settingsRow("info.circle", color: secondaryColor) {
            Text("Learn more")
        }
    }
}

struct ContactConnectionInfo_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionInfo(contactConnection: PendingContactConnection.getSampleData())
    }
}
