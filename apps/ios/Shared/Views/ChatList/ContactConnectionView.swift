//
//  ContactConnectionView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactConnectionView: View {
    @EnvironmentObject var m: ChatModel
    @State var contactConnection: PendingContactConnection
    @State private var localAlias = ""
    @State private var editLocalAlias = false
    @FocusState private var aliasTextFieldFocused: Bool
    @State private var showContactConnectionInfo = false

    var body: some View {
        HStack(spacing: 8) {
            Image(systemName: contactConnection.initiated ? "link.badge.plus" : "link")
                .resizable()
                .foregroundColor(Color(uiColor: .secondarySystemBackground))
                .scaledToFill()
                .frame(width: 48, height: 48)
                .frame(width: 63, height: 63)
                .padding(.leading, 4)
            VStack(alignment: .leading, spacing: 0) {
                HStack(alignment: .top) {
                    if editLocalAlias {
                        let v = TextField("Set contact name…", text: $localAlias)
                            .font(.title3)
                            .disableAutocorrection(true)
                            .focused($aliasTextFieldFocused)
                            .submitLabel(.done)
                            .onChange(of: aliasTextFieldFocused) { focused in
                                if !focused {
                                    setConnectionAlias()
                                }
                            }
                            .onSubmit {
                                setConnectionAlias()
                            }
                            .foregroundColor(.secondary)
                            .padding(.horizontal, 8)
                            .onTapGesture {}
                        if #available(iOS 16.0, *) {
                            v.bold()
                        } else {
                            v
                        }
                    } else {
                        Text(contactConnection.chatViewName)
                            .font(.title3)
                            .bold()
                            .allowsTightening(false)
                            .foregroundColor(.secondary)
                            .padding(.horizontal, 8)
                            .padding(.top, 1)
                            .padding(.bottom, 0.5)
                            .frame(alignment: .topLeading)
                    }

                    Spacer()

                    formatTimestampText(contactConnection.updatedAt)
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.vertical, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
                .padding(.bottom, 2)

                HStack {
                    VStack(alignment: .leading, spacing: 0) {
                        Text(contactConnection.description)
                            .frame(alignment: .topLeading)
                            .padding(.horizontal, 8)
                            .padding(.bottom, 2)

                        if editLocalAlias {
                            HStack {
                                Image(systemName: "multiply")
                                Text("Cancel")
                            }
                            .foregroundColor(.accentColor               )
                            .padding(.leading, 8)
                            .onTapGesture {
                                editLocalAlias = false
                                aliasTextFieldFocused = false
                            }
                        } else {
                            HStack {
                                Image(systemName: "pencil")
                                Text("Edit")
                            }
                            .foregroundColor(.accentColor               )
                            .padding(.leading, 8)
                            .onTapGesture {
                                localAlias = contactConnection.localAlias
                                editLocalAlias = true
                                aliasTextFieldFocused = true
                            }
                        }
                    }
                    Spacer()
                    if contactConnection.connReqInv != nil && contactConnection.initiated  {
                        Image(systemName: "qrcode")
                            .resizable()
                            .scaledToFit()
                            .frame(width: 32, height: 32)
                            .padding(.trailing, 8)
                            .foregroundColor(.accentColor)
                            .onTapGesture { showContactConnectionInfo = true }
                    }
                }

                Spacer()
            }
            .frame(maxHeight: .infinity)
            .sheet(isPresented: $showContactConnectionInfo) {
                if let connReqInv = contactConnection.connReqInv {
                    ContactConnectionInfo(contactConnection: contactConnection, connReqInvitation: connReqInv)
                }
            }
        }
    }

    private func setConnectionAlias() {
        Task {
            do {
                if let conn = try await apiSetConnectionAlias(connId: contactConnection.pccConnId, localAlias: localAlias) {
                    await MainActor.run {
                        contactConnection = conn
                        ChatModel.shared.updateContactConnection(conn)
                        editLocalAlias = false
                    }
                }
            } catch {
                logger.error("setContactAlias error: \(responseError(error))")
            }
        }
    }
}

struct ContactConnectionView_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionView(contactConnection: PendingContactConnection.getSampleData())
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
