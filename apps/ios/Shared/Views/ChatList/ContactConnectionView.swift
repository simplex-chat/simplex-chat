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
    @State private var editLocalAlias = false
    @FocusState private var aliasTextFieldFocused: Bool
    @State private var showContactConnectionInfo = false

    var body: some View {
        HStack(spacing: 8) {
            Group {
                if contactConnection.initiated  {
                    let v = Image(systemName: "qrcode")
                        .resizable()
                        .scaledToFill()
                        .frame(width: 40, height: 40)
                    if contactConnection.connReqInv == nil {
                        v.foregroundColor(Color(uiColor: .secondarySystemBackground))
                    } else {
                        v.foregroundColor(contactConnection.incognito ? .indigo : .accentColor)
                        .onTapGesture { showContactConnectionInfo = true }
                    }
                } else {
                    Image(systemName: "link")
                        .resizable()
                        .scaledToFill()
                        .frame(width: 48, height: 48)
                        .foregroundColor(Color(uiColor: .secondarySystemBackground))
                }
            }
            .frame(width: 63, height: 63)
            .padding(.leading, 4)

            VStack(alignment: .leading, spacing: 0) {
                HStack(alignment: .top) {
                    Image(systemName: "pencil")
                        .resizable()
                        .scaledToFill()
                        .frame(width: 16, height: 16)
                        .foregroundColor(.accentColor)
                        .padding(.leading, 8)
                        .padding(.top, 8)
                        .onTapGesture {
                            editLocalAlias = true
                            aliasTextFieldFocused = true
                        }

                    if editLocalAlias {
                        let v = TextField("Set contact name…", text: $contactConnection.localAlias)
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
                            .padding(.trailing, 8)
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
                            .padding(.trailing, 8)
                            .padding(.top, 1)
                            .padding(.bottom, 0.5)
                            .frame(alignment: .topLeading)
                            .onTapGesture {
                                editLocalAlias = true
                                aliasTextFieldFocused = true
                            }
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

                Text(contactConnection.description)
                    .frame(alignment: .topLeading)
                    .padding(.horizontal, 8)
                    .padding(.bottom, 2)

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
                if let conn = try await apiSetConnectionAlias(connId: contactConnection.pccConnId, localAlias: contactConnection.localAlias) {
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
