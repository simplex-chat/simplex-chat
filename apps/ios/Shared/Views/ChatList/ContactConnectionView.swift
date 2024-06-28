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
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    @State private var localAlias = ""
    @FocusState private var aliasTextFieldFocused: Bool
    @State private var showContactConnectionInfo = false

    var body: some View {
        if case let .contactConnection(conn) = chat.chatInfo {
            contactConnectionView(conn)
        }
    }

    func contactConnectionView(_ contactConnection: PendingContactConnection) -> some View {
        HStack(spacing: 8) {
            Group {
                Image(systemName: contactConnection.initiated ? "link.badge.plus" : "link")
                    .resizable()
                    .scaledToFill()
                    .frame(width: 48, height: 48)
                    .foregroundColor(Color(uiColor: .tertiarySystemGroupedBackground).asAnotherColorFromSecondaryVariant(theme))
                    .onTapGesture { showContactConnectionInfo = true }
            }
            .frame(width: 63, height: 63)
            .padding(.leading, 4)

            VStack(alignment: .leading, spacing: 0) {
                HStack(alignment: .top) {
                    Text(contactConnection.chatViewName)
                        .font(.title3)
                        .bold()
                        .allowsTightening(false)
                        .foregroundColor(theme.colors.secondary)
                        .padding(.horizontal, 8)
                        .padding(.top, 1)
                        .padding(.bottom, 0.5)
                        .frame(alignment: .topLeading)

                    Spacer()

                    formatTimestampText(contactConnection.updatedAt)
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.vertical, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(theme.colors.secondary)
                }
                .padding(.bottom, 2)

                ZStack(alignment: .topTrailing) {
                    Text(contactConnection.description)
                        .frame(maxWidth: .infinity, alignment: .leading)
                    incognitoIcon(contactConnection.incognito, theme.colors.secondary)
                        .padding(.top, 26)
                        .frame(maxWidth: .infinity, alignment: .trailing)
                }
                .padding(.horizontal, 8)

                Spacer()
            }
            .frame(maxHeight: .infinity)
            .appSheet(isPresented: $showContactConnectionInfo) {
                ContactConnectionInfo(contactConnection: contactConnection)
            }
        }
    }
}

struct ContactConnectionView_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionView(chat: Chat(chatInfo: ChatInfo.sampleData.contactConnection))
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
