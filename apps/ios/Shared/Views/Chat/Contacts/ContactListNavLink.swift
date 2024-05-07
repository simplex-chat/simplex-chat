//
//  ContactListNavLink.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 06.05.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactListNavLink: View {
    @ObservedObject var chat: Chat
    
    var body: some View {
        // TODO keep bottom bar?
        switch chat.chatInfo {
        case let .direct(contact):
            NavigationLink {
                ChatInfoView(
                    chat: chat,
                    contact: contact,
                    localAlias: chat.chatInfo.localAlias
                )
            } label: {
                HStack{
                    ProfileImage(imageStr: contact.image, size: 38)
                        .padding(.trailing, 2)
                    previewTitle(contact)
                    if contact.contactConnIncognito {
                        Spacer()
                        Image(systemName: "theatermasks")
                            .resizable()
                            .scaledToFit()
                            .frame(width: 22, height: 22)
                            .foregroundColor(.secondary)
                    }
                }
            }
        default:
            EmptyView()
        }
    }

    @ViewBuilder private func previewTitle(_ contact: Contact) -> some View {
        let t = Text(chat.chatInfo.chatViewName)
        (
            contact.verified == true
            ? verifiedIcon + t
            : t
        )
        .lineLimit(1)
    }

    private var verifiedIcon: Text {
        (Text(Image(systemName: "checkmark.shield")) + Text(" "))
            .foregroundColor(.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }
}

#Preview {
    ContactListNavLink(chat: Chat.sampleData)
}
