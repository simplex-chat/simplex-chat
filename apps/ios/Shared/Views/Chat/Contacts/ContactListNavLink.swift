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
    var contact: Contact
    
    var body: some View {
        NavigationLink {
            ChatInfoView(
                chat: chat,
                contact: contact,
                // TODO
                connectionStats: Binding.constant(nil),
                customUserProfile: Binding.constant(nil),
                localAlias: "",
                connectionCode: Binding.constant(nil)
            )
        } label: {
            HStack{
                ProfileImage(imageStr: contact.image, size: 38)
                    .padding(.trailing, 2)
                Text(contact.chatViewName)
                    .lineLimit(1)
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

//        HStack{
//            ProfileImage(imageStr: contact.image, size: 38)
//                .padding(.trailing, 2)
//            Text(contact.chatViewName)
//                .lineLimit(1)
//            if contact.contactConnIncognito {
//                Spacer()
//                Image(systemName: "theatermasks")
//                    .resizable()
//                    .scaledToFit()
//                    .frame(width: 22, height: 22)
//                    .foregroundColor(.secondary)
//            }
//        }

    }
}

#Preview {
    ContactListNavLink(chat: Chat.sampleData, contact: Contact.sampleData)
}
