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
    var contact: Contact
    
    var body: some View {
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
}

#Preview {
    ContactListNavLink(contact: Contact.sampleData)
}
