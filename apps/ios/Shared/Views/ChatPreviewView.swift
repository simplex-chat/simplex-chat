//
//  ChatPreviewView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 28/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatPreviewView: View {
    var chatPreview: ChatPreview
    
    var body: some View {
        Text(chatPreview.chatInfo.displayName)
    }
}

struct ChatPreviewView_Previews: PreviewProvider {
    static var previews: some View {
        ChatPreviewView(chatPreview: ChatPreview(
            chatInfo: .direct(contact: Contact(
                contactId: 123,
                localDisplayName: "ep",
                profile: Profile(
                    displayName: "ep",
                    fullName: "Ep"
                ),
                viaGroup: nil
            ))
        ))
    }
}
