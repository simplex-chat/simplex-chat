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
        Text(chatPreview.chatInfo.localDisplayName)
    }
}

struct ChatPreviewView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatPreviewView(chatPreview: ChatPreview(chatInfo: sampleDirectChatInfo))
            ChatPreviewView(chatPreview: ChatPreview(chatInfo: sampleGroupChatInfo))
        }
        .previewLayout(.fixed(width: 300, height: 70))
    }
}
