//
//  MemberSupportChatView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 29.04.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct MemberSupportChatView: View {
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        if let chatId = chatModel.chatId, let chat = chatModel.getChat(chatId) {
            // TODO [knocking] open scoped chat
            ChatView(chat: chat)
        }
    }
}

#Preview {
    MemberSupportChatView()
}
