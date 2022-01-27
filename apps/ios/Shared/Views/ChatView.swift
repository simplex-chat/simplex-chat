//
//  ChatView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatView: View {
    var chat: Chat
    var body: some View {
        var chatItems: [ChatItem]
        switch chat {
        case let .direct(_, items):
                chatItems = items
        case let .group(groupInfo, items):
                chatItems = items
        }
        
        return VStack {
            ChatItemListView(chatItems: chatItems)
        }
    }
}

//struct ChatView_Previews: PreviewProvider {
//    static var previews: some View {
//        ChatView()
//    }
//}
