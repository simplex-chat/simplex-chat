//
//  ChatInfoToolbar.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let chatImageColorLight = Color(red: 0.9, green: 0.9, blue: 0.9)
private let chatImageColorDark = Color(red: 0.2, green: 0.2, blue: 0.2                                                     )
struct ChatInfoToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat

    var body: some View {
        let cInfo = chat.chatInfo
        return HStack {
            ChatInfoImage(
                chat: chat,
                color: colorScheme == .dark
                        ? chatImageColorDark
                        : chatImageColorLight
            )
            .frame(width: 32, height: 32)
            .padding(.trailing, 4)
            VStack {
                Text(cInfo.displayName).font(.headline)
                if cInfo.fullName != "" && cInfo.displayName != cInfo.fullName {
                    Text(cInfo.fullName).font(.subheadline)
                }
            }
        }
        .foregroundColor(.primary)
    }
}

struct ChatInfoToolbar_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoToolbar(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
    }
}
