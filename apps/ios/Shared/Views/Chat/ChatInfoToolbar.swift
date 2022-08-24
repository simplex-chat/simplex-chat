//
//  ChatInfoToolbar.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let chatImageColorLight = Color(red: 0.9, green: 0.9, blue: 0.9)
let chatImageColorDark = Color(red: 0.2, green: 0.2, blue: 0.2)
struct ChatInfoToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    var imageSize: CGFloat = 32

    var body: some View {
        let cInfo = chat.chatInfo
        return HStack {
            if (cInfo.incognito) {
                Image(systemName: "theatermasks").frame(maxWidth: 24, maxHeight: 24, alignment: .center).foregroundColor(.indigo)
                Spacer().frame(width: 16)
            }
            ChatInfoImage(
                chat: chat,
                color: colorScheme == .dark
                        ? chatImageColorDark
                        : chatImageColorLight
            )
            .frame(width: imageSize, height: imageSize)
            .padding(.trailing, 4)
            VStack {
                Text(cInfo.displayName).font(.headline)
                if cInfo.fullName != "" && cInfo.displayName != cInfo.fullName {
                    Text(cInfo.fullName).font(.subheadline)
                }
            }
        }
        .foregroundColor(.primary)
        .frame(width: 220)
    }
}

struct ChatInfoToolbar_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoToolbar(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
    }
}
