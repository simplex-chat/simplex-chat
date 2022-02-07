//
//  ChatInfoImage.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatInfoImage: View {
    @ObservedObject var chat: Chat

    var body: some View {
        var iconName: String
        switch chat.chatInfo {
        case .direct: iconName = "person.crop.circle.fill"
        case .group: iconName = "person.2.circle.fill"
        default: iconName = "circle.fill"
        }

        return Image(systemName: iconName)
            .resizable()
            .foregroundColor(Color(uiColor: .tertiarySystemGroupedBackground))
    }
}

struct ChatInfoImage_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoImage(chat: Chat(chatInfo: sampleDirectChatInfo, chatItems: []))
            .previewLayout(.fixed(width: 63, height: 63))
    }
}
