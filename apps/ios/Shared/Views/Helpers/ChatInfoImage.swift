//
//  ChatInfoImage.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatInfoImage: View {
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    var size: CGFloat
    var color = Color(uiColor: .tertiarySystemGroupedBackground)

    var body: some View {
        var iconName: String
        switch chat.chatInfo {
        case .direct: iconName = "person.crop.circle.fill"
        case .group: iconName = "person.2.circle.fill"
        case .local: iconName = "folder.circle.fill"
        case .contactRequest: iconName = "person.crop.circle.fill"
        default: iconName = "circle.fill"
        }
        let notesColor = colorScheme == .light ? notesChatColorLight : notesChatColorDark
        let iconColor = if case .local = chat.chatInfo { notesColor } else { color }
        return ProfileImage(
            imageStr: chat.chatInfo.image,
            iconName: iconName,
            size: size,
            color: iconColor
        )
    }
}

struct ChatInfoImage_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoImage(
            chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []),
            size: 63,
            color:  Color(red: 0.9, green: 0.9, blue: 0.9)
        )
        .previewLayout(.fixed(width: 63, height: 63))
    }
}
