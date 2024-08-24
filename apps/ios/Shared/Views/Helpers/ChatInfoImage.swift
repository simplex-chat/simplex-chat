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
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    var size: CGFloat
    var color = Color(uiColor: .tertiarySystemGroupedBackground)

    var body: some View {
        let iconColor = if case .local = chat.chatInfo { theme.appColors.primaryVariant2 } else { color }
        return ProfileImage(
            imageStr: chat.chatInfo.image,
            iconName: chatIconName(chat.chatInfo),
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
