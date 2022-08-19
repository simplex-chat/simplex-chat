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
    @ObservedObject var chat: Chat
    var color = Color(uiColor: .tertiarySystemGroupedBackground)
    var imageSize: CGFloat = 63

    var body: some View {
        var iconName: String
        switch chat.chatInfo {
        case .direct: iconName = "person.crop.circle.fill"
        case .group: iconName = "person.2.circle.fill"
        case .contactRequest: iconName = "person.crop.circle.fill"
        default: iconName = "circle.fill"
        }
        return ZStack(alignment: .bottomTrailing) {
            ProfileImage(
                imageStr: chat.chatInfo.image,
                iconName: iconName,
                color: color
            )
            .frame(width: imageSize, height: imageSize)
            ZStack {
                chatPreviewImageOverlayIcon()
                    .padding([.bottom, .trailing], 0.1 * imageSize)
                    .scaledToFill()
            }
            .frame(width: 0.2 * imageSize, height: 0.2 * imageSize)
        }
    }

    @ViewBuilder private func chatPreviewImageOverlayIcon() -> some View {
        if case let .group(groupInfo) = chat.chatInfo {
            switch (groupInfo.membership.memberStatus) {
            case .memLeft:
                groupInactiveIcon()
            case .memRemoved:
                groupInactiveIcon()
            case .memGroupDeleted:
                groupInactiveIcon()
            default:
                incognitoIcon()
            }
        } else {
            incognitoIcon()
        }
    }

    @ViewBuilder private func incognitoIcon() -> some View {
        if chat.chatInfo.incognito {
            Image(systemName: "theatermasks.circle.fill")
                .foregroundColor(.indigo)
                .background(Circle().foregroundColor(Color(uiColor: .systemBackground)))
        } else {
            EmptyView()
        }
    }

    @ViewBuilder private func groupInactiveIcon() -> some View {
        Image(systemName: "multiply.circle.fill")
            .foregroundColor(.secondary)
            .background(Circle().foregroundColor(Color(uiColor: .systemBackground)))
    }
}

struct ChatInfoImage_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoImage(
            chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: [])
            , color:  Color(red: 0.9, green: 0.9, blue: 0.9)
        )
        .previewLayout(.fixed(width: 63, height: 63))
    }
}
