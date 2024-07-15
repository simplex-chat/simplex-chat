//
//  ChatInfoToolbar.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatInfoToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
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
                size: imageSize,
                color: Color(uiColor: .tertiaryLabel)
            )
            .padding(.trailing, 4)
            let t = Text(cInfo.displayName).font(.headline)
            (cInfo.contact?.verified == true ? contactVerifiedShield + t : t)
                .lineLimit(1)
                .if (cInfo.fullName != "" && cInfo.displayName != cInfo.fullName) { v in
                    VStack(spacing: 0) {
                        v
                        Text(cInfo.fullName).font(.subheadline)
                            .lineLimit(1)
                            .padding(.top, -2)
                    }
                }
        }
        .foregroundColor(theme.colors.onBackground)
        .frame(width: 220)
    }

    private var contactVerifiedShield: Text {
        (Text(Image(systemName: "checkmark.shield")) + Text(" "))
            .font(.caption)
            .foregroundColor(theme.colors.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }
}

struct ChatInfoToolbar_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoToolbar(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(CurrentColors.toAppTheme())
    }
}
