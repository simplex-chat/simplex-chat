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
            ZStack(alignment: .bottomTrailing) {
                ChatInfoImage(
                    chat: chat,
                    size: imageSize,
                    color: Color(uiColor: .tertiaryLabel)
                )
                if chat.chatStats.reportsCount > 0 {
                    Image(systemName: "flag.circle.fill")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 14, height: 14)
                        .symbolRenderingMode(.palette)
                        .foregroundStyle(.white, .red)
                } else if chat.supportUnreadCount > 0 {
                    Image(systemName: "flag.circle.fill")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 14, height: 14)
                        .symbolRenderingMode(.palette)
                        .foregroundStyle(.white, theme.colors.primary)
                }
            }
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
            if let contact = chat.chatInfo.contact,
               contact.ready && contact.active,
               let chatSubStatus = ChatModel.shared.chatSubStatus,
               chatSubStatus != .active {
                SubStatusView(status: chatSubStatus)
                    .padding(.leading, 4)
            }
        }
        .foregroundColor(theme.colors.onBackground)
        .frame(width: 220)
    }

    private var contactVerifiedShield: Text {
        (Text(Image(systemName: "checkmark.shield")) + textSpace)
            .font(.caption)
            .foregroundColor(theme.colors.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }

    struct SubStatusView: View {
        @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
        @EnvironmentObject var theme: AppTheme
        var status: SubscriptionStatus

        var body: some View {
            switch status {
            case .active: EmptyView()
            case .pending: ProgressView()
            case .removed: subStatusError()
            case .noSub: subStatusError()
            }
        }

        @ViewBuilder private func subStatusError() -> some View {
            let dynamicChatInfoSize = dynamicSize(userFont).chatInfoSize
            Image(systemName: "exclamationmark.circle")
                .resizable()
                .scaledToFit()
                .frame(width: dynamicChatInfoSize, height: dynamicChatInfoSize)
                .foregroundColor(theme.colors.secondary)
        }
    }
}

struct ChatInfoToolbar_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoToolbar(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(CurrentColors.toAppTheme())
    }
}
