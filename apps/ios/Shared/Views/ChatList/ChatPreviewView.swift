//
//  ChatNavLabel.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 28/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatPreviewView: View {
    @ObservedObject var chat: Chat
    @Environment(\.colorScheme) var colorScheme
    var darkGreen = Color(red: 0, green: 0.5, blue: 0)

    var body: some View {
        let cItem = chat.chatItems.last
        return HStack(spacing: 8) {
            ZStack(alignment: .bottomLeading) {
                ChatInfoImage(chat: chat)
                    .frame(width: 63, height: 63)
                if case .direct = chat.chatInfo,
                   chat.serverInfo.networkStatus == .connected {
                    Image(systemName: "circle.fill")
                        .resizable()
                        .foregroundColor(colorScheme == .dark ? darkGreen : .green)
                        .frame(width: 5, height: 5)
                        .padding([.bottom, .leading], 1)
                }
            }
            .padding(.leading, 4)

            VStack(spacing: 0) {
                HStack(alignment: .top) {
                    Text(chat.chatInfo.chatViewName)
                        .font(.title3)
                        .fontWeight(.bold)
                        .frame(maxHeight: .infinity, alignment: .topLeading)
                    Spacer()
                    Text(getDateFormatter().string(from: cItem?.meta.itemTs ?? chat.chatInfo.createdAt))
                        .font(.subheadline)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
                .padding(.top, 4)
                .padding(.horizontal, 8)

                if let cItem = cItem {
                    Text(chatItemText(cItem))
                        .frame(minWidth: 0, maxWidth: .infinity, minHeight: 44, maxHeight: 44, alignment: .topLeading)
                        .padding([.leading, .trailing], 8)
                        .padding(.bottom, 4)
                }
                else if case let .direct(contact) = chat.chatInfo, !contact.ready {
                    Text("Connecting...")
                        .frame(minWidth: 0, maxWidth: .infinity, minHeight: 44, maxHeight: 44, alignment: .topLeading)
                        .padding([.leading, .trailing], 8)
                        .padding(.bottom, 4)
                }
            }
        }
    }

    private func chatItemText(_ cItem: ChatItem) -> String {
        let t = cItem.content.text
        if case let .groupRcv(groupMember) = cItem.chatDir {
            return groupMember.memberProfile.displayName + ": " +  t
        }
        return t
    }
}

struct ChatPreviewView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: []
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.group,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")]
            ))
        }
        .previewLayout(.fixed(width: 360, height: 78))
    }
}
