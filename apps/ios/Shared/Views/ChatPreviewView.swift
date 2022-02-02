//
//  ChatPreviewView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 28/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatPreviewView: View {
    @ObservedObject var chat: Chat

    var body: some View {
        let cItem = chat.chatItems.last
        return VStack(spacing: 4) {
            HStack(alignment: .top) {
                Text(chat.chatInfo.localDisplayName)
                    .font(.title3)
                    .fontWeight(.bold)
                    .padding(.leading, 8)
                    .padding(.top, 4)
                    .frame(maxHeight: .infinity, alignment: .topLeading)
                Spacer()
                if let cItem = cItem {
                    Text(getDateFormatter().string(from: cItem.meta.itemTs))
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.top, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
            }
            if let cItem = cItem {
                Text(cItem.content.text)
                    .frame(minWidth: 0, maxWidth: .infinity, minHeight: 44, maxHeight: 44, alignment: .topLeading)
                    .padding([.leading, .trailing], 8)
                    .padding(.bottom, 4)
                    .padding(.top, 1)
            }
//            else if case let .direct(contact) = chatPreview.chatInfo, !contact.connected {
//                Text("Connecting...")
//                    .frame(minWidth: 0, maxWidth: .infinity, minHeight: 44, maxHeight: 44, alignment: .topLeading)
//                    .padding([.leading, .trailing], 8)
//                    .padding(.bottom, 4)
//                    .padding(.top, 1)
//            }
        }
    }
}

struct ChatPreviewView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatPreviewView(chat: Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: []
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "hello")]
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: sampleGroupChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")]
            ))
        }
        .previewLayout(.fixed(width: 360, height: 80))
    }
}
