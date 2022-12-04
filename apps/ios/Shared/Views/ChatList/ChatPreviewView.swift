//
//  ChatNavLabel.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 28/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatPreviewView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var chat: Chat
    @Environment(\.colorScheme) var colorScheme
    var darkGreen = Color(red: 0, green: 0.5, blue: 0)

    var body: some View {
        let cItem = chat.chatItems.last
        return HStack(spacing: 8) {
            ZStack(alignment: .bottomTrailing) {
                ChatInfoImage(chat: chat)
                    .frame(width: 63, height: 63)
                chatPreviewImageOverlayIcon()
                    .padding([.bottom, .trailing], 1)
            }
            .padding(.leading, 4)

            VStack(spacing: 0) {
                HStack(alignment: .top) {
                    chatPreviewTitle()
                    Spacer()
                    (cItem?.timestampText ?? formatTimestampText(chat.chatInfo.updatedAt))
                        .font(.subheadline)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                        .padding(.top, 4)
                }
                .padding(.bottom, 4)
                .padding(.horizontal, 8)

                ZStack(alignment: .topTrailing) {
                    chatPreviewText(cItem)
                    if case .direct = chat.chatInfo {
                        chatStatusImage()
                            .padding(.top, 24)
                            .frame(maxWidth: .infinity, alignment: .trailing)
                    }
                }
                .padding(.trailing, 8)
                
                Spacer()
            }
            .frame(maxHeight: .infinity)
        }
        .padding(.bottom, -8)
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
            default: EmptyView()
            }
        } else {
            EmptyView()
        }
    }

    @ViewBuilder private func groupInactiveIcon() -> some View {
        Image(systemName: "multiply.circle.fill")
            .foregroundColor(.secondary)
            .background(Circle().foregroundColor(Color(uiColor: .systemBackground)))
    }

    @ViewBuilder private func chatPreviewTitle() -> some View {
        let v = Text(chat.chatInfo.chatViewName)
            .font(.title3)
            .fontWeight(.bold)
            .lineLimit(1)
            .frame(alignment: .topLeading)
        switch (chat.chatInfo) {
        case .direct:
            v.foregroundColor(chat.chatInfo.ready ? .primary : .secondary)
        case .group(groupInfo: let groupInfo):
            switch (groupInfo.membership.memberStatus) {
            case .memInvited:
                chat.chatInfo.incognito ? v.foregroundColor(.indigo) : v.foregroundColor(.accentColor)
            case .memAccepted:
                v.foregroundColor(.secondary)
            default: v
            }
        default: v
        }
    }

    @ViewBuilder private func chatPreviewText(_ cItem: ChatItem?) -> some View {
        if let cItem = cItem {
            let itemText = !cItem.meta.itemDeleted ? cItem.text : NSLocalizedString("marked deleted", comment: "marked deleted chat item preview text")
            let itemFormattedText = !cItem.meta.itemDeleted ? cItem.formattedText : nil
            ZStack(alignment: .topTrailing) {
                (itemStatusMark(cItem) + messageText(itemText, itemFormattedText, cItem.memberDisplayName, preview: true))
                    .lineLimit(2)
                    .multilineTextAlignment(.leading)
                    .frame(maxWidth: .infinity, alignment: .topLeading)
                    .padding(.leading, 8)
                    .padding(.trailing, 36)
                let s = chat.chatStats
                if s.unreadCount > 0 || s.unreadChat {
                    unreadCountText(s.unreadCount)
                        .font(.caption)
                        .foregroundColor(.white)
                        .padding(.horizontal, 4)
                        .frame(minWidth: 18, minHeight: 18)
                        .background(chat.chatInfo.ntfsEnabled ? Color.accentColor : Color.secondary)
                        .cornerRadius(10)
                } else if !chat.chatInfo.ntfsEnabled {
                    Image(systemName: "speaker.slash.fill")
                        .foregroundColor(.secondary)
                }
            }
        } else {
            switch (chat.chatInfo) {
            case let .direct(contact):
                if !contact.ready {
                    chatPreviewInfoText("connecting…")
                }
            case let .group(groupInfo):
                switch (groupInfo.membership.memberStatus) {
                case .memInvited: groupInvitationPreviewText(groupInfo)
                case .memAccepted: chatPreviewInfoText("connecting…")
                default: EmptyView()
                }
            default: EmptyView()
            }
        }
    }

    @ViewBuilder private func groupInvitationPreviewText(_ groupInfo: GroupInfo) -> some View {
        groupInfo.membership.memberIncognito
        ? chatPreviewInfoText("join as \(groupInfo.membership.memberProfile.displayName)")
        : (chatModel.incognito
           ? chatPreviewInfoText("join as \(chatModel.currentUser?.profile.displayName ?? "yourself")")
           : chatPreviewInfoText("you are invited to group")
        )
    }

    @ViewBuilder private func chatPreviewInfoText(_ text: LocalizedStringKey) -> some View {
        Text(text)
            .frame(maxWidth: .infinity, minHeight: 44, maxHeight: 44, alignment: .topLeading)
            .padding([.leading, .trailing], 8)
            .padding(.bottom, 4)
    }

    private func itemStatusMark(_ cItem: ChatItem) -> Text {
        switch cItem.meta.itemStatus {
        case .sndErrorAuth:
            return Text(Image(systemName: "multiply"))
                .font(.caption)
                .foregroundColor(.red) + Text(" ")
        case .sndError:
            return Text(Image(systemName: "exclamationmark.triangle.fill"))
                .font(.caption)
                .foregroundColor(.yellow) + Text(" ")
        default: return Text("")
        }
    }

    @ViewBuilder private func chatStatusImage() -> some View {
        switch chat.serverInfo.networkStatus {
        case .connected: EmptyView()
        case .error:
            Image(systemName: "exclamationmark.circle")
                .resizable()
                .scaledToFit()
                .frame(width: 17, height: 17)
                .foregroundColor(.secondary)
        default:
            ProgressView()
        }
    }
}

func unreadCountText(_ n: Int) -> Text {
    Text(n > 999 ? "\(n / 1000)k" : n > 0 ? "\(n)" : "")
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
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent)]
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent)],
                chatStats: ChatStats(unreadCount: 11, minUnreadItemId: 0)
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent, true, false)]
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent)],
                chatStats: ChatStats(unreadCount: 3, minUnreadItemId: 0),
                serverInfo: Chat.ServerInfo(networkStatus: .error("status"))
            ))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.group,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "Lorem ipsum dolor sit amet, d. consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")],
                chatStats: ChatStats(unreadCount: 11, minUnreadItemId: 0)
            ))
        }
        .previewLayout(.fixed(width: 360, height: 78))
    }
}
