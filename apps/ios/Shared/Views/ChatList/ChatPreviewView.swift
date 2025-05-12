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
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
    @ObservedObject var chat: Chat
    @Binding var progressByTimeout: Bool
    @State var deleting: Bool = false
    var darkGreen = Color(red: 0, green: 0.5, blue: 0)
    @State private var activeContentPreview: ActiveContentPreview? = nil
    @State private var showFullscreenGallery: Bool = false

    @AppStorage(DEFAULT_PRIVACY_SHOW_CHAT_PREVIEWS) private var showChatPreviews = true

    var dynamicMediaSize: CGFloat { dynamicSize(userFont).mediaSize }
    var dynamicChatInfoSize: CGFloat { dynamicSize(userFont).chatInfoSize }
    
    var body: some View {
        let cItem = chat.chatItems.last
        return ZStack {
            HStack(spacing: 8) {
                ZStack(alignment: .bottomTrailing) {
                    ChatInfoImage(chat: chat, size: dynamicSize(userFont).profileImageSize)
                    chatPreviewImageOverlayIcon()
                        .padding([.bottom, .trailing], 1)
                }
                .padding(.leading, 4)
                
                let chatTs = if let cItem {
                    cItem.meta.itemTs
                } else {
                    chat.chatInfo.chatTs
                }
                VStack(spacing: 0) {
                    HStack(alignment: .top) {
                        chatPreviewTitle()
                        Spacer()
                        (formatTimestampText(chatTs))
                            .font(.subheadline)
                            .frame(minWidth: 60, alignment: .trailing)
                            .foregroundColor(theme.colors.secondary)
                            .padding(.top, 4)
                    }
                    .padding(.bottom, 4)
                    .padding(.horizontal, 8)
                    
                    ZStack(alignment: .topTrailing) {
                        let chat = activeContentPreview?.chat ?? chat
                        let ci = activeContentPreview?.ci ?? chat.chatItems.last
                        let mc = ci?.content.msgContent
                        HStack(alignment: .top) {
                            let deleted = ci?.isDeletedContent == true || ci?.meta.itemDeleted != nil
                            let showContentPreview = (showChatPreviews && chatModel.draftChatId != chat.id && !deleted) || activeContentPreview != nil
                            if let ci, showContentPreview {
                                chatItemContentPreview(chat, ci)
                            }
                            let mcIsVoice = switch mc { case .voice: true; default: false }
                            if !mcIsVoice || !showContentPreview || mc?.text != "" || chatModel.draftChatId == chat.id {
                                let hasFilePreview = if case .file = mc { true } else { false }
                                chatMessagePreview(cItem, hasFilePreview)
                            } else {
                                Spacer()
                                chatInfoIcon(chat).frame(minWidth: 37, alignment: .trailing)
                            }
                        }
                        .onChange(of: chatModel.stopPreviousRecPlay?.path) { _ in
                            checkActiveContentPreview(chat, ci, mc)
                        }
                        .onChange(of: activeContentPreview) { _ in
                            checkActiveContentPreview(chat, ci, mc)
                        }
                        .onChange(of: showFullscreenGallery) { _ in
                            checkActiveContentPreview(chat, ci, mc)
                        }
                        chatStatusImage()
                            .padding(.top, dynamicChatInfoSize * 1.44)
                            .frame(maxWidth: .infinity, alignment: .trailing)
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.trailing, 8)
                    
                    Spacer()
                }
                .frame(maxHeight: .infinity)
            }
            .opacity(deleting ? 0.4 : 1)
            .padding(.bottom, -8)
            
            if deleting {
                ProgressView()
                    .scaleEffect(2)
            }
        }
        .onChange(of: chatModel.deletedChats.contains(chat.chatInfo.id)) { contains in
            deleting = contains
            // Stop voice when deleting the chat
            if contains, let ci = activeContentPreview?.ci {
                VoiceItemState.stopVoiceInSmallView(chat.chatInfo, ci)
            }
        }

        func checkActiveContentPreview(_ chat: Chat, _ ci: ChatItem?, _ mc: MsgContent?) {
            let playing = chatModel.stopPreviousRecPlay
            if case .voice = activeContentPreview?.mc, playing == nil {
                activeContentPreview = nil
            } else if activeContentPreview == nil {
                if case .image = mc, let ci, let mc, showFullscreenGallery {
                    activeContentPreview = ActiveContentPreview(chat: chat, ci: ci, mc: mc)
                }
                if case .video = mc, let ci, let mc, showFullscreenGallery {
                    activeContentPreview = ActiveContentPreview(chat: chat, ci: ci, mc: mc)
                }
                if case .voice = mc, let ci, let mc, let fileSource = ci.file?.fileSource, playing?.path.hasSuffix(fileSource.filePath) == true {
                    activeContentPreview = ActiveContentPreview(chat: chat, ci: ci, mc: mc)
                }
            } else if case .voice = activeContentPreview?.mc {
                if let playing, let fileSource = ci?.file?.fileSource, !playing.path.hasSuffix(fileSource.filePath) {
                    activeContentPreview = nil
                }
            } else if !showFullscreenGallery {
                activeContentPreview = nil
            }
        }
    }

    @ViewBuilder private func chatPreviewImageOverlayIcon() -> some View {
        switch chat.chatInfo {
        case let .direct(contact):
            if !contact.active {
                inactiveIcon()
            } else {
                EmptyView()
            }
        case let .group(groupInfo):
            switch (groupInfo.membership.memberStatus) {
            case .memRejected: inactiveIcon()
            case .memLeft: inactiveIcon()
            case .memRemoved: inactiveIcon()
            case .memGroupDeleted: inactiveIcon()
            default: EmptyView()
            }
        default:
            EmptyView()
        }
    }

    private func inactiveIcon() -> some View {
        Image(systemName: "multiply.circle.fill")
            .foregroundColor(.secondary.opacity(0.65))
            .background(Circle().foregroundColor(Color(uiColor: .systemBackground)))
    }

    @ViewBuilder private func chatPreviewTitle() -> some View {
        let t = Text(chat.chatInfo.chatViewName).font(.title3).fontWeight(.bold)
        switch chat.chatInfo {
        case let .direct(contact):
            previewTitle(contact.verified == true ? verifiedIcon + t : t).foregroundColor(deleting ? Color.secondary : nil)
        case let .group(groupInfo):
            let v = previewTitle(t)
            switch (groupInfo.membership.memberStatus) {
            case .memInvited: v.foregroundColor(deleting ? theme.colors.secondary : chat.chatInfo.incognito ? .indigo : theme.colors.primary)
            case .memAccepted, .memRejected: v.foregroundColor(theme.colors.secondary)
            default: if deleting  { v.foregroundColor(theme.colors.secondary) } else { v }
            }
        default: previewTitle(t)
        }
    }

    private func previewTitle(_ t: Text) -> some View {
        t.lineLimit(1).frame(alignment: .topLeading)
    }

    private var verifiedIcon: Text {
        (Text(Image(systemName: "checkmark.shield")) + textSpace)
            .foregroundColor(theme.colors.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }

    private func chatPreviewLayout(_ text: Text?, draft: Bool = false, hasFilePreview: Bool = false, hasSecrets: Bool) -> some View {
        ZStack(alignment: .topTrailing) {
            let s = chat.chatStats
            let mentionWidth: CGFloat = if s.unreadMentions > 0 && s.unreadCount > 1 { dynamicSize(userFont).unreadCorner } else { 0 }
            let t = text
                .lineLimit(userFont <= .xxxLarge ? 2 : 1)
                .multilineTextAlignment(.leading)
                .if(hasSecrets, transform: hiddenSecretsView)
                .frame(maxWidth: .infinity, alignment: .topLeading)
                .padding(.leading, hasFilePreview ? 0 : 8)
                .padding(.trailing, mentionWidth + (hasFilePreview ? 38 : 36))
                .offset(x: hasFilePreview ? -2 : 0)
                .fixedSize(horizontal: false, vertical: true)
            if !showChatPreviews && !draft {
                t.privacySensitive(true).redacted(reason: .privacy)
            } else {
                t
            }
            chatInfoIcon(chat).frame(minWidth: 37, alignment: .trailing)
        }
    }

    @ViewBuilder private func chatInfoIcon(_ chat: Chat) -> some View {
        let s = chat.chatStats
        if s.unreadCount > 0 || s.unreadChat {
            let mentionColor = mentionColor(chat)
            HStack(alignment: .center, spacing: 2) {
                if s.unreadMentions > 0 && s.unreadCount > 1 {
                    Text("\(MENTION_START)")
                        .font(userFont <= .xxxLarge ? .body : .callout)
                        .foregroundColor(mentionColor)
                        .frame(minWidth: dynamicChatInfoSize, minHeight: dynamicChatInfoSize)
                        .cornerRadius(dynamicSize(userFont).unreadCorner)
                        .padding(.bottom, 1)
                }
                let singleUnreadIsMention = s.unreadMentions > 0 && s.unreadCount == 1
                (singleUnreadIsMention ? Text("\(MENTION_START)") : unreadCountText(s.unreadCount))
                    .font(userFont <= .xxxLarge ? .caption : .caption2)
                    .foregroundColor(.white)
                    .padding(.horizontal, dynamicSize(userFont).unreadPadding)
                    .frame(minWidth: dynamicChatInfoSize, minHeight: dynamicChatInfoSize)
                    .background(singleUnreadIsMention ? mentionColor : chat.chatInfo.ntfsEnabled(false) || chat.chatInfo.chatType == .local ? theme.colors.primary : theme.colors.secondary)
                    .cornerRadius(dynamicSize(userFont).unreadCorner)
            }
            .frame(height: dynamicChatInfoSize)
        } else if let ntfMode = chat.chatInfo.chatSettings?.enableNtfs, ntfMode != .all {
            let iconSize = ntfMode == .mentions ? dynamicChatInfoSize * 0.8 : dynamicChatInfoSize
            let iconColor = ntfMode == .mentions ? theme.colors.secondary.opacity(0.7) : theme.colors.secondary
            Image(systemName: ntfMode.iconFilled)
                .resizable()
                .scaledToFill()
                .frame(width: iconSize, height: iconSize)
                .foregroundColor(iconColor)
        } else if chat.chatInfo.chatSettings?.favorite ?? false {
            Image(systemName: "star.fill")
                .resizable()
                .scaledToFill()
                .frame(width: dynamicChatInfoSize, height: dynamicChatInfoSize)
                .padding(.trailing, 1)
                .foregroundColor(theme.colors.secondary.opacity(0.65))
        } else {
            Color.clear.frame(width: 0)
        }
    }
    
    private func mentionColor(_ chat: Chat) -> Color {
        switch chat.chatInfo.chatSettings?.enableNtfs {
        case .all: theme.colors.primary
        case .mentions: theme.colors.primary
        default: theme.colors.secondary
        }
    }

    private func messageDraft(_ draft: ComposeState) -> (Text, Bool) {
        let msg = draft.message
        let r = messageText(msg, parseSimpleXMarkdown(msg), sender: nil, preview: true, mentions: draft.mentions, userMemberId: nil, showSecrets: nil, backgroundColor: UIColor(theme.colors.background))
        return (image("rectangle.and.pencil.and.ellipsis", color: theme.colors.primary)
                    + attachment()
                    + Text(AttributedString(r.string)),
                r.hasSecrets)

        func image(_ s: String, color: Color = Color(uiColor: .tertiaryLabel)) -> Text {
            Text(Image(systemName: s)).foregroundColor(color) + textSpace
        }

        func attachment() -> Text {
            switch draft.preview {
            case let .filePreview(fileName, _): return image("doc.fill") + Text(fileName) + textSpace
            case .mediaPreviews: return image("photo")
            case let .voicePreview(_, duration): return image("play.fill") + Text(durationText(duration))
            default: return Text("")
            }
        }
    }

    func chatItemPreview(_ cItem: ChatItem) -> (Text, Bool) {
        let itemText = cItem.meta.itemDeleted == nil ? cItem.text : markedDeletedText()
        let itemFormattedText = cItem.meta.itemDeleted == nil ? cItem.formattedText : nil
        let r = messageText(itemText, itemFormattedText, sender: cItem.memberDisplayName, preview: true, mentions: cItem.mentions, userMemberId: chat.chatInfo.groupInfo?.membership.memberId, showSecrets: nil, backgroundColor: UIColor(theme.colors.background), prefix: prefix())
        return (Text(AttributedString(r.string)), r.hasSecrets)

        // same texts are in markedDeletedText in MarkedDeletedItemView, but it returns LocalizedStringKey;
        // can be refactored into a single function if functions calling these are changed to return same type
        func markedDeletedText() -> String {
            if cItem.meta.itemDeleted != nil, cItem.isReport {
                "archived report"
            } else {
                switch cItem.meta.itemDeleted {
                case let .moderated(_, byGroupMember): String.localizedStringWithFormat(NSLocalizedString("moderated by %@", comment: "marked deleted chat item preview text"), byGroupMember.displayName)
                case .blocked: NSLocalizedString("blocked", comment: "marked deleted chat item preview text")
                case .blockedByAdmin: NSLocalizedString("blocked by admin", comment: "marked deleted chat item preview text")
                case .deleted, nil: NSLocalizedString("marked deleted", comment: "marked deleted chat item preview text")
                }
            }
        }

        func attachment() -> String? {
            switch cItem.content.msgContent {
            case .file: return "doc.fill"
            case .image: return "photo"
            case .video: return "video"
            case .voice: return "play.fill"
            default: return nil
            }
        }
        
        func prefix() -> NSAttributedString? {
            switch cItem.content.msgContent {
            case let .report(_, reason): reason.attrString
            default: nil
            }
        }
    }

    @ViewBuilder private func chatMessagePreview(_ cItem: ChatItem?, _ hasFilePreview: Bool = false) -> some View {
        if chatModel.draftChatId == chat.id, let draft = chatModel.draft {
            let (t, hasSecrets) = messageDraft(draft)
            chatPreviewLayout(t, draft: true, hasFilePreview: hasFilePreview, hasSecrets: hasSecrets)
        } else if let cItem = cItem {
            let (t, hasSecrets) = chatItemPreview(cItem)
            chatPreviewLayout(itemStatusMark(cItem) + t, hasFilePreview: hasFilePreview, hasSecrets: hasSecrets)
        } else {
            switch (chat.chatInfo) {
            case let .direct(contact):
                if contact.activeConn == nil && contact.profile.contactLink != nil && contact.active {
                    chatPreviewInfoText("Tap to Connect")
                        .foregroundColor(theme.colors.primary)
                } else if !contact.sndReady && contact.activeConn != nil {
                    if contact.nextSendGrpInv {
                        chatPreviewInfoText("send direct message")
                    } else if contact.active {
                        chatPreviewInfoText("connecting…")
                    }
                }
            case let .group(groupInfo):
                switch (groupInfo.membership.memberStatus) {
                case .memRejected: chatPreviewInfoText("rejected")
                case .memInvited: groupInvitationPreviewText(groupInfo)
                case .memAccepted: chatPreviewInfoText("connecting…")
                default: EmptyView()
                }
            default: EmptyView()
            }
        }
    }

    @ViewBuilder func chatItemContentPreview(_ chat: Chat, _ ci: ChatItem) -> some View {
        let mc = ci.content.msgContent
        switch mc {
        case let .link(_, preview):
            smallContentPreview(size: dynamicMediaSize) {
                ZStack(alignment: .topTrailing) {
                    Image(uiImage: imageFromBase64(preview.image) ?? UIImage(systemName: "arrow.up.right")!)
                        .resizable()
                        .aspectRatio(contentMode: .fill)
                        .frame(width: dynamicMediaSize, height: dynamicMediaSize)
                    ZStack {
                        Image(systemName: "arrow.up.right")
                            .resizable()
                            .foregroundColor(Color.white)
                            .font(.system(size: 15, weight: .black))
                            .frame(width: 8, height: 8)
                    }
                    .frame(width: 16, height: 16)
                    .background(Color.black.opacity(0.25))
                    .cornerRadius(8)
                }
                .onTapGesture {
                    openBrowserAlert(uri: preview.uri)
                }
            }
        case let .image(_, image):
            smallContentPreview(size: dynamicMediaSize) {
                CIImageView(chatItem: ci, preview: imageFromBase64(image), maxWidth: dynamicMediaSize, smallView: true, showFullScreenImage: $showFullscreenGallery)
            }
        case let .video(_,image, duration):
            smallContentPreview(size: dynamicMediaSize) {
                CIVideoView(chatItem: ci, preview: imageFromBase64(image), duration: duration, maxWidth: dynamicMediaSize, videoWidth: nil, smallView: true, showFullscreenPlayer: $showFullscreenGallery)
            }
        case let .voice(_, duration):
            smallContentPreviewVoice(size: dynamicMediaSize) {
                CIVoiceView(chat: chat, chatItem: ci, recordingFile: ci.file, duration: duration, allowMenu: Binding.constant(true), smallViewSize: dynamicMediaSize)
            }
        case .file:
            smallContentPreviewFile(size: dynamicMediaSize) {
                CIFileView(file: ci.file, edited: ci.meta.itemEdited, smallViewSize: dynamicMediaSize)
            }
        default: EmptyView()
        }
    }


    @ViewBuilder private func groupInvitationPreviewText(_ groupInfo: GroupInfo) -> some View {
        groupInfo.membership.memberIncognito
        ? chatPreviewInfoText("join as \(groupInfo.membership.memberProfile.displayName)")
        : chatPreviewInfoText("you are invited to group")
    }

    private func chatPreviewInfoText(_ text: LocalizedStringKey) -> some View {
        Text(text)
            .frame(maxWidth: .infinity, minHeight: 44, maxHeight: 44, alignment: .topLeading)
            .padding([.leading, .trailing], 8)
            .padding(.bottom, 4)
    }

    private func itemStatusMark(_ cItem: ChatItem) -> Text {
        switch cItem.meta.itemStatus {
        case .sndErrorAuth, .sndError:
            return Text(Image(systemName: "multiply"))
                .font(.caption)
                .foregroundColor(.red) + textSpace
        case .sndWarning:
            return Text(Image(systemName: "exclamationmark.triangle.fill"))
                .font(.caption)
                .foregroundColor(.orange) + textSpace
        default: return Text("")
        }
    }

    @ViewBuilder private func chatStatusImage() -> some View {
        let size = dynamicSize(userFont).incognitoSize
        switch chat.chatInfo {
        case let .direct(contact):
            if contact.active && contact.activeConn != nil {
                NetworkStatusView(contact: contact, size: size)
            } else {
                incognitoIcon(chat.chatInfo.incognito, theme.colors.secondary, size: size)
            }
        case .group:
            if progressByTimeout {
                ProgressView()
            } else if chat.chatStats.reportsCount > 0 {
                groupReportsIcon(size: size * 0.8)
            } else {
                incognitoIcon(chat.chatInfo.incognito, theme.colors.secondary, size: size)
            }
        default:
            incognitoIcon(chat.chatInfo.incognito, theme.colors.secondary, size: size)
        }
    }

    struct NetworkStatusView: View {
        @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
        @EnvironmentObject var theme: AppTheme
        @ObservedObject var networkModel = NetworkModel.shared

        let contact: Contact
        let size: CGFloat

        var body: some View {
            let dynamicChatInfoSize = dynamicSize(userFont).chatInfoSize
            switch (networkModel.contactNetworkStatus(contact)) {
            case .connected: incognitoIcon(contact.contactConnIncognito, theme.colors.secondary, size: size)
            case .error:
                Image(systemName: "exclamationmark.circle")
                    .resizable()
                    .scaledToFit()
                    .frame(width: dynamicChatInfoSize, height: dynamicChatInfoSize)
                    .foregroundColor(theme.colors.secondary)
            default:
                ProgressView()
            }
        }
    }
}

@ViewBuilder func incognitoIcon(_ incognito: Bool, _ secondaryColor: Color, size: CGFloat) -> some View {
    if incognito {
        Image(systemName: "theatermasks")
            .resizable()
            .scaledToFit()
            .frame(width: size, height: size)
            .foregroundColor(secondaryColor)
    } else {
        EmptyView()
    }
}

func groupReportsIcon(size: CGFloat) -> some View {
    Image(systemName: "flag")
        .resizable()
        .scaledToFit()
        .frame(width: size, height: size)
        .foregroundColor(.red)
}

func smallContentPreview(size: CGFloat, _ view: @escaping () -> some View) -> some View {
    view()
    .frame(width: size, height: size)
    .cornerRadius(8)
    .overlay(RoundedRectangle(cornerSize: CGSize(width: 8, height: 8))
        .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true))
    .padding(.vertical, size / 6)
    .padding(.leading, 3)
    .offset(x: 6)
}

func smallContentPreviewVoice(size: CGFloat, _ view: @escaping () -> some View) -> some View {
    view()
    .frame(height: voiceMessageSizeBasedOnSquareSize(size))
    .padding(.vertical, size / 6)
    .padding(.leading, 8)
}

func smallContentPreviewFile(size: CGFloat, _ view: @escaping () -> some View) -> some View {
    view()
    .frame(width: size, height: size)
    .padding(.vertical, size / 7)
    .padding(.leading, 5)
}

func unreadCountText(_ n: Int) -> Text {
    Text(n > 999 ? "\(n / 1000)k" : n > 0 ? "\(n)" : "")
}

private struct ActiveContentPreview: Equatable {
    var chat: Chat
    var ci: ChatItem
    var mc: MsgContent

    static func == (lhs: ActiveContentPreview, rhs: ActiveContentPreview) -> Bool {
        lhs.chat.id == rhs.chat.id && lhs.ci.id == rhs.ci.id && lhs.mc == rhs.mc
    }
}

struct ChatPreviewView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: []
            ), progressByTimeout: Binding.constant(false))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete))]
            ), progressByTimeout: Binding.constant(false))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete))],
                chatStats: ChatStats(unreadCount: 11, minUnreadItemId: 0)
            ), progressByTimeout: Binding.constant(false))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemDeleted: .deleted(deletedTs: .now))]
            ), progressByTimeout: Binding.constant(false))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete))],
                chatStats: ChatStats(unreadCount: 3, minUnreadItemId: 0)
            ), progressByTimeout: Binding.constant(false))
            ChatPreviewView(chat: Chat(
                chatInfo: ChatInfo.sampleData.group,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "Lorem ipsum dolor sit amet, d. consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")],
                chatStats: ChatStats(unreadCount: 11, minUnreadItemId: 0)
            ), progressByTimeout: Binding.constant(false))
        }
        .previewLayout(.fixed(width: 360, height: 78))
    }
}
