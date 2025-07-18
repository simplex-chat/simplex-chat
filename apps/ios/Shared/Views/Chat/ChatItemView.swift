//
//  ChatItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

extension EnvironmentValues {
    struct ShowTimestamp: EnvironmentKey {
        static let defaultValue: Bool = true
    }

    struct Revealed: EnvironmentKey {
        static let defaultValue: Bool = true
    }

    struct ContainerBackground: EnvironmentKey {
        static let defaultValue: UIColor = .clear
    }

    var showTimestamp: Bool {
        get { self[ShowTimestamp.self] }
        set { self[ShowTimestamp.self] = newValue }
    }

    var revealed: Bool {
        get { self[Revealed.self] }
        set { self[Revealed.self] = newValue }
    }

    var containerBackground: UIColor {
        get { self[ContainerBackground.self] }
        set { self[ContainerBackground.self] = newValue }
    }
}

struct ChatItemView: View {
    @ObservedObject var chat: Chat
    @ObservedObject var im: ItemsModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.showTimestamp) var showTimestamp: Bool
    @Environment(\.revealed) var revealed: Bool
    var chatItem: ChatItem
    var scrollToItem: (ChatItem.ID) -> Void
    @Binding var scrollToItemId: ChatItem.ID?
    var maxWidth: CGFloat = .infinity
    @Binding var allowMenu: Bool

    init(
        chat: Chat,
        im: ItemsModel,
        chatItem: ChatItem,
        scrollToItem: @escaping (ChatItem.ID) -> Void,
        scrollToItemId: Binding<ChatItem.ID?> = .constant(nil),
        showMember: Bool = false,
        maxWidth: CGFloat = .infinity,
        allowMenu: Binding<Bool> = .constant(false)
    ) {
        self.chat = chat
        self.im = im
        self.chatItem = chatItem
        self.scrollToItem = scrollToItem
        _scrollToItemId = scrollToItemId
        self.maxWidth = maxWidth
        _allowMenu = allowMenu
    }

    var body: some View {
        let ci = chatItem
        if chatItem.meta.itemDeleted != nil && (!revealed || chatItem.isDeletedContent) {
            MarkedDeletedItemView(chat: chat, im: im, chatItem: chatItem)
        } else if ci.quotedItem == nil && ci.meta.itemForwarded == nil && ci.meta.itemDeleted == nil && !ci.meta.isLive {
            if let mc = ci.content.msgContent, mc.isText && isShortEmoji(ci.content.text) {
                EmojiItemView(chat: chat, chatItem: ci)
            } else if ci.content.text.isEmpty, case let .voice(_, duration) = ci.content.msgContent {
                CIVoiceView(chat: chat, chatItem: ci, recordingFile: ci.file, duration: duration, allowMenu: $allowMenu)
            } else if ci.content.msgContent == nil {
                ChatItemContentView(chat: chat, im: im, chatItem: chatItem, msgContentView: { Text(ci.text) }) // msgContent is unreachable branch in this case
            } else {
                framedItemView()
            }
        } else {
            framedItemView()
        }
    }

    private func framedItemView() -> some View {
        let preview = chatItem.content.msgContent
            .flatMap {
                switch $0 {
                case let .image(_, image): image
                case let .video(_, image, _): image
                default: nil
                }
            }
            .flatMap { imageFromBase64($0) }
        let adjustedMaxWidth = {
            if let preview, preview.size.width <= preview.size.height {
                maxWidth * 0.75
            } else {
                maxWidth
            }
        }()
        return FramedItemView(
            chat: chat,
            im: im,
            chatItem: chatItem,
            scrollToItem: scrollToItem,
            scrollToItemId: $scrollToItemId,
            preview: preview,
            maxWidth: maxWidth,
            imgWidth: adjustedMaxWidth,
            videoWidth: adjustedMaxWidth,
            allowMenu: $allowMenu
        )
    }
}

struct ChatItemContentView<Content: View>: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.revealed) var revealed: Bool
    @ObservedObject var chat: Chat
    @ObservedObject var im: ItemsModel
    var chatItem: ChatItem
    var msgContentView: () -> Content
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    var body: some View {
        switch chatItem.content {
        case .sndMsgContent: msgContentView()
        case .rcvMsgContent: msgContentView()
        case .sndDeleted: deletedItemView()
        case .rcvDeleted: deletedItemView()
        case let .sndCall(status, duration): callItemView(status, duration)
        case let .rcvCall(status, duration): callItemView(status, duration)
        case let .rcvIntegrityError(msgError):
            if developerTools {
                IntegrityErrorItemView(chat: chat, msgError: msgError, chatItem: chatItem)
            } else {
                ZStack {}
            }
        case let .rcvDecryptionError(msgDecryptError, msgCount): CIRcvDecryptionError(chat: chat, msgDecryptError: msgDecryptError, msgCount: msgCount, chatItem: chatItem)
        case let .rcvGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case let .sndGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case .rcvDirectEvent: eventItemView()
        case .rcvGroupEvent(.memberCreatedContact): CIMemberCreatedContactView(chatItem: chatItem)
        case .rcvGroupEvent(.newMemberPendingReview): CIEventView(eventText: pendingReviewEventItemText())
        case .rcvGroupEvent: eventItemView()
        case .sndGroupEvent(.userPendingReview): CIEventView(eventText: pendingReviewEventItemText())
        case .sndGroupEvent: eventItemView()
        case .rcvConnEvent: eventItemView()
        case .sndConnEvent: eventItemView()
        case let .rcvChatFeature(feature, enabled, _): chatFeatureView(feature, enabled.iconColor(theme.colors.secondary))
        case let .sndChatFeature(feature, enabled, _): chatFeatureView(feature, enabled.iconColor(theme.colors.secondary))
        case let .rcvChatPreference(feature, allowed, param):
            CIFeaturePreferenceView(chat: chat, chatItem: chatItem, feature: feature, allowed: allowed, param: param)
        case let .sndChatPreference(feature, _, _):
            CIChatFeatureView(chat: chat, im: im, chatItem: chatItem, feature: feature, icon: feature.icon, iconColor: theme.colors.secondary)
        case let .rcvGroupFeature(feature, preference, _, role): chatFeatureView(feature, preference.enabled(role, for: chat.chatInfo.groupInfo?.membership).iconColor(theme.colors.secondary))
        case let .sndGroupFeature(feature, preference, _, role): chatFeatureView(feature, preference.enabled(role, for: chat.chatInfo.groupInfo?.membership).iconColor(theme.colors.secondary))
        case let .rcvChatFeatureRejected(feature): chatFeatureView(feature, .red)
        case let .rcvGroupFeatureRejected(feature): chatFeatureView(feature, .red)
        case .sndModerated: deletedItemView()
        case .rcvModerated: deletedItemView()
        case .rcvBlocked: deletedItemView()
        case let .sndDirectE2EEInfo(e2eeInfo): CIEventView(eventText: directE2EEInfoText(e2eeInfo))
        case let .rcvDirectE2EEInfo(e2eeInfo): CIEventView(eventText: directE2EEInfoText(e2eeInfo))
        case .sndGroupE2EEInfo: CIEventView(eventText: e2eeInfoNoPQText())
        case .rcvGroupE2EEInfo: CIEventView(eventText: e2eeInfoNoPQText())
        case .chatBanner: EmptyView()
        case let .invalidJSON(json): CIInvalidJSONView(json: json)
        }
    }

    private func deletedItemView() -> some View {
        DeletedItemView(chat: chat, chatItem: chatItem)
    }

    private func callItemView(_ status: CICallStatus, _ duration: Int) -> some View {
        CICallItemView(chat: chat, chatItem: chatItem, status: status, duration: duration)
    }

    private func groupInvitationItemView(_ groupInvitation: CIGroupInvitation, _ memberRole: GroupMemberRole) -> some View {
        CIGroupInvitationView(chat: chat, chatItem: chatItem, groupInvitation: groupInvitation, memberRole: memberRole, chatIncognito: chat.chatInfo.incognito)
    }

    private func eventItemView() -> some View {
        CIEventView(eventText: eventItemViewText(theme.colors.secondary))
    }

    private func pendingReviewEventItemText() -> Text {
        Text(chatItem.content.text)
            .font(.caption)
            .foregroundColor(theme.colors.secondary)
            .fontWeight(.bold)
    }

    private func eventItemViewText(_ secondaryColor: Color) -> Text {
        if !revealed, let t = mergedGroupEventText {
            return chatEventText(t + textSpace + chatItem.timestampText, secondaryColor)
        } else if let member = chatItem.memberDisplayName {
            return Text(member + " ")
                    .font(.caption)
                    .foregroundColor(secondaryColor)
                    .fontWeight(.light)
                + chatEventText(chatItem, secondaryColor)
        } else {
            return chatEventText(chatItem, secondaryColor)
        }
    }

    private func chatFeatureView(_ feature: Feature, _ iconColor: Color) -> some View {
        CIChatFeatureView(chat: chat, im: im, chatItem: chatItem, feature: feature, iconColor: iconColor)
    }

    private var mergedGroupEventText: Text? {
        let (count, ns) = chatModel.getConnectedMemberNames(chatItem)
        let members: LocalizedStringKey =
            switch ns.count {
            case 1: "\(ns[0]) connected"
            case 2: "\(ns[0]) and \(ns[1]) connected"
            case 3: "\(ns[0] + ", " + ns[1]) and \(ns[2]) connected"
            default:
                ns.count > 3
                ? "\(ns[0]), \(ns[1]) and \(ns.count - 2) other members connected"
                : ""
            }
        return if count <= 1 {
            nil
        } else if ns.count == 0 {
            Text("\(count) group events")
        } else if count > ns.count {
            Text(members) + textSpace + Text("and \(count - ns.count) other events")
        } else {
            Text(members)
        }
    }

    private func directE2EEInfoText(_ info: E2EEInfo) -> Text {
        if let pqEnabled = info.pqEnabled {
            pqEnabled
            ? e2eeInfoText("Messages, files and calls are protected by **quantum resistant e2e encryption** with perfect forward secrecy, repudiation and break-in recovery.")
            : e2eeInfoNoPQText()
        } else {
            e2eeInfoText("Messages are protected by **end-to-end encryption**.")
        }
    }

    private func e2eeInfoNoPQText() -> Text {
        e2eeInfoText("Messages, files and calls are protected by **end-to-end encryption** with perfect forward secrecy, repudiation and break-in recovery.")
    }

    private func e2eeInfoText(_ s: LocalizedStringKey) -> Text {
        Text(s)
            .font(.caption)
            .foregroundColor(theme.colors.secondary)
            .fontWeight(.light)
    }
}

func chatEventText(_ text: Text, _ secondaryColor: Color) -> Text {
    text
        .font(.caption)
        .foregroundColor(secondaryColor)
        .fontWeight(.light)
}

func chatEventText(_ eventText: LocalizedStringKey, _ ts: Text, _ secondaryColor: Color) -> Text {
    chatEventText(Text(eventText) + textSpace + ts, secondaryColor)
}

func chatEventText(_ ci: ChatItem, _ secondaryColor: Color) -> Text {
    chatEventText("\(ci.content.text)", ci.timestampText, secondaryColor)
}

struct ChatItemView_Previews: PreviewProvider {
    static var previews: some View {
        let im = ItemsModel.shared
        Group{
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil))
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too"), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil))
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂"), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil))
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂"), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil))
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂🙂"), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil))
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getDeletedContentSample(), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil))
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemDeleted: .deleted(deletedTs: .now)), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil))
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂", .sndSent(sndProgress: .complete), itemLive: true), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil)).environment(\.revealed, true)
            ChatItemView(chat: Chat.sampleData, im: im, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemLive: true), scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil)).environment(\.revealed, true)
        }
        .environment(\.revealed, false)
        .previewLayout(.fixed(width: 360, height: 70))
        .environmentObject(Chat.sampleData)
    }
}

struct ChatItemView_NonMsgContentDeleted_Previews: PreviewProvider {
    static var previews: some View {
        let im = ItemsModel.shared
        let ciFeatureContent = CIContent.rcvChatFeature(feature: .fullDelete, enabled: FeatureEnabled(forUser: false, forContact: false), param: nil)
        Group{
            ChatItemView(
                chat: Chat.sampleData,
                im: im,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "1 skipped message", .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: .rcvIntegrityError(msgError: .msgSkipped(fromMsgId: 1, toMsgId: 2)),
                    quotedItem: nil,
                    file: nil
                ),
                scrollToItem: { _ in },
                scrollToItemId: Binding.constant(nil)
            )
            ChatItemView(
                chat: Chat.sampleData,
                im: im,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "1 skipped message", .rcvRead),
                    content: .rcvDecryptionError(msgDecryptError: .ratchetHeader, msgCount: 2),
                    quotedItem: nil,
                    file: nil
                ),
                scrollToItem: { _ in }, scrollToItemId: Binding.constant(nil)
            )
            ChatItemView(
                chat: Chat.sampleData,
                im: im,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "received invitation to join group team as admin", .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: .rcvGroupInvitation(groupInvitation: CIGroupInvitation.getSample(status: .pending), memberRole: .admin),
                    quotedItem: nil,
                    file: nil
                ),
                scrollToItem: { _ in },
                scrollToItemId: Binding.constant(nil)
            )
            ChatItemView(
                chat: Chat.sampleData,
                im: im,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "group event text", .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: .rcvGroupEvent(rcvGroupEvent: .memberAdded(groupMemberId: 1, profile: Profile.sampleData)),
                    quotedItem: nil,
                    file: nil
                ),
                scrollToItem: { _ in },
                scrollToItemId: Binding.constant(nil)
            )
            ChatItemView(
                chat: Chat.sampleData,
                im: im,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, ciFeatureContent.text, .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: ciFeatureContent,
                    quotedItem: nil,
                    file: nil
                ),
                scrollToItem: { _ in },
                scrollToItemId: Binding.constant(nil)
            )
        }
        .environment(\.revealed, true)
        .previewLayout(.fixed(width: 360, height: 70))
        .environmentObject(Chat.sampleData)
    }
}
