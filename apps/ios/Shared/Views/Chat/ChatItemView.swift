//
//  ChatItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatItemView: View {
    var chatInfo: ChatInfo
    var chatItem: ChatItem
    var maxWidth: CGFloat = .infinity
    @State var scrollProxy: ScrollViewProxy? = nil
    @Binding var revealed: Bool
    @Binding var allowMenu: Bool
    @Binding var audioPlayer: AudioPlayer?
    @Binding var playbackState: VoiceMessagePlaybackState
    @Binding var playbackTime: TimeInterval?
    init(chatInfo: ChatInfo, chatItem: ChatItem, showMember: Bool = false, maxWidth: CGFloat = .infinity, scrollProxy: ScrollViewProxy? = nil, revealed: Binding<Bool>, allowMenu: Binding<Bool> = .constant(false), audioPlayer: Binding<AudioPlayer?> = .constant(nil), playbackState: Binding<VoiceMessagePlaybackState> = .constant(.noPlayback), playbackTime: Binding<TimeInterval?> = .constant(nil)) {
        self.chatInfo = chatInfo
        self.chatItem = chatItem
        self.maxWidth = maxWidth
        _scrollProxy = .init(initialValue: scrollProxy)
        _revealed = revealed
        _allowMenu = allowMenu
        _audioPlayer = audioPlayer
        _playbackState = playbackState
        _playbackTime = playbackTime
    }

    var body: some View {
        let ci = chatItem
        if chatItem.meta.itemDeleted != nil && !revealed {
            MarkedDeletedItemView(chatItem: chatItem)
        } else if ci.quotedItem == nil && ci.meta.itemDeleted == nil && !ci.meta.isLive {
            if let mc = ci.content.msgContent, mc.isText && isShortEmoji(ci.content.text) {
                EmojiItemView(chatItem: ci)
            } else if ci.content.text.isEmpty, case let .voice(_, duration) = ci.content.msgContent {
                CIVoiceView(chatItem: ci, recordingFile: ci.file, duration: duration, audioPlayer: $audioPlayer, playbackState: $playbackState, playbackTime: $playbackTime, allowMenu: $allowMenu)
            } else if ci.content.msgContent == nil {
                ChatItemContentView(chatInfo: chatInfo, chatItem: chatItem, msgContentView: { Text(ci.text) }) // msgContent is unreachable branch in this case
            } else {
                framedItemView()
            }
        } else {
            framedItemView()
        }
    }

    private func framedItemView() -> some View {
        FramedItemView(chatInfo: chatInfo, chatItem: chatItem, maxWidth: maxWidth, scrollProxy: scrollProxy, allowMenu: $allowMenu, audioPlayer: $audioPlayer, playbackState: $playbackState, playbackTime: $playbackTime)
    }
}

struct ChatItemContentView<Content: View>: View {
    @EnvironmentObject var chatModel: ChatModel
    var chatInfo: ChatInfo
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
                IntegrityErrorItemView(msgError: msgError, chatItem: chatItem)
            } else {
                ZStack {}
            }
        case let .rcvDecryptionError(msgDecryptError, msgCount): CIRcvDecryptionError(msgDecryptError: msgDecryptError, msgCount: msgCount, chatItem: chatItem)
        case let .rcvGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case let .sndGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case .rcvGroupEvent(.memberConnected): CIEventView(eventText: membersConnectedItemText)
        case .rcvGroupEvent(.memberCreatedContact): CIMemberCreatedContactView(chatItem: chatItem)
        case .rcvGroupEvent: eventItemView()
        case .sndGroupEvent: eventItemView()
        case .rcvConnEvent: eventItemView()
        case .sndConnEvent: eventItemView()
        case let .rcvChatFeature(feature, enabled, _): chatFeatureView(feature, enabled.iconColor)
        case let .sndChatFeature(feature, enabled, _): chatFeatureView(feature, enabled.iconColor)
        case let .rcvChatPreference(feature, allowed, param):
            CIFeaturePreferenceView(chatItem: chatItem, feature: feature, allowed: allowed, param: param)
        case let .sndChatPreference(feature, _, _):
            CIChatFeatureView(chatItem: chatItem, feature: feature, icon: feature.icon, iconColor: .secondary)
        case let .rcvGroupFeature(feature, preference, _): chatFeatureView(feature, preference.enable.iconColor)
        case let .sndGroupFeature(feature, preference, _): chatFeatureView(feature, preference.enable.iconColor)
        case let .rcvChatFeatureRejected(feature): chatFeatureView(feature, .red)
        case let .rcvGroupFeatureRejected(feature): chatFeatureView(feature, .red)
        case .sndModerated: deletedItemView()
        case .rcvModerated: deletedItemView()
        case let .invalidJSON(json): CIInvalidJSONView(json: json)
        }
    }

    private func deletedItemView() -> some View {
        DeletedItemView(chatItem: chatItem)
    }

    private func callItemView(_ status: CICallStatus, _ duration: Int) -> some View {
        CICallItemView(chatInfo: chatInfo, chatItem: chatItem, status: status, duration: duration)
    }

    private func groupInvitationItemView(_ groupInvitation: CIGroupInvitation, _ memberRole: GroupMemberRole) -> some View {
        CIGroupInvitationView(chatItem: chatItem, groupInvitation: groupInvitation, memberRole: memberRole, chatIncognito: chatInfo.incognito)
    }

    private func eventItemView() -> some View {
        return CIEventView(eventText: eventItemViewText())
    }

    private func eventItemViewText() -> Text {
        if let member = chatItem.memberDisplayName {
            return Text(member + " ")
                    .font(.caption)
                    .foregroundColor(.secondary)
                    .fontWeight(.light)
                + chatEventText(chatItem)
        } else {
            return chatEventText(chatItem)
        }
    }

    private func chatFeatureView(_ feature: Feature, _ iconColor: Color) -> some View {
        CIChatFeatureView(chatItem: chatItem, feature: feature, iconColor: iconColor)
    }

    private var membersConnectedItemText: Text {
        if let t = membersConnectedText {
            return chatEventText(t, chatItem.timestampText)
        } else {
            return eventItemViewText()
        }
    }

    private var membersConnectedText: LocalizedStringKey? {
        let ns = chatModel.getConnectedMemberNames(chatItem)
        return ns.count > 3
            ? "\(ns[0]), \(ns[1]) and \(ns.count - 2) other members connected"
            : ns.count == 3
            ? "\(ns[0] + ", " + ns[1]) and \(ns[2]) connected"
            : ns.count == 2
            ? "\(ns[0]) and \(ns[1]) connected"
            : nil
    }
}

func chatEventText(_ eventText: LocalizedStringKey, _ ts: Text) -> Text {
    (Text(eventText) + Text(" ") + ts)
        .font(.caption)
        .foregroundColor(.secondary)
        .fontWeight(.light)
}

func chatEventText(_ ci: ChatItem) -> Text {
    chatEventText("\(ci.content.text)", ci.timestampText)
}

struct ChatItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂🙂"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getDeletedContentSample(), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemDeleted: .deleted(deletedTs: .now)), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂", .sndSent(sndProgress: .complete), itemLive: true), revealed: Binding.constant(true))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemLive: true), revealed: Binding.constant(true))
        }
        .previewLayout(.fixed(width: 360, height: 70))
        .environmentObject(Chat.sampleData)
    }
}

struct ChatItemView_NonMsgContentDeleted_Previews: PreviewProvider {
    static var previews: some View {
        let ciFeatureContent = CIContent.rcvChatFeature(feature: .fullDelete, enabled: FeatureEnabled(forUser: false, forContact: false), param: nil)
        Group{
            ChatItemView(
                chatInfo: ChatInfo.sampleData.direct,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "1 skipped message", .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: .rcvIntegrityError(msgError: .msgSkipped(fromMsgId: 1, toMsgId: 2)),
                    quotedItem: nil,
                    file: nil
                ),
                revealed: Binding.constant(true)
            )
            ChatItemView(
                chatInfo: ChatInfo.sampleData.direct,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "1 skipped message", .rcvRead),
                    content: .rcvDecryptionError(msgDecryptError: .ratchetHeader, msgCount: 2),
                    quotedItem: nil,
                    file: nil
                ),
                revealed: Binding.constant(true)
            )
            ChatItemView(
                chatInfo: ChatInfo.sampleData.direct,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "received invitation to join group team as admin", .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: .rcvGroupInvitation(groupInvitation: CIGroupInvitation.getSample(status: .pending), memberRole: .admin),
                    quotedItem: nil,
                    file: nil
                ),
                revealed: Binding.constant(true)
            )
            ChatItemView(
                chatInfo: ChatInfo.sampleData.direct,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, "group event text", .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: .rcvGroupEvent(rcvGroupEvent: .memberAdded(groupMemberId: 1, profile: Profile.sampleData)),
                    quotedItem: nil,
                    file: nil
                ),
                revealed: Binding.constant(true)
            )
            ChatItemView(
                chatInfo: ChatInfo.sampleData.direct,
                chatItem: ChatItem(
                    chatDir: .directRcv,
                    meta: CIMeta.getSample(1, .now, ciFeatureContent.text, .rcvRead, itemDeleted: .deleted(deletedTs: .now)),
                    content: ciFeatureContent,
                    quotedItem: nil,
                    file: nil
                ),
                revealed: Binding.constant(true)
            )
        }
        .previewLayout(.fixed(width: 360, height: 70))
        .environmentObject(Chat.sampleData)
    }
}
