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
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    var maxWidth: CGFloat = .infinity
    @Binding var revealed: Bool
    @Binding var allowMenu: Bool
    @Binding var audioPlayer: AudioPlayer?
    @Binding var playbackState: VoiceMessagePlaybackState
    @Binding var playbackTime: TimeInterval?
    init(
        chat: Chat,
        chatItem: ChatItem,
        showMember: Bool = false,
        maxWidth: CGFloat = .infinity,
        revealed: Binding<Bool>,
        allowMenu: Binding<Bool> = .constant(false),
        audioPlayer: Binding<AudioPlayer?> = .constant(nil),
        playbackState: Binding<VoiceMessagePlaybackState> = .constant(.noPlayback),
        playbackTime: Binding<TimeInterval?> = .constant(nil)
    ) {
        self.chat = chat
        self.chatItem = chatItem
        self.maxWidth = maxWidth
        _revealed = revealed
        _allowMenu = allowMenu
        _audioPlayer = audioPlayer
        _playbackState = playbackState
        _playbackTime = playbackTime
    }

    var body: some View {
        let ci = chatItem
        if chatItem.meta.itemDeleted != nil && (!revealed || chatItem.isDeletedContent) {
            MarkedDeletedItemView(chat: chat, chatItem: chatItem, revealed: $revealed)
        } else if ci.quotedItem == nil && ci.meta.itemForwarded == nil && ci.meta.itemDeleted == nil && !ci.meta.isLive {
            if let mc = ci.content.msgContent, mc.isText && isShortEmoji(ci.content.text) {
                EmojiItemView(chat: chat, chatItem: ci)
            } else if ci.content.text.isEmpty, case let .voice(_, duration) = ci.content.msgContent {
                CIVoiceView(chat: chat, chatItem: ci, recordingFile: ci.file, duration: duration, audioPlayer: $audioPlayer, playbackState: $playbackState, playbackTime: $playbackTime, allowMenu: $allowMenu)
            } else if ci.content.msgContent == nil {
                ChatItemContentView(chat: chat, chatItem: chatItem, revealed: $revealed, msgContentView: { Text(ci.text) }) // msgContent is unreachable branch in this case
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
            .map { dropImagePrefix($0) }
            .flatMap { Data(base64Encoded: $0) }
            .flatMap { UIImage(data: $0) }
        let adjustedMaxWidth = {
            if let preview, preview.size.width <= preview.size.height {
                maxWidth * 0.75
            } else {
                maxWidth
            }
        }()
        return FramedItemView(
            chat: chat,
            chatItem: chatItem,
            preview: preview,
            revealed: $revealed,
            maxWidth: maxWidth,
            imgWidth: adjustedMaxWidth,
            videoWidth: adjustedMaxWidth,
            allowMenu: $allowMenu,
            audioPlayer: $audioPlayer,
            playbackState: $playbackState,
            playbackTime: $playbackTime
        )
    }
}

struct ChatItemContentView<Content: View>: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    @Binding var revealed: Bool
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
        case .rcvGroupEvent: eventItemView()
        case .sndGroupEvent: eventItemView()
        case .rcvConnEvent: eventItemView()
        case .sndConnEvent: eventItemView()
        case let .rcvChatFeature(feature, enabled, _): chatFeatureView(feature, enabled.iconColor)
        case let .sndChatFeature(feature, enabled, _): chatFeatureView(feature, enabled.iconColor)
        case let .rcvChatPreference(feature, allowed, param):
            CIFeaturePreferenceView(chat: chat, chatItem: chatItem, feature: feature, allowed: allowed, param: param)
        case let .sndChatPreference(feature, _, _):
            CIChatFeatureView(chat: chat, chatItem: chatItem, revealed: $revealed, feature: feature, icon: feature.icon, iconColor: .secondary)
        case let .rcvGroupFeature(feature, preference, _, role): chatFeatureView(feature, preference.enabled(role, for: chat.chatInfo.groupInfo?.membership).iconColor)
        case let .sndGroupFeature(feature, preference, _, role): chatFeatureView(feature, preference.enabled(role, for: chat.chatInfo.groupInfo?.membership).iconColor)
        case let .rcvChatFeatureRejected(feature): chatFeatureView(feature, .red)
        case let .rcvGroupFeatureRejected(feature): chatFeatureView(feature, .red)
        case .sndModerated: deletedItemView()
        case .rcvModerated: deletedItemView()
        case .rcvBlocked: deletedItemView()
        case let .sndDirectE2EEInfo(e2eeInfo): CIEventView(eventText: directE2EEInfoText(e2eeInfo))
        case let .rcvDirectE2EEInfo(e2eeInfo): CIEventView(eventText: directE2EEInfoText(e2eeInfo))
        case .sndGroupE2EEInfo: CIEventView(eventText: e2eeInfoNoPQText())
        case .rcvGroupE2EEInfo: CIEventView(eventText: e2eeInfoNoPQText())
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
        return CIEventView(eventText: eventItemViewText())
    }

    private func eventItemViewText() -> Text {
        if !revealed, let t = mergedGroupEventText {
            return chatEventText(t + Text(" ") + chatItem.timestampText)
        } else if let member = chatItem.memberDisplayName {
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
        CIChatFeatureView(chat: chat, chatItem: chatItem, revealed: $revealed, feature: feature, iconColor: iconColor)
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
            Text(members) + Text(" ") + Text("and \(count - ns.count) other events")
        } else {
            Text(members)
        }
    }

    private func directE2EEInfoText(_ info: E2EEInfo) -> Text {
        info.pqEnabled
        ? Text("Messages, files and calls are protected by **quantum resistant e2e encryption** with perfect forward secrecy, repudiation and break-in recovery.")
            .font(.caption)
            .foregroundColor(.secondary)
            .fontWeight(.light)
        : e2eeInfoNoPQText()
    }

    private func e2eeInfoNoPQText() -> Text {
        Text("Messages, files and calls are protected by **end-to-end encryption** with perfect forward secrecy, repudiation and break-in recovery.")
            .font(.caption)
            .foregroundColor(.secondary)
            .fontWeight(.light)
    }
}

func chatEventText(_ text: Text) -> Text {
    text
        .font(.caption)
        .foregroundColor(.secondary)
        .fontWeight(.light)
}

func chatEventText(_ eventText: LocalizedStringKey, _ ts: Text) -> Text {
    chatEventText(Text(eventText) + Text(" ") + ts)
}

func chatEventText(_ ci: ChatItem) -> Text {
    chatEventText("\(ci.content.text)", ci.timestampText)
}

struct ChatItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too"), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂"), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂"), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂🙂"), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getDeletedContentSample(), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemDeleted: .deleted(deletedTs: .now)), revealed: Binding.constant(false))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂", .sndSent(sndProgress: .complete), itemLive: true), revealed: Binding.constant(true))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemLive: true), revealed: Binding.constant(true))
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
                chat: Chat.sampleData,
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
                chat: Chat.sampleData,
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
                chat: Chat.sampleData,
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
                chat: Chat.sampleData,
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
                chat: Chat.sampleData,
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
