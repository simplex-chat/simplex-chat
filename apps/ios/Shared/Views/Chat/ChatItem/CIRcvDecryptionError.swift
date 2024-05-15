//
//  CIRcvDecryptionError.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let decryptErrorReason: LocalizedStringKey = "It can happen when you or your connection used the old database backup."

struct CIRcvDecryptionError: View {
    @EnvironmentObject var m: ChatModel
    @ObservedObject var chat: Chat
    var msgDecryptError: MsgDecryptError
    var msgCount: UInt32
    var chatItem: ChatItem
    @State private var alert: CIRcvDecryptionErrorAlert?

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false

    enum CIRcvDecryptionErrorAlert: Identifiable {
        case syncAllowedAlert(_ syncConnection: () -> Void)
        case syncNotSupportedContactAlert
        case syncNotSupportedMemberAlert
        case decryptionErrorAlert
        case error(title: LocalizedStringKey, error: LocalizedStringKey)

        var id: String {
            switch self {
            case .syncAllowedAlert: return "syncAllowedAlert"
            case .syncNotSupportedContactAlert: return "syncNotSupportedContactAlert"
            case .syncNotSupportedMemberAlert: return "syncNotSupportedMemberAlert"
            case .decryptionErrorAlert: return "decryptionErrorAlert"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        viewBody()
            .onAppear {
                // for direct chat ConnectionStats are populated on opening chat, see ChatView onAppear
                if case let .group(groupInfo) = chat.chatInfo,
                   case let .groupRcv(groupMember) = chatItem.chatDir {
                    do {
                        let (member, stats) = try apiGroupMemberInfo(groupInfo.apiId, groupMember.groupMemberId)
                        if let s = stats {
                            m.updateGroupMemberConnectionStats(groupInfo, member, s)
                        }
                    } catch let error {
                        logger.error("apiGroupMemberInfo error: \(responseError(error))")
                    }
                }
            }
            .alert(item: $alert) { alertItem in
                switch(alertItem) {
                case let .syncAllowedAlert(syncConnection): return syncAllowedAlert(syncConnection)
                case .syncNotSupportedContactAlert: return Alert(title: Text("Fix not supported by contact"), message: message())
                case .syncNotSupportedMemberAlert: return Alert(title: Text("Fix not supported by group member"), message: message())
                case .decryptionErrorAlert: return Alert(title: Text("Decryption error"), message: message())
                case let .error(title, error): return Alert(title: Text(title), message: Text(error))
                }
            }
    }

    @ViewBuilder private func viewBody() -> some View {
        if case let .direct(contact) = chat.chatInfo,
           let contactStats = contact.activeConn?.connectionStats {
            if contactStats.ratchetSyncAllowed {
                decryptionErrorItemFixButton(syncSupported: true) {
                    alert = .syncAllowedAlert { syncContactConnection(contact) }
                }
            } else if !contactStats.ratchetSyncSupported {
                decryptionErrorItemFixButton(syncSupported: false) {
                    alert = .syncNotSupportedContactAlert
                }
            } else {
                basicDecryptionErrorItem()
            }
        } else if case let .group(groupInfo) = chat.chatInfo,
                  case let .groupRcv(groupMember) = chatItem.chatDir,
                  let mem = m.getGroupMember(groupMember.groupMemberId),
                  let memberStats = mem.wrapped.activeConn?.connectionStats {
            if memberStats.ratchetSyncAllowed {
                decryptionErrorItemFixButton(syncSupported: true) {
                    alert = .syncAllowedAlert { syncMemberConnection(groupInfo, groupMember) }
                }
            } else if !memberStats.ratchetSyncSupported {
                decryptionErrorItemFixButton(syncSupported: false) {
                    alert = .syncNotSupportedMemberAlert
                }
            } else {
                basicDecryptionErrorItem()
            }
        } else {
            basicDecryptionErrorItem()
        }
    }

    private func basicDecryptionErrorItem() -> some View {
        decryptionErrorItem { alert = .decryptionErrorAlert }
    }

    private func decryptionErrorItemFixButton(syncSupported: Bool, _ onClick: @escaping (() -> Void)) -> some View {
        ZStack(alignment: .bottomTrailing) {
            VStack(alignment: .leading, spacing: 2) {
                HStack {
                    Text(chatItem.content.text)
                        .foregroundColor(.red)
                        .italic()
                }
                (
                    Text(Image(systemName: "exclamationmark.arrow.triangle.2.circlepath"))
                        .foregroundColor(syncSupported ? .accentColor : .secondary)
                        .font(.callout)
                    + Text(" ")
                    + Text("Fix connection")
                        .foregroundColor(syncSupported ? .accentColor : .secondary)
                        .font(.callout)
                    + Text("   ")
                    + ciMetaText(chatItem.meta, chatTTL: nil, encrypted: nil, transparent: true, showViaProxy: showSentViaProxy)
                )
            }
            .padding(.horizontal, 12)
            CIMetaView(chat: chat, chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .onTapGesture(perform: { onClick() })
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .textSelection(.disabled)
    }

    private func decryptionErrorItem(_ onClick: @escaping (() -> Void)) -> some View {
        return ZStack(alignment: .bottomTrailing) {
            HStack {
                Text(chatItem.content.text)
                    .foregroundColor(.red)
                    .italic()
                + Text("   ")
                + ciMetaText(chatItem.meta, chatTTL: nil, encrypted: nil, transparent: true, showViaProxy: showSentViaProxy)
            }
            .padding(.horizontal, 12)
            CIMetaView(chat: chat, chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .onTapGesture(perform: { onClick() })
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .textSelection(.disabled)
    }

    private func message() -> Text {
        var message: Text
        let why = Text(decryptErrorReason)
        switch msgDecryptError {
        case .ratchetHeader:
            message = Text("\(msgCount) messages failed to decrypt.") + Text("\n") + why
        case .tooManySkipped:
            message = Text("\(msgCount) messages skipped.") + Text("\n") + why
        case .ratchetEarlier:
            message = Text("\(msgCount) messages failed to decrypt.") + Text("\n") + why
        case .other:
            message = Text("\(msgCount) messages failed to decrypt.") + Text("\n") + why
        case .ratchetSync:
            message = Text("Encryption re-negotiation failed.")
        }
        return message
    }

    private func syncMemberConnection(_ groupInfo: GroupInfo, _ member: GroupMember) {
        Task {
            do {
                let (mem, stats) = try apiSyncGroupMemberRatchet(groupInfo.apiId, member.groupMemberId, false)
                await MainActor.run {
                    m.updateGroupMemberConnectionStats(groupInfo, mem, stats)
                }
            } catch let error {
                logger.error("syncMemberConnection apiSyncGroupMemberRatchet error: \(responseError(error))")
                let a = getErrorAlert(error, "Error synchronizing connection")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }

    private func syncContactConnection(_ contact: Contact) {
        Task {
            do {
                let stats = try apiSyncContactRatchet(contact.apiId, false)
                await MainActor.run {
                    m.updateContactConnectionStats(contact, stats)
                }
            } catch let error {
                logger.error("syncContactConnection apiSyncContactRatchet error: \(responseError(error))")
                let a = getErrorAlert(error, "Error synchronizing connection")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }

    private func syncAllowedAlert(_ syncConnection: @escaping () -> Void) -> Alert {
        Alert(
            title: Text("Fix connection?"),
            message: message(),
            primaryButton: .default(Text("Fix"), action: syncConnection),
            secondaryButton: .cancel()
        )
    }
}

//struct CIRcvDecryptionError_Previews: PreviewProvider {
//    static var previews: some View {
//        CIRcvDecryptionError(msgDecryptError: .ratchetHeader, msgCount: 1, chatItem: ChatItem.getIntegrityErrorSample())
//    }
//}
