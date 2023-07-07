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
    @EnvironmentObject var chat: Chat
    var msgDecryptError: MsgDecryptError
    var msgCount: UInt32
    var chatItem: ChatItem
    var showMember = false
    @State private var alert: CIRcvDecryptionErrorAlert?

    enum CIRcvDecryptionErrorAlert: Identifiable {
        case syncAllowedAlert(_ syncConnection: () -> Void)
        case decryptionErrorAlert
        case error(title: LocalizedStringKey, error: LocalizedStringKey)

        var id: String {
            switch self {
            case .syncAllowedAlert: return "syncAllowedAlert"
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
                            ChatModel.shared.updateGroupMemberConnectionStats(groupInfo, member, s)
                        }
                    } catch let error {
                        logger.error("apiGroupMemberInfo error: \(responseError(error))")
                    }
                }
            }
    }

    @ViewBuilder private func viewBody() -> some View {
        if case let .direct(contact) = chat.chatInfo,
           contact.activeConn.connectionStats?.ratchetSyncAllowed ?? false {
            decryptionErrorItem({ syncContactConnection(contact) })
        } else if case let .group(groupInfo) = chat.chatInfo,
                  case let .groupRcv(groupMember) = chatItem.chatDir,
                  let modelMember = ChatModel.shared.groupMembers.first(where: { $0.id == groupMember.id }),
                  modelMember.activeConn?.connectionStats?.ratchetSyncAllowed ?? false {
            decryptionErrorItem({ syncMemberConnection(groupInfo, groupMember) })
        } else {
            decryptionErrorItem()
        }
    }

    @ViewBuilder private func decryptionErrorItem(_ syncConnection: (() -> Void)? = nil) -> some View {
        VStack {
            if let syncConn = syncConnection {
                ZStack(alignment: .bottomTrailing) {
                    VStack(alignment: .leading, spacing: 2) {
                        HStack {
                            if showMember, let member = chatItem.memberDisplayName {
                                Text(member).fontWeight(.medium) + Text(": ")
                            }
                            Text(chatItem.content.text)
                                .foregroundColor(.red)
                                .italic()
                        }
                        (
                            Text(Image(systemName: "exclamationmark.arrow.triangle.2.circlepath")).foregroundColor(.accentColor).font(.callout)
                            + Text(" ")
                            + Text("Fix connection").foregroundColor(.accentColor).font(.callout)
                            + Text("   ")
                            + ciMetaText(chatItem.meta, chatTTL: nil, transparent: true)
                        )
                    }
                    .padding(.horizontal, 12)
                    CIMetaView(chatItem: chatItem)
                        .padding(.horizontal, 12)
                }
                .onTapGesture(perform: {
                    alert = .syncAllowedAlert(syncConn)
                })
            } else {
                ZStack(alignment: .bottomTrailing) {
                    HStack {
                        if showMember, let member = chatItem.memberDisplayName {
                            Text(member).fontWeight(.medium) + Text(": ")
                        }
                        (
                            Text(chatItem.content.text)
                                .foregroundColor(.red)
                                .italic()
                            + Text("   ")
                            + ciMetaText(chatItem.meta, chatTTL: nil, transparent: true)
                        )
                    }
                    .padding(.horizontal, 12)
                    CIMetaView(chatItem: chatItem)
                        .padding(.horizontal, 12)
                }
                .onTapGesture(perform: {
                    alert = .decryptionErrorAlert
                })
            }
        }
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .textSelection(.disabled)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case let .syncAllowedAlert(syncConnection): return syncAllowedAlert(syncConnection)
            case .decryptionErrorAlert: return decryptionErrorAlert()
            case let .error(title, error): return Alert(title: Text(title), message: Text(error))
            }
        }
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
        }
        return message
    }

    private func syncMemberConnection(_ groupInfo: GroupInfo, _ member: GroupMember) {
        Task {
            do {
                let (mem, stats) = try apiSyncGroupMemberRatchet(groupInfo.apiId, member.groupMemberId, false)
                await MainActor.run {
                    ChatModel.shared.updateGroupMemberConnectionStats(groupInfo, mem, stats)
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
                    ChatModel.shared.updateContactConnectionStats(contact, stats)
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

    private func decryptionErrorAlert() -> Alert {
        Alert(title: Text("Decryption error"), message: message())
    }
}

//struct CIRcvDecryptionError_Previews: PreviewProvider {
//    static var previews: some View {
//        CIRcvDecryptionError(msgDecryptError: .ratchetHeader, msgCount: 1, chatItem: ChatItem.getIntegrityErrorSample())
//    }
//}
