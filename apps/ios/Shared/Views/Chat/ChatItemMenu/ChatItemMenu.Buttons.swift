//
//  ChatItemMenu.Buttons.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 17/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

extension ChatItemMenu {
    func reactions(from: Int? = nil, till: Int? = nil) -> some View {
        ForEach(availableReactions[(from ?? .zero)..<(till ?? availableReactions.count)]) { reaction in
            Button(reaction.text) {
                Task {
                    do {
                        let cInfo = chat.chatInfo
                        let chatItem = try await apiChatItemReaction(
                            type: cInfo.chatType,
                            id: cInfo.apiId,
                            itemId: chatItem.id,
                            add: true,
                            reaction: reaction
                        )
                        await MainActor.run {
                            m.updateChatItem(chat.chatInfo, chatItem)
                        }
                    } catch let error {
                        logger.error("apiChatItemReaction error: \(responseError(error))")
                    }
                }
            }
        }
    }

    var reply: Button<some View> {
        Button {
            withAnimation {
                if composeState.editing {
                    composeState = ComposeState(contextItem: .quotedItem(chatItem: chatItem))
                } else {
                    composeState = composeState.copy(contextItem: .quotedItem(chatItem: chatItem))
                }
            }
        } label: {
            Label(
                NSLocalizedString("Reply", comment: "chat item action"),
                systemImage: "arrowshape.turn.up.left"
            )
        }
    }

    var forward: Button<some View> {
        Button {
            sheet = .forward(chatItem)
        } label: {
            Label(
                NSLocalizedString("Forward", comment: "chat item action"),
                systemImage: "arrowshape.turn.up.forward"
            )
        }
    }


    var share: Button<some View> {
        Button {
            var shareItems: [Any] = [chatItem.content.text]
            if case .image = chatItem.content.msgContent, let image = getLoadedImage(chatItem.file) {
                shareItems.append(image)
            }
            showShareSheet(items: shareItems)
        } label: {
            Label(
                NSLocalizedString("Share", comment: "chat item action"),
                systemImage: "square.and.arrow.up"
            )
        }
    }

    var copy: Button<some View> {
        Button {
            if case let .image(text, _) = chatItem.content.msgContent,
               text == "",
               let image = getLoadedImage(chatItem.file) {
                UIPasteboard.general.image = image
            } else {
                UIPasteboard.general.string = chatItem.content.text
            }
        } label: {
            Label("Copy", systemImage: "doc.on.doc")
        }
    }

    func save(image: UIImage) -> Button<some View> {
        Button {
            UIImageWriteToSavedPhotosAlbum(image, nil, nil, nil)
        } label: {
            Label(
                NSLocalizedString("Save", comment: "chat item action"),
                systemImage: "square.and.arrow.down"
            )
        }
    }

    func save(file: CryptoFile) -> Button<some View> {
        Button {
            saveCryptoFile(file)
        } label: {
            Label(
                NSLocalizedString("Save", comment: "chat item action"),
                systemImage: file.cryptoArgs == nil ? "square.and.arrow.down" : "lock.open"
            )
        }
    }

    func download(file: CIFile) -> Button<some View> {
        Button {
            Task {
                logger.debug("ChatView downloadFileAction, in Task")
                if let user = m.currentUser {
                    await receiveFile(user: user, fileId: file.fileId)
                }
            }
        } label: {
            Label(
                NSLocalizedString("Download", comment: "chat item action"),
                systemImage: "arrow.down.doc"
            )
        }
    }

    var edit: Button<some View> {
        Button {
            withAnimation {
                composeState = ComposeState(editingItem: chatItem)
            }
        } label: {
            Label(
                NSLocalizedString("Edit", comment: "chat item action"),
                systemImage: "square.and.pencil"
            )
        }
    }

    var info: Button<some View> {
        Button {
            Task {
                do {
                    let cInfo = chat.chatInfo
                    let ciInfo = try await apiGetChatItemInfo(type: cInfo.chatType, id: cInfo.apiId, itemId: chatItem.id)
                    if case let .group(groupInfo) = chat.chatInfo {
                        let groupMembers = await apiListMembers(groupInfo.groupId)
                        await MainActor.run {
                            if m.chatId == groupInfo.id {
                                m.groupMembers = groupMembers.map { GMember.init($0) }
                            }
                        }
                    }
                    await MainActor.run { sheet = .itemInfo(chatItem, ciInfo) }
                } catch let error {
                    logger.error("apiGetChatItemInfo error: \(responseError(error))")
                }
            }
        } label: {
            Label(
                NSLocalizedString("Info", comment: "chat item action"),
                systemImage: "info.circle"
            )
        }
    }

    func cancel(action cancelAction: CancelAction, fileId: Int64) -> Button<some View> {
        Button(role: .destructive) {
            AlertManager.shared.showAlert(Alert(
                title: Text(cancelAction.alert.title),
                message: Text(cancelAction.alert.message),
                primaryButton: .destructive(Text(cancelAction.alert.confirm)) {
                    Task {
                        if let user = m.currentUser {
                            await cancelFile(user: user, fileId: fileId)
                        }
                    }
                },
                secondaryButton: .cancel()
            ))
        } label: {
            Label(
                cancelAction.uiAction,
                systemImage: "xmark"
            )
        }
    }

    var delete: Button<some View> {
        Button(role: .destructive) {
            deleteItems = mergedIndex
                .flatMap { [timelineItem.mergedChatItems[$0]] }
                ?? timelineItem.mergedChatItems + [timelineItem.chatItem]
        } label: {
            Label(
                NSLocalizedString("Delete", comment: "chat item action"),
                systemImage: "trash"
            )
        }
    }

    var expand: Button<some View> {
        Button {
            withAnimation { let _ = expandedItems.insert(chatItem.id) }
        } label: {
            Label(
                NSLocalizedString("Expand", comment: "chat item action"),
                systemImage: "arrow.up.and.line.horizontal.and.arrow.down"
            )
        }
    }

    var collapse: Button<some View> {
        Button {
            expandedItems.remove(timelineItem.chatItem.id)
        } label: {
            Label(
                NSLocalizedString("Hide", comment: "chat item action"),
                systemImage: "arrow.down.and.line.horizontal.and.arrow.up"
            )
        }
    }
}
