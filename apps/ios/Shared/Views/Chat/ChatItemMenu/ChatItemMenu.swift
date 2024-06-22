//
//  ChatItemMenu.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 15/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatItemMenu: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    let timelineItem: Timeline.Item
    /// Index, in case this ``ChatItemMenu`` represents to one of the merged items
    let mergedIndex: Int?

    @Binding var composeState: ComposeState
    @Binding var expandedItems: Set<ChatItem.ID>
    @Binding var sheet: Timeline.Sheet?
    @Binding var deleteItems: Array<ChatItem>?

    var body: some View {
        if chatItem.content.msgContent != nil,
           chatItem.meta.itemDeleted == nil || isExpanded {
            if reactionsAllowed { reactionsGroup }
            if replyAllowed { reply }
            if copyAndShareAllowed {
                share
                copy
            }
            fileGroup
            if editAllowed { edit }
            if forwardAllowed { forward }
            if infoAllowed { info }
            if isExpanded { collapse }
            if chatItem.meta.itemDeleted == nil && !chatItem.localNote,
               let file = chatItem.file,
               let cancelAction = file.cancelAction {
                cancel(action: cancelAction, fileId: file.fileId)
            }
            if !isLive || !chatItem.meta.isLive { delete }
        } else if chatItem.meta.itemDeleted != nil {
            if isExpanded {
                collapse
            } else if !chatItem.isDeletedContent {
                expand
            }
            info
            delete
        } else if chatItem.isDeletedContent {
            info
            delete
        } else if chatItem.mergeCategory != nil && (canExpand || isExpanded) {
            if isExpanded { collapse } else { expand }
            delete
        } else if chatItem.showLocalDelete {
            delete
        }
    }

    private var reactionsGroup: some View {
        if #available(iOS 16.4, *) {
            return ControlGroup {
                if availableReactions.count > 4 {
                    reactions(till: 3)
                    Menu {
                        reactions(from: 3)
                    } label: {
                        Image(systemName: "ellipsis")
                    }
                } else { reactions() }
            }.controlGroupStyle(.compactMenu)
        } else {
            return Menu {
                reactions()
            } label: {
                Label(
                    NSLocalizedString("React…", comment: "chat item menu"),
                    systemImage: "face.smiling"
                )
            }
        }
    }

    private var fileGroup: some View {
        Group {
            if let fileSource = fileSource, fileExists {
                if case .image = chatItem.content.msgContent, let image = getLoadedImage(chatItem.file) {
                    if image.imageData != nil {
                        save(file: fileSource)
                    } else {
                        save(image: image)
                    }
                } else {
                    save(file: fileSource)
                }
            } else if let file = chatItem.file, case .rcvInvitation = file.fileStatus, fileSizeValid(file) {
                download(file: file)
            }
        }
    }
}
