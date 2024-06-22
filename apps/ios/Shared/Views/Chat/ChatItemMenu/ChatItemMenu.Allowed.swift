//
//  ChatItemMenu.Computed.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 17/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

extension ChatItemMenu {
    var reactionsAllowed: Bool {
        chat.chatInfo.featureEnabled(.reactions) &&
        chatItem.allowAddReaction
    }

    var replyAllowed: Bool {
        chatItem.meta.itemDeleted == nil &&
        !chatItem.isLiveDummy &&
        !isLive && !chatItem.localNote
    }

    var copyAndShareAllowed: Bool {
        !chatItem.content.text.isEmpty ||
        (chatItem.content.msgContent?.isImage == true && fileExists)
    }

    var editAllowed: Bool {
        chatItem.meta.editable &&
        !(chatItem.content.msgContent?.isVoice ?? false) &&
        !isLive
    }

    var forwardAllowed: Bool {
        chatItem.meta.itemDeleted == nil &&
        (chatItem.file == nil || (fileSource != nil && fileExists)) &&
        !chatItem.isLiveDummy &&
        !isLive
    }

    var infoAllowed: Bool {
        !chatItem.isLiveDummy
    }

    var deleteAllowed: Bool {
        chatItem.meta.itemDeleted == nil &&
        !chatItem.localNote
    }

    var isExpanded: Bool {
        expandedItems.contains(timelineItem.chatItem.id)
    }

    var isLive: Bool {
        composeState.liveMessage != nil
    }

    var chatItem: ChatItem {
        mergedIndex.flatMap { timelineItem.mergedChatItems[$0] }
        ?? timelineItem.chatItem
    }

    var canExpand: Bool {
        mergedIndex == nil &&
        !timelineItem.mergedChatItems.isEmpty
    }

    /// Reactions, which has not been used yet
    var availableReactions: Array<MsgReaction> {
        MsgReaction.values
            .filter { reaction in
                !chatItem.reactions.contains {
                    $0.userReacted && $0.reaction == reaction
                }
            }
    }

    var fileSource: CryptoFile? {
        getLoadedFileSource(chatItem.file)
    }

    var fileExists: Bool {
        fileSource
            .flatMap {
                FileManager.default.fileExists(
                    atPath: getAppFilePath($0.filePath).path
                )
            } ?? false
    }
}
