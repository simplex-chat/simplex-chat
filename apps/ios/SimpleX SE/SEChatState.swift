//
//  SEChatState.swift
//  SimpleX SE
//
//  Created by User on 18/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// SEStateGroupDefault must not be used in the share extension directly, only via this singleton
class SEChatState {
    static let shared = SEChatState()
    private var value_ = seStateGroupDefault.get()

    var value: SEState {
        value_
    }

    func set(_ state: SEState) {
        seStateGroupDefault.set(state)
        sendSEState(state)
        value_ = state
    }
}

/// Global event loop
/// The process might be reused between multiple share-sheet presentations,
/// when they are triggered by the same host application
final class EventLoop {
    static let shared = EventLoop()

    enum EventHandling {
        case directMessage
        case group
    }

    private var eventHandling: EventHandling?
    private var itemId: ChatItem.ID?
    private weak var model: ShareModel?

    func set(handling: EventHandling, itemId: ChatItem.ID, model: ShareModel) {
        self.eventHandling = handling
        self.itemId = itemId
        self.model = model
    }

    init() {
        Task {
            while true {
                let message = recvSimpleXMsg()
                logger.log(level: .info, "\(message.debugDescription)")
                switch message {
                // Drive the progress bar for direct chats
                case let .sndFileProgressXFTP(_, aChatItem, _, sentSize, totalSize):
                    logger.debug("Received sndFileCompleteXFTP message, sentSize: \(sentSize), totalSize: \(totalSize)")
                    guard eventHandling == .directMessage, let id = aChatItem?.chatItem.id, id == itemId else { continue }
                    await MainActor.run {
                        withAnimation {
                            model?.bottomBar = .loadingBar(progress: Double(sentSize) / Double(totalSize))
                        }
                    }
                // For direct chats show that file upload is complete
                // For group chats wait for 5 seconds after receiving `sndFileCompleteXFTP` to run completion
                case let .sndFileCompleteXFTP(_, aChatItem, _):
                    logger.debug("Received sndFileCompleteXFTP message")
                    guard let eventHandling, aChatItem.chatItem.id == itemId else { continue }
                    switch eventHandling {
                    case .directMessage:
                        await MainActor.run { model?.bottomBar = .loadingBar(progress: 1) }
                    case .group:
                        try? await Task.sleep(for: .seconds(5))
                        model?.completion?(nil)
                    }
                // For direct chats run completion only after receiving `.complete` item state
                case let .chatItemUpdated(_, aChatItem):
                    logger.debug("Received chatItemStatusUpdated message")
                    guard eventHandling == .directMessage,
                          aChatItem.chatItem.id == itemId,
                          aChatItem.chatItem.meta.itemStatus == .sndSent(sndProgress: .complete) else { continue }
                    model?.completion?(nil)
                default: logger.debug("UnhandledMessage \(message.debugDescription)" )
                }
            }
        }
    }
}

/// Waits for other processes to set their state to suspended
/// Will wait for maximum of two seconds, since they might not be running
func waitForOtherProcessesToSuspend() async {
    let startTime = CFAbsoluteTimeGetCurrent()
    while CFAbsoluteTimeGetCurrent() - startTime < 2 {
        try? await Task.sleep(for: .milliseconds(100))
        if appStateGroupDefault.get() == .suspended &&
           nseStateGroupDefault.get() == .suspended {
            break
        }
    }
}
