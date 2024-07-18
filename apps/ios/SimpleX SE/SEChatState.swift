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

    private var itemId: ChatItem.ID?
    private weak var model: ShareModel?

    func set(itemId: ChatItem.ID, model: ShareModel) {
        self.itemId = itemId
        self.model = model
    }

    init() {
        Task {
            while true {
                switch recvSimpleXMsg() {
                case let .sndFileProgressXFTP(_, aChatItem, _, sentSize, totalSize):
                    if let id = aChatItem?.chatItem.id, id == itemId {
                        await MainActor.run {
                            withAnimation {
                                model?.bottomBar = .loadingBar(progress: Double(sentSize) / Double(totalSize))
                            }
                        }
                    }
                case let .sndFileCompleteXFTP(_, aChatItem, _):
                    if aChatItem.chatItem.id == itemId {
                        await MainActor.run {
                            model?.bottomBar = .loadingBar(progress: 1)
                            model?.completion!(nil)
                        }
                    }
                default: break
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
