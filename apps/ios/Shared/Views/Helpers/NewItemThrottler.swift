//
//  Collector.swift
//  SimpleX (iOS)
//
//  Created by User on 01/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import Combine
import SimpleXChat

class NewItemThrottler {
    static let shared = NewItemThrottler()

    private let subject = PassthroughSubject<Void, Never>()
    private var bag = Set<AnyCancellable>()
    private var accumulated = [(ChatInfo, ChatItem)]()

    private init() {
        subject
            .throttle(for: 1, scheduler: DispatchQueue.main, latest: true)
            .sink {
                self.accumulated.forEach { cInfo, cItem in
                    ChatModel.shared.addChatItem(cInfo, cItem)
                }
                self.accumulated = []
            }
            .store(in: &bag)
    }

    func receive(_ cItem: ChatItem, for cInfo: ChatInfo) {
        // Messages sent to the current chat are updated directly
        if ChatModel.shared.chatId == cInfo.id {
            ChatModel.shared.addChatItem(cInfo, cItem)
        } else {
            DispatchQueue.main.async { self.accumulated.append((cInfo, cItem)) }
            subject.send()
        }
    }
}
