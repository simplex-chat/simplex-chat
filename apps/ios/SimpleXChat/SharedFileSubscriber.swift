//
//  SharedFileSubscriber.swift
//  SimpleXChat
//
//  Created by Evgeny on 09/12/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import Foundation

public typealias AppSubscriber = SharedFileSubscriber<ProcessMessage<AppProcessMessage>>

public typealias NSESubscriber = SharedFileSubscriber<ProcessMessage<NSEProcessMessage>>

public class SharedFileSubscriber<Message: Codable>: NSObject, NSFilePresenter {
    var fileURL: URL
    public var presentedItemURL: URL?
    public var presentedItemOperationQueue: OperationQueue = .main
    var subscriber: (Message) -> Void

    init(fileURL: URL, onMessage: @escaping (Message) -> Void) {
        self.fileURL = fileURL
        presentedItemURL = fileURL
        subscriber = onMessage
        super.init()
        NSFileCoordinator.addFilePresenter(self)
    }

    public func presentedItemDidChange() {
        do {
            let data = try Data(contentsOf: fileURL)
            let msg = try jsonDecoder.decode(Message.self, from: data)
            subscriber(msg)
        } catch let error {
            logger.error("presentedItemDidChange error: \(error)")
        }
    }

    static func notify(url: URL, message: Message) {
        let fc = NSFileCoordinator(filePresenter: nil)
        fc.coordinate(writingItemAt: url, options: [], error: nil) { newURL in
            do {
                let data = try jsonEncoder.encode(message)
                try data.write(to: newURL, options: [.atomic])
            } catch {
                logger.error("notifyViaSharedFile error: \(error)")
            }
        }
    }

    deinit {
        NSFileCoordinator.removeFilePresenter(self)
    }
}

let appMessagesSharedFile = getGroupContainerDirectory().appendingPathComponent("chat.simplex.app.messages", isDirectory: false)

let nseMessagesSharedFile = getGroupContainerDirectory().appendingPathComponent("chat.simplex.app.SimpleX-NSE.messages", isDirectory: false)

public struct ProcessMessage<Message: Codable>: Codable {
    var createdAt: Date = Date.now
    var message: Message
}

public enum AppProcessMessage: Codable {
    case state(state: AppState)
}

public enum NSEProcessMessage: Codable {
    case state(state: NSEState)
}

public func sendAppProcessMessage(_ message: AppProcessMessage) {
    SharedFileSubscriber.notify(url: appMessagesSharedFile, message: ProcessMessage(message: message))
}

public func sendNSEProcessMessage(_ message: NSEProcessMessage) {
    SharedFileSubscriber.notify(url: nseMessagesSharedFile, message: ProcessMessage(message: message))
}

public func appMessageSubscriber(onMessage: @escaping (AppProcessMessage) -> Void) -> AppSubscriber {
    SharedFileSubscriber(fileURL: appMessagesSharedFile) { (msg: ProcessMessage<AppProcessMessage>) in
        onMessage(msg.message)
    }
}

public func nseMessageSubscriber(onMessage: @escaping (NSEProcessMessage) -> Void) -> NSESubscriber {
    SharedFileSubscriber(fileURL: nseMessagesSharedFile) { (msg: ProcessMessage<NSEProcessMessage>) in
        onMessage(msg.message)
    }
}

public func sendAppState(_ state: AppState) {
    sendAppProcessMessage(.state(state: state))
}

public func sendNSEState(_ state: NSEState) {
    sendNSEProcessMessage(.state(state: state))
}
