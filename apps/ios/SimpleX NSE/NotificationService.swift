//
//  NotificationService.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import UserNotifications
import OSLog
import FileProvider
import SimpleXChatSDK
import SimpleXAppShared
import SimpleXServiceProtocol

import Foundation

let logger = Logger()

let machMessenger = MachMessenger(NSE_MACH_PORT, callback: receivedAppMachMessage)

class NotificationService: UNNotificationServiceExtension {
    var contentHandler: ((UNNotificationContent) -> Void)?
    var bestAttemptContent: UNMutableNotificationContent?

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("NotificationService.didReceive")
        machMessenger.start()
        let res = machMessenger.sendMessageWithReply(APP_MACH_PORT, msg: "starting NSE didReceive")
        logger.debug("MachMessenger \(String(describing: res), privacy: .public)")
        if getAppState() != .background {
            contentHandler(request.content)
            machMessenger.stop()
            return
        }
        logger.debug("NotificationService: app is in the background")
        self.contentHandler = contentHandler
        bestAttemptContent = (request.content.mutableCopy() as? UNMutableNotificationContent)
        Task {
            if let _ = await startChat() {
                let content = await receiveMessages()
                contentHandler (content)
                machMessenger.stop()
                return
            }

            if let bestAttemptContent = bestAttemptContent {
                // Modify the notification content here...
                bestAttemptContent.title = "\(bestAttemptContent.title) [modified]"

                contentHandler(bestAttemptContent)
            }
            machMessenger.stop()
        }
    }
    
    override func serviceExtensionTimeWillExpire() {
        logger.debug("NotificationService.serviceExtensionTimeWillExpire")
        // Called just before the extension will be terminated by the system.
        // Use this as an opportunity to deliver your "best attempt" at modified content, otherwise the original push payload will be used.
        if let contentHandler = contentHandler, let bestAttemptContent =  bestAttemptContent {
            contentHandler(bestAttemptContent)
        }
    }
}

func receivedAppMachMessage(msgId: Int32, msg: String) -> String? {
    logger.debug("MachMessenger: receivedAppMachMessage \"\(msg)\" from App, replying")
    return "reply from NSE to: \(msg)"
}

func startChat() async -> User? {
//    hs_init(0, nil)
    if let user = await apiGetActiveUser() {
        logger.debug("active user \(String(describing: user))")
        do {
            try await apiStartChat()
            try await apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
            return user
        } catch {
            logger.error("NotificationService startChat error: \(responseError(error), privacy: .public)")
        }
    } else {
        logger.debug("no active user")
    }
    return nil
}

func receiveMessages() async -> UNNotificationContent {
    logger.debug("NotificationService receiveMessages started")
    while true {
//        let res = chatResponse(chat_recv_msg(getChatCtrl())!)
        let res = await recvSimpleXMsg()
        logger.debug("NotificationService receiveMessages: \(res.responseType)")
        switch res {
//        case let .newContactConnection(connection):
//        case let .contactConnectionDeleted(connection):
        case let .contactConnected(contact):
            return createContactConnectedNtf(contact)
//        case let .contactConnecting(contact):
//            TODO profile update
        case let .receivedContactRequest(contactRequest):
            return createContactRequestNtf(contactRequest)
//        case let .contactUpdated(toContact):
//            TODO profile updated
        case let .newChatItem(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            return createMessageReceivedNtf(cInfo, cItem)
//        case let .chatItemUpdated(aChatItem):
//            TODO message updated
//            let cInfo = aChatItem.chatInfo
//            let cItem = aChatItem.chatItem
//            NtfManager.shared.notifyMessageReceived(cInfo, cItem)
//        case let .chatItemDeleted(_, toChatItem):
//            TODO message updated
//        case let .rcvFileComplete(aChatItem):
//            TODO file received?
//            let cInfo = aChatItem.chatInfo
//            let cItem = aChatItem.chatItem
//            NtfManager.shared.notifyMessageReceived(cInfo, cItem)
        default:
            logger.debug("NotificationService ignored event: \(res.responseType)")
        }
    }
}

func apiGetActiveUser() async -> User? {
//    let _ = getChatCtrl()
    let r = await sendSimpleXCmd(.showActiveUser)
    logger.debug("apiGetActiveUser sendSimpleXCmd responce: \(String(describing: r))")
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(.error(.noActiveUser)): return nil
    default:
        logger.error("NotificationService apiGetActiveUser unexpected response: \(String(describing: r))")
        return nil
    }
}

func apiStartChat() async throws {
    let r = await sendSimpleXCmd(.startChat)
    switch r {
    case .chatStarted: return
    case .chatRunning: return
    default: throw r
    }
}

func apiSetFilesFolder(filesFolder: String) async throws {
    let r = await sendSimpleXCmd(.setFilesFolder(filesFolder: filesFolder))
    if case .cmdOk = r { return }
    throw r
}
