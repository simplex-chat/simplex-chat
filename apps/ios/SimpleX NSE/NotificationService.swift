//
//  NotificationService.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import UserNotifications
import OSLog

let logger = Logger()

class NotificationService: UNNotificationServiceExtension {

    var contentHandler: ((UNNotificationContent) -> Void)?
    var bestAttemptContent: UNMutableNotificationContent?

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("NotificationService.didReceive")
        if getAppState() != .background {
            contentHandler(request.content)
            return
        }
        logger.debug("NotificationService: app is in the background")
        self.contentHandler = contentHandler
        bestAttemptContent = (request.content.mutableCopy() as? UNMutableNotificationContent)
        if let user = startChat() {
            receiveMessages()
        }

        if let bestAttemptContent = bestAttemptContent {
            // Modify the notification content here...
            bestAttemptContent.title = "\(bestAttemptContent.title) [modified]"
            
            contentHandler(bestAttemptContent)
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

func startChat() -> User? {
    hs_init(0, nil)
    if let user = apiGetActiveUser() {
        do {
            try apiStartChat()
            try apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
            return user
        } catch {
            logger.error("NotificationService startChat error: \(responseError(error))")
        }
    }
    return nil
}

func receiveMessages() -> UNNotificationContent? {
    logger.debug("NotificationService receiveMessages started")
    while true {
        let res = chatResponse(chat_recv_msg(getChatCtrl())!)
        logger.debug("NotificationService receiveMessages: \(res.responseType)")
        return nil
        switch res {
//        case let .newContactConnection(connection):
//        case let .contactConnectionDeleted(connection):

//        case let .contactConnected(contact):
//            NtfManager.shared.notifyContactConnected(contact)
//        case let .contactConnecting(contact):
//            m.updateContact(contact)
//            m.removeChat(contact.activeConn.id)
//        case let .receivedContactRequest(contactRequest):
//            m.addChat(Chat(
//                chatInfo: ChatInfo.contactRequest(contactRequest: contactRequest),
//                chatItems: []
//            ))
//            NtfManager.shared.notifyContactRequest(contactRequest)
//        case let .contactUpdated(toContact):
//            let cInfo = ChatInfo.direct(contact: toContact)
//            if m.hasChat(toContact.id) {
//                m.updateChatInfo(cInfo)
//            }
//        case let .contactsSubscribed(_, contactRefs):
//            updateContactsStatus(contactRefs, status: .connected)
//        case let .contactsDisconnected(_, contactRefs):
//            updateContactsStatus(contactRefs, status: .disconnected)
//        case let .contactSubError(contact, chatError):
//            processContactSubError(contact, chatError)
//        case let .contactSubSummary(contactSubscriptions):
//            for sub in contactSubscriptions {
//                if let err = sub.contactError {
//                    processContactSubError(sub.contact, err)
//                } else {
//                    m.updateContact(sub.contact)
//                    m.updateNetworkStatus(sub.contact.id, .connected)
//                }
//            }
//        case let .newChatItem(aChatItem):
//            let cInfo = aChatItem.chatInfo
//            let cItem = aChatItem.chatItem
//            m.addChatItem(cInfo, cItem)
//            if let file = cItem.file,
//               file.fileSize <= maxImageSize {
//                Task {
//                    do {
//                        try await receiveFile(fileId: file.fileId)
//                    } catch {
//                        logger.error("receiveFile error: \(error.localizedDescription)")
//                    }
//                }
//            }
//            NtfManager.shared.notifyMessageReceived(cInfo, cItem)
//        case let .chatItemStatusUpdated(aChatItem):
//            let cInfo = aChatItem.chatInfo
//            let cItem = aChatItem.chatItem
//            var res = false
//            if !cItem.isDeletedContent() {
//                res = m.upsertChatItem(cInfo, cItem)
//            }
//            if res {
//                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
//            } else if let endTask = m.messageDelivery[cItem.id] {
//                switch cItem.meta.itemStatus {
//                case .sndSent: endTask()
//                case .sndErrorAuth: endTask()
//                case .sndError: endTask()
//                default: break
//                }
//            }
//        case let .chatItemUpdated(aChatItem):
//            let cInfo = aChatItem.chatInfo
//            let cItem = aChatItem.chatItem
//            if m.upsertChatItem(cInfo, cItem) {
//                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
//            }
//        case let .chatItemDeleted(_, toChatItem):
//            let cInfo = toChatItem.chatInfo
//            let cItem = toChatItem.chatItem
//            if cItem.meta.itemDeleted {
//                m.removeChatItem(cInfo, cItem)
//            } else {
//                // currently only broadcast deletion of rcv message can be received, and only this case should happen
//                _ = m.upsertChatItem(cInfo, cItem)
//            }
//        case let .rcvFileComplete(aChatItem):
//            let cInfo = aChatItem.chatInfo
//            let cItem = aChatItem.chatItem
//            if m.upsertChatItem(cInfo, cItem) {
//                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
//            }
        default:
            logger.debug("unsupported event: \(res.responseType)")
        }
    }
}

func apiGetActiveUser() -> User? {
    let _ = getChatCtrl()
    let r = sendSimpleXCmd(.showActiveUser)
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(.error(.noActiveUser)): return nil
    default:
        logger.error("NotificationService apiGetActiveUser unexpected response: \(String(describing: r))")
        return nil
    }
}

func apiStartChat() throws {
    let r = sendSimpleXCmd(.startChat)
    if case .chatStarted = r { return }
    throw r
}

func apiSetFilesFolder(filesFolder: String) throws {
    let r = sendSimpleXCmd(.setFilesFolder(filesFolder: filesFolder))
    if case .cmdOk = r { return }
    throw r
}

