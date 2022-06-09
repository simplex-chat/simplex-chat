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
import SimpleXChat
import SimpleXServiceProtocol

import Foundation

let logger = Logger()

let machMessenger = MachMessenger(NSE_MACH_PORT, callback: receivedAppMachMessage)

class NotificationService: UNNotificationServiceExtension {
    var contentHandler: ((UNNotificationContent) -> Void)?
    var bestAttemptContent: UNMutableNotificationContent?

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        testFPService()

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
        if let _ = startChat() {
            let content = receiveMessages()
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

func startChat() -> User? {
    hs_init(0, nil)
    if let user = apiGetActiveUser() {
        logger.debug("active user \(String(describing: user))")
        do {
            try apiStartChat()
            try apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
            return user
        } catch {
            logger.error("NotificationService startChat error: \(responseError(error), privacy: .public)")
        }
    } else {
        logger.debug("no active user")
    }
    return nil
}

func receiveMessages() -> UNNotificationContent {
    logger.debug("NotificationService receiveMessages started")
    while true {
        let res = chatResponse(chat_recv_msg(getChatCtrl())!)
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

func apiGetActiveUser() -> User? {
    let _ = getChatCtrl()
    let r = sendSimpleXCmd(.showActiveUser)
    logger.debug("apiGetActiveUser sendSimpleXCmd responce: \(String(describing: r))")
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
    switch r {
    case .chatStarted: return
    case .chatRunning: return
    default: throw r
    }
}

func apiSetFilesFolder(filesFolder: String) throws {
    let r = sendSimpleXCmd(.setFilesFolder(filesFolder: filesFolder))
    if case .cmdOk = r { return }
    throw r
}





func testFPService() {
    logger.debug("testFPService get services")
    let manager = NSFileProviderManager.default
    // TODO try access file
    logger.debug("testFPService NSFileProviderManager.documentStorageURL \(manager.documentStorageURL, privacy: .public)")

//    let res = machMessenger.sendMessageWithReply(FPS_MACH_PORT, msg: "machMessenger before getFileProviderServicesForItem")
//    print("reply 1", res)

    FileManager.default.getFileProviderServicesForItem(at: URL(string: "\(manager.documentStorageURL)123")!) { (services, error) in
//        let res = machMessenger.sendMessageWithReply(FPS_MACH_PORT, msg: "machMessenger after getFileProviderServicesForItem")
//        print("reply 2", res)

        // Check to see if an error occurred.
        guard error == nil else {
            logger.debug("testFPService error getting service")
            print(error!) // <-- this prints the error I posted
            // Handle the error here...
            return
        }

        if let desiredService = services?[SIMPLEX_SERVICE_NAME] {
            logger.debug("testFPService has desiredService")

            // The named service is available for the item at the provided URL.
            // To use the service, get the connection object.
            desiredService.getFileProviderConnection(completionHandler: { (connectionOrNil, connectionError) in

                guard connectionError == nil else {
                    // Handle the error here...
                    return
                }

                guard let connection = connectionOrNil else {
                    // No connection object found.
                    return
                }

                // Set the remote interface.
                connection.remoteObjectInterface = simpleXServiceInterface

                // Start the connection.
                connection.resume()

                // Get the proxy object.
                let rawProxy = connection.remoteObjectProxyWithErrorHandler({ (errorAccessingRemoteObject) in
                    // Handle the error here...
                })

                // Cast the proxy object to the interface's protocol.
                guard let proxy = rawProxy as? SimpleXFPServiceProtocol else {
                    // If the interface is set up properly, this should never fail.
                    fatalError("*** Unable to cast \(rawProxy) to a DesiredProtocol instance ***")
                }

                logger.debug("testFPService calling service")
                proxy.upperCaseString("hello to service", withReply: { reply in
                    logger.debug("testFPService reply from service \(reply, privacy: .public)")
                })
            })
        }
    }
}
