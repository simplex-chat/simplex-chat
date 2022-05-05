//
//  NtfManager.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 08/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UserNotifications
import UIKit

let ntfActionAccept = "NTF_ACT_ACCEPT"

private let ntfTimeInterval: TimeInterval = 1

class NtfManager: NSObject, UNUserNotificationCenterDelegate, ObservableObject {
    static let shared = NtfManager()

    private var granted = false
    private var prevNtfTime: Dictionary<ChatId, Date> = [:]


    // Handle notification when app is in background
    func userNotificationCenter(_ center: UNUserNotificationCenter,
                                didReceive response: UNNotificationResponse,
                                withCompletionHandler handler: () -> Void) {
        logger.debug("NtfManager.userNotificationCenter: didReceive")
        let content = response.notification.request.content
        let chatModel = ChatModel.shared
        if content.categoryIdentifier == ntfCategoryContactRequest && response.actionIdentifier == ntfActionAccept,
           let chatId = content.userInfo["chatId"] as? String,
           case let .contactRequest(contactRequest) = chatModel.getChat(chatId)?.chatInfo {
            Task { await acceptContactRequest(contactRequest) }
        } else {
            chatModel.chatId = content.targetContentIdentifier
        }
        handler()
    }

    // Handle notification when the app is in foreground
    func userNotificationCenter(_ center: UNUserNotificationCenter,
                                willPresent notification: UNNotification,
                                withCompletionHandler handler: (UNNotificationPresentationOptions) -> Void) {
        logger.debug("NtfManager.userNotificationCenter: willPresent")
        handler(presentationOptions(notification.request.content))
    }

    private func presentationOptions(_ content: UNNotificationContent) -> UNNotificationPresentationOptions {
        let model = ChatModel.shared
        if UIApplication.shared.applicationState == .active {
            switch content.categoryIdentifier {
            case ntfCategoryMessageReceived:
                if model.chatId == nil {
                    // in the chat list
                    return recentInTheSameChat(content) ? [] : [.sound, .list]
                } else if model.chatId == content.targetContentIdentifier {
                    // in the current chat
                    return recentInTheSameChat(content) ? [] : [.sound, .list]
                } else {
                    // in another chat
                    return recentInTheSameChat(content) ? [.banner, .list] : [.sound, .banner, .list]
                }
            // this notification is deliverd from the notifications server
            // when the app is in foreground it does not need to be shown
            case ntfCategoryCheckMessage: return []
            default: return [.sound, .banner, .list]
            }
        } else {
            return content.categoryIdentifier == ntfCategoryCheckMessage ? [] : [.sound, .banner, .list]
        }
    }

    private func recentInTheSameChat(_ content: UNNotificationContent) -> Bool {
        let now = Date.now
        if let chatId = content.targetContentIdentifier {
            var res: Bool = false
            if let t = prevNtfTime[chatId] { res = t.distance(to: now) < 30 }
            prevNtfTime[chatId] = now
            return res
        }
        return false
    }

    func registerCategories() {
        logger.debug("NtfManager.registerCategories")
        UNUserNotificationCenter.current().setNotificationCategories([
            UNNotificationCategory(
                identifier: ntfCategoryContactRequest,
                actions: [UNNotificationAction(
                    identifier: ntfActionAccept,
                    title: NSLocalizedString("Accept", comment: "accept contact request via notification")
                )],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: NSLocalizedString("New contact request", comment: "notification")
            ),
            UNNotificationCategory(
                identifier: ntfCategoryContactConnected,
                actions: [],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: NSLocalizedString("Contact is connected", comment: "notification")
            ),
            UNNotificationCategory(
                identifier: ntfCategoryMessageReceived,
                actions: [],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: NSLocalizedString("New message", comment: "notification")
            ),
            // TODO remove
            UNNotificationCategory(
                identifier: ntfCategoryCheckingMessages,
                actions: [],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: NSLocalizedString("Checking new messages...", comment: "notification")
            )
        ])
    }

    func requestAuthorization(onDeny handler: (()-> Void)? = nil) {
        logger.debug("NtfManager.requestAuthorization")
        let center = UNUserNotificationCenter.current()
        center.getNotificationSettings { settings in
            switch settings.authorizationStatus {
            case .denied:
                if let handler = handler { handler() }
                return
            case .authorized:
                self.granted = true
            default:
                center.requestAuthorization(options: [.alert, .sound, .badge]) { granted, error in
                    if let error = error {
                        logger.error("NtfManager.requestAuthorization error \(error.localizedDescription)")
                    } else {
                        self.granted = granted
                    }
                }
            }
        }
        center.delegate = self
    }

    func notifyContactRequest(_ contactRequest: UserContactRequest) {
        logger.debug("NtfManager.notifyContactRequest")
        addNotification(createContactRequestNtf(contactRequest))
    }

    func notifyContactConnected(_ contact: Contact) {
        logger.debug("NtfManager.notifyContactConnected")
        addNotification(createContactConnectedNtf(contact))
    }

    func notifyMessageReceived(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        logger.debug("NtfManager.notifyMessageReceived")
        addNotification(createMessageReceivedNtf(cInfo, cItem))
    }

    // TODO remove
    func notifyCheckingMessages() {
        logger.debug("NtfManager.notifyCheckingMessages")
        let content = createNotification(
            categoryIdentifier: ntfCategoryCheckingMessages,
            title: NSLocalizedString("Checking new messages...", comment: "notification")
        )
        addNotification(content)
    }

    private func addNotification(_ content: UNMutableNotificationContent) {
        if !granted { return }
        let trigger = UNTimeIntervalNotificationTrigger(timeInterval: ntfTimeInterval, repeats: false)
        let request = UNNotificationRequest(identifier: appNotificationId, content: content, trigger: trigger)
        UNUserNotificationCenter.current().add(request) { error in
            if let error = error { logger.error("addNotification error: \(error.localizedDescription)") }
        }
    }

    func removeNotifications(_ ids : [String]){
        UNUserNotificationCenter.current().removeDeliveredNotifications(withIdentifiers: ids)
        UNUserNotificationCenter.current().removePendingNotificationRequests(withIdentifiers: ids)
    }
}
