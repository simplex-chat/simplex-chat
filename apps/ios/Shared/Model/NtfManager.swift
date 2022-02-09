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

let ntfCategoryContactRequest = "NTF_CAT_CONTACT_REQUEST"
let ntfCategoryContactConnected = "NTF_CAT_CONTACT_CONNECTED"
let ntfCategoryMessageReceived = "NTF_CAT_MESSAGE_RECEIVED"

let appNotificationId = "chat.simplex.app.notification"

private let ntfTimeInterval: TimeInterval = 2

class NtfManager: NSObject, UNUserNotificationCenterDelegate, ObservableObject {
    static let shared = NtfManager()

    static func internalPublisher(_ categoryIdentifier: String) -> NotificationCenter.Publisher {
        let name = Notification.Name("\(appNotificationId).\(categoryIdentifier)")
        return NotificationCenter.default.publisher(for: name)
    }

    static func internalPost(_ response: UNNotificationResponse) {
        let name = Notification.Name("\(appNotificationId).\(response.notification.request.content.categoryIdentifier)")
        NotificationCenter.default.post(name: name , object: response)
    }

    private var granted = false
    private var chatModel: ChatModel?

    func setModel(_ chatModel: ChatModel) {
        self.chatModel = chatModel
    }

    // Handle notification when app is in background
    func userNotificationCenter(_ center: UNUserNotificationCenter,
                                didReceive response: UNNotificationResponse,
                                withCompletionHandler handler: () -> Void) {
        let content = response.notification.request.content
        if content.categoryIdentifier == ntfCategoryContactRequest {
            if response.actionIdentifier == ntfActionAccept,
               let contactRequestId = content.userInfo["contactRequestId"] as? Int64,
               let chatId = content.userInfo["chatId"] as? String,
               let model = self.chatModel {
                do {
                    let contact = try apiAcceptContactRequest(contactReqId: contactRequestId)
                    let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
                    model.replaceChat(chatId, chat)
                } catch let error {
                    print("apiAcceptContactRequest error: \(error)")
                }
            } else {
                NtfManager.internalPost(response)
            }
        }

        if let model = self.chatModel {
            model.chatId = content.targetContentIdentifier
        }

        handler()
    }

    // Handle notification when the app is in foreground
    func userNotificationCenter(_ center: UNUserNotificationCenter,
                                willPresent notification: UNNotification,
                                withCompletionHandler handler: (UNNotificationPresentationOptions) -> Void) {
        let content = notification.request.content
        if let model = self.chatModel {
            if content.categoryIdentifier == ntfCategoryContactRequest || model.chatId != content.targetContentIdentifier {
                handler([.sound, .banner, .list])
            } else {
                handler([.sound, .list])
            }
        } else {
            handler([.sound, .banner, .list])
        }
    }

    func registerCategories() {
        UNUserNotificationCenter.current().setNotificationCategories([
            UNNotificationCategory(
                identifier: ntfCategoryContactRequest,
                actions: [UNNotificationAction(
                    identifier: ntfActionAccept,
                    title: "Accept"
                )],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: "New contact request"
            ),
            UNNotificationCategory(
                identifier: ntfCategoryContactConnected,
                actions: [],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: "Contact is connected"
            ),
            UNNotificationCategory(
                identifier: ntfCategoryMessageReceived,
                actions: [],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: "New message"
            )
        ])
    }

    func requestPermission(onDeny handler: (()-> Void)? = nil) {
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
                        print("requestAuthorization error \(error)")
                    } else {
                        self.granted = granted
                    }
                }
            }
        }
        center.delegate = self
    }

    func notifyContactRequest(_ contactRequest: UserContactRequest) {
        addNotification(
            categoryIdentifier: ntfCategoryContactRequest,
            title: "\(contactRequest.displayName) wants to connect!",
            body: "Accept contact request from \(contactRequest.chatViewName)?",
            targetContentIdentifier: nil,
            userInfo: ["chatId": contactRequest.id, "contactRequestId": contactRequest.apiId]
        )
    }

    func notifyContactConnected(_ contact: Contact) {
        addNotification(
            categoryIdentifier: ntfCategoryContactConnected,
            title: "\(contact.displayName) is connected!",
            body: "You can now send messages to \(contact.chatViewName)",
            targetContentIdentifier: contact.id
//            userInfo: ["chatId": contact.id, "contactId": contact.apiId]
        )
    }

    func notifyMessageReceived(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        addNotification(
            categoryIdentifier: ntfCategoryMessageReceived,
            title: "\(cInfo.chatViewName):",
            body: cItem.content.text,
            targetContentIdentifier: cInfo.id
//            userInfo: ["chatId": cInfo.id, "chatItemId": cItem.id]
        )
    }

    private func addNotification(categoryIdentifier: String, title: String, subtitle: String? = nil, body: String? = nil,
                                 targetContentIdentifier: String? = nil, userInfo: [AnyHashable : Any] = [:]) {
        if !granted { return }
        let content = UNMutableNotificationContent()
        content.categoryIdentifier = categoryIdentifier
        content.title = title
        if let s = subtitle { content.subtitle = s }
        if let s = body { content.body = s }
        content.targetContentIdentifier = targetContentIdentifier
        content.userInfo = userInfo
        content.sound = .default
//        content.interruptionLevel = .active
//        content.relevanceScore = 0.5 // 0-1
        let trigger = UNTimeIntervalNotificationTrigger(timeInterval: ntfTimeInterval, repeats: false)
        let request = UNNotificationRequest(identifier: appNotificationId, content: content, trigger: trigger)
        UNUserNotificationCenter.current().add(request) { error in
            if let error = error { print("notification error: \(error)") }
        }
    }

    func removeNotifications(_ ids : [String]){
        UNUserNotificationCenter.current().removeDeliveredNotifications(withIdentifiers: ids)
        UNUserNotificationCenter.current().removePendingNotificationRequests(withIdentifiers: ids)
    }
}
