//
//  NtfManager.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 08/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UserNotifications

enum NtfAction: String {
    case accept = "NTF_ACT_ACCEPT"
    case send = "NTF_ACT_SEND"
    case reply = "NTF_ACT_REPLY"
}

enum NtfCategory: String {
    case contactRequest = "NTF_CAT_CONTACT_REQUEST"
    case contactConnected = "NTF_CAT_CONTACT_CONNECTED"
    case messageReceived = "NTF_CAT_MESSAGE_RECEIVED"
}

private let messageNotificationId = "chat.simplex.app.message"

private let ntfTimeInterval: TimeInterval = 2

class NtfManager: NSObject, UNUserNotificationCenterDelegate, ObservableObject {
    static let shared = NtfManager()

    static var publisher: NotificationCenter.Publisher {
        get { NotificationCenter.default.publisher(for: Notification.Name(messageNotificationId)) }
    }

    var granted = false

    // Handle notification when app is in background
    func userNotificationCenter(_ center: UNUserNotificationCenter,
                                didReceive response: UNNotificationResponse,
                                withCompletionHandler handler: () -> Void) {
        print("userNotificationCenter 1")
        print(response)
        let name = Notification.Name(response.notification.request.identifier)
        NotificationCenter.default.post(name: name , object: response.notification.request.content)
        handler()
    }

    // Handle notification when the app is in foreground
    func userNotificationCenter(_ center: UNUserNotificationCenter,
                                willPresent notification: UNNotification,
                                withCompletionHandler handler: (UNNotificationPresentationOptions) -> Void) {
        print("userNotificationCenter 2")
        print(notification)
        let name = Notification.Name(notification.request.identifier)
        NotificationCenter.default.post(name: name, object: notification.request.content)
        handler(.sound)
    }

    func registerCategories() {
        let ntfAcceptAction = UNNotificationAction(
            identifier: NtfAction.accept.rawValue,
            title: "Accept"
        )
        let ntfSendAction = UNTextInputNotificationAction(
            identifier: NtfAction.send.rawValue,
            title: "Send",
            icon: UNNotificationActionIcon(systemImageName: "arrowshape.turn.up.left"),
            textInputButtonTitle: "Send",
            textInputPlaceholder: "Your message"
        )
        let ntfReplyAction = UNTextInputNotificationAction(
            identifier: NtfAction.reply.rawValue,
            title: "Reply",
            icon: UNNotificationActionIcon(systemImageName: "arrowshape.turn.up.left"),
            textInputButtonTitle: "Send",
            textInputPlaceholder: "Your message"
        )
        UNUserNotificationCenter.current().setNotificationCategories([
            UNNotificationCategory(
                identifier: NtfCategory.contactRequest.rawValue,
                actions: [ntfAcceptAction],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: "New contact request"
            //    options: .customDismissAction
            ),
            UNNotificationCategory(
                identifier: NtfCategory.contactConnected.rawValue,
                actions: [ntfSendAction],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: "Contact is connected"
            //    options: .customDismissAction
            ),
            UNNotificationCategory(
                identifier: NtfCategory.messageReceived.rawValue,
                actions: [ntfReplyAction],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: "New message"
            //    options: .customDismissAction
            )
        ])
    }

    func requestPermission(onDeny handler: (()-> Void)? = nil) {
        let center = UNUserNotificationCenter.current()
        center.getNotificationSettings { settings in
            print("getNotificationSettings")
            print(settings)

            switch settings.authorizationStatus {
            case .denied:
                if let handler = handler { handler() }
                return
            case .authorized:
                self.granted = true
            default:
                center.requestAuthorization(options: [.alert, .sound, .badge]) { granted, error in
                    if let error = error {
                        print("error handling \(error)")
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
            category: .contactRequest,
            title: "\(contactRequest.displayName) wants to connect!",
            body: "Accept contact request from \(contactRequest.chatViewName)?",
            targetContentIdentifier: contactRequest.id
        )
    }

    func notifyContactConnected(_ contact: Contact) {
        addNotification(
            category: .contactConnected,
            title: "\(contact.displayName) is connected!",
            body: "You can now send messages to \(contact.chatViewName)",
            targetContentIdentifier: contact.id
        )
    }

    func notifyMessageReceived(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        addNotification(
            category: .messageReceived,
            title: "\(cInfo.chatViewName):",
            body: cItem.content.text,
            targetContentIdentifier: cInfo.id,
            userInfo: ["chatItemId" : cItem.id]
        )
    }

    private func addNotification(category: NtfCategory, title: String, subtitle: String? = nil, body: String? = nil,
                                 targetContentIdentifier: String? = nil, userInfo: [AnyHashable : Any] = [:]) {
        if !granted { return }
        let content = UNMutableNotificationContent()
        content.categoryIdentifier = category.rawValue
        content.title = title
        if let s = subtitle { content.subtitle = s }
        if let s = body { content.body = s }
        content.targetContentIdentifier = targetContentIdentifier
        content.userInfo = userInfo
        content.sound = .default
//        content.interruptionLevel = .active
//        content.relevanceScore = 0.5 // 0-1
        let trigger = UNTimeIntervalNotificationTrigger(timeInterval: ntfTimeInterval, repeats: false)
        let request = UNNotificationRequest(identifier: messageNotificationId, content: content, trigger: trigger)
        UNUserNotificationCenter.current().add(request) { error in
            if let error = error { print("error scheduling notification: \(error)") }
        }
    }

    func removeNotifications(_ ids : [String]){
        UNUserNotificationCenter.current().removeDeliveredNotifications(withIdentifiers: ids)
        UNUserNotificationCenter.current().removePendingNotificationRequests(withIdentifiers: ids)
    }
}
