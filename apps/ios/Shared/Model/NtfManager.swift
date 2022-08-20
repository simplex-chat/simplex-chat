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
import SimpleXChat

let ntfActionAcceptContact = "NTF_ACT_ACCEPT_CONTACT"
let ntfActionAcceptCall = "NTF_ACT_ACCEPT_CALL"
let ntfActionRejectCall = "NTF_ACT_REJECT_CALL"

private let ntfTimeInterval: TimeInterval = 1

enum NtfCallAction {
    case accept
    case reject
}

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
        let action = response.actionIdentifier
        logger.debug("NtfManager.userNotificationCenter: didReceive: action \(action), categoryIdentifier \(content.categoryIdentifier)")
        if content.categoryIdentifier == ntfCategoryContactRequest && action == ntfActionAcceptContact,
           let chatId = content.userInfo["chatId"] as? String {
            if case let .contactRequest(contactRequest) = chatModel.getChat(chatId)?.chatInfo {
                Task { await acceptContactRequest(contactRequest) }
            } else {
                chatModel.ntfContactRequest = chatId
            }
        } else if let (chatId, ntfAction) = ntfCallAction(content, action) {
            if let invitation = chatModel.callInvitations.removeValue(forKey: chatId) {
                CallController.shared.callAction(invitation: invitation, action: ntfAction)
            } else {
                chatModel.ntfCallInvitationAction = (chatId, ntfAction)
            }
        } else {
            chatModel.chatId = content.targetContentIdentifier
        }
        handler()
    }

    private func ntfCallAction(_ content: UNNotificationContent, _ action: String) -> (ChatId, NtfCallAction)? {
        if content.categoryIdentifier == ntfCategoryCallInvitation,
           let chatId = content.userInfo["chatId"] as? String {
            if action == ntfActionAcceptCall {
                return (chatId, .accept)
            } else if action == ntfActionRejectCall {
                return (chatId, .reject)
            }
        }
        return nil
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
            case ntfCategoryCallInvitation: return []
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
                    identifier: ntfActionAcceptContact,
                    title: NSLocalizedString("Accept", comment: "accept contact request via notification"),
                    options: .foreground
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
            UNNotificationCategory(
                identifier: ntfCategoryCallInvitation,
                actions: [
                    UNNotificationAction(
                        identifier: ntfActionAcceptCall,
                        title: NSLocalizedString("Accept", comment: "accept incoming call via notification"),
                        options: .foreground
                    ),
                    UNNotificationAction(
                        identifier: ntfActionRejectCall,
                        title: NSLocalizedString("Reject", comment: "reject incoming call via notification")
                    )
                ],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: NSLocalizedString("Incoming call", comment: "notification")
            ),
            UNNotificationCategory(
                identifier: ntfCategoryConnectionEvent,
                actions: [],
                intentIdentifiers: [],
                hiddenPreviewsBodyPlaceholder: NSLocalizedString("SimpleX encrypted message or connection event", comment: "notification")
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
        if cInfo.ntfsEnabled {
            addNotification(createMessageReceivedNtf(cInfo, cItem))
        }
    }

    func notifyCallInvitation(_ invitation: RcvCallInvitation) {
        logger.debug("NtfManager.notifyCallInvitation")
        addNotification(createCallInvitationNtf(invitation))
    }

    func setNtfBadgeCount(_ count: Int) {
        UIApplication.shared.applicationIconBadgeNumber = count
        ntfBadgeCountGroupDefault.set(count)
    }

    func decNtfBadgeCount(by count: Int = 1) {
        setNtfBadgeCount(max(0, UIApplication.shared.applicationIconBadgeNumber - count))
    }

    func incNtfBadgeCount(by count: Int = 1) {
        setNtfBadgeCount(UIApplication.shared.applicationIconBadgeNumber + count)
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
