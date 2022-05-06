//
//  Notifications.swift
//  SimpleX
//
//  Created by Evgeny on 28/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UserNotifications

let ntfCategoryContactRequest = "NTF_CAT_CONTACT_REQUEST"
let ntfCategoryContactConnected = "NTF_CAT_CONTACT_CONNECTED"
let ntfCategoryMessageReceived = "NTF_CAT_MESSAGE_RECEIVED"
let ntfCategoryCheckMessage = "NTF_CAT_CHECK_MESSAGE"
// TODO remove
let ntfCategoryCheckingMessages = "NTF_CAT_CHECKING_MESSAGES"

let appNotificationId = "chat.simplex.app.notification"

func createContactRequestNtf(_ contactRequest: UserContactRequest) -> UNMutableNotificationContent {
    createNotification(
        categoryIdentifier: ntfCategoryContactRequest,
        title: String.localizedStringWithFormat(NSLocalizedString("%@ wants to connect!", comment: "notification title"), contactRequest.displayName),
        body: String.localizedStringWithFormat(NSLocalizedString("Accept contact request from %@?", comment: "notification body"), contactRequest.chatViewName),
        targetContentIdentifier: nil,
        userInfo: ["chatId": contactRequest.id, "contactRequestId": contactRequest.apiId]
    )
}

func createContactConnectedNtf(_ contact: Contact) -> UNMutableNotificationContent {
    createNotification(
        categoryIdentifier: ntfCategoryContactConnected,
        title: String.localizedStringWithFormat(NSLocalizedString("%@ is connected!", comment: "notification title"), contact.displayName),
        body: String.localizedStringWithFormat(NSLocalizedString("You can now send messages to %@", comment: "notification body"), contact.chatViewName),
        targetContentIdentifier: contact.id
//            userInfo: ["chatId": contact.id, "contactId": contact.apiId]
    )
}

func createMessageReceivedNtf(_ cInfo: ChatInfo, _ cItem: ChatItem) -> UNMutableNotificationContent {
    createNotification(
        categoryIdentifier: ntfCategoryMessageReceived,
        title: "\(cInfo.chatViewName):",
        body: hideSecrets(cItem),
        targetContentIdentifier: cInfo.id
//            userInfo: ["chatId": cInfo.id, "chatItemId": cItem.id]
    )
}

func createNotification(categoryIdentifier: String, title: String, subtitle: String? = nil, body: String? = nil,
                        targetContentIdentifier: String? = nil, userInfo: [AnyHashable : Any] = [:]) -> UNMutableNotificationContent {
    let content = UNMutableNotificationContent()
    content.categoryIdentifier = categoryIdentifier
    content.title = title
    if let s = subtitle { content.subtitle = s }
    if let s = body { content.body = s }
    content.targetContentIdentifier = targetContentIdentifier
    content.userInfo = userInfo
    // TODO move logic of adding sound here, so it applies to background notifications too
    content.sound = .default
//        content.interruptionLevel = .active
//        content.relevanceScore = 0.5 // 0-1
    return content
}

func hideSecrets(_ cItem: ChatItem) -> String {
    if cItem.content.text != "" {
        if let md = cItem.formattedText {
            var res = ""
            for ft in md {
                if case .secret = ft.format {
                    res = res + "..."
                } else {
                    res = res + ft.text
                }
            }
            return res
        } else {
            return cItem.content.text
        }
    } else {
        return cItem.file?.fileName ?? ""
    }
}
