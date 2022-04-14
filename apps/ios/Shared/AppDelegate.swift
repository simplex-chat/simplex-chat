//
//  AppDelegate.swift
//  SimpleX
//
//  Created by Evgeny on 30/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit

class AppDelegate: NSObject, UIApplicationDelegate {
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey : Any]? = nil) -> Bool {
        logger.debug("AppDelegate: didFinishLaunchingWithOptions")
        application.registerForRemoteNotifications()
        return true
    }

    func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        let token = deviceToken.map { String(format: "%02hhx", $0) }.joined()
        ChatModel.shared.deviceToken = token
        logger.debug("AppDelegate: didRegisterForRemoteNotificationsWithDeviceToken \(token)")
    }

    func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        logger.error("AppDelegate: didFailToRegisterForRemoteNotificationsWithError \(error.localizedDescription)")
    }

    func application(_ application: UIApplication,
                     didReceiveRemoteNotification userInfo: [AnyHashable : Any],
                     fetchCompletionHandler completionHandler: @escaping (UIBackgroundFetchResult) -> Void) {
        logger.debug("AppDelegate: didReceiveRemoteNotification")
        print(userInfo)
        if let ntfData = userInfo["notificationData"] as? [AnyHashable : Any] {
            if let verification = ntfData["verification"] as? String {
                logger.debug("AppDelegate: didReceiveRemoteNotification: verification, confirming \(verification)")
                // TODO send to chat
                completionHandler(.newData)
            } else if let checkMessages = ntfData["checkMessages"] as? Bool, checkMessages {
                // TODO check if app in background
                logger.debug("AppDelegate: didReceiveRemoteNotification: checkMessages")
                receiveMessages(completionHandler)
            } else if let smpQueue = ntfData["checkMessage"] as? String {
                // TODO check if app in background
                logger.debug("AppDelegate: didReceiveRemoteNotification: checkMessage \(smpQueue)")
                receiveMessages(completionHandler)
            }
        }
    }

    private func receiveMessages(_ completionHandler: @escaping (UIBackgroundFetchResult) -> Void) {
        let complete = BGManager.shared.completionHandler {
            logger.debug("AppDelegate: completed BGManager.receiveMessages")
            completionHandler(.newData)
        }

        BGManager.shared.receiveMessages(complete)
    }
}
