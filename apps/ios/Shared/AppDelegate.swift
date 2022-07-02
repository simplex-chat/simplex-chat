//
//  AppDelegate.swift
//  SimpleX
//
//  Created by Evgeny on 30/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit
import SimpleXChat

class AppDelegate: NSObject, UIApplicationDelegate {
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey : Any]? = nil) -> Bool {
        logger.debug("AppDelegate: didFinishLaunchingWithOptions")
        application.registerForRemoteNotifications()
        return true
    }

    func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        let token = deviceToken.map { String(format: "%02hhx", $0) }.joined()
        logger.debug("AppDelegate: didRegisterForRemoteNotificationsWithDeviceToken \(token)")
        let m = ChatModel.shared
        let deviceToken = DeviceToken(pushProvider: PushProvider(env: pushEnvironment), token: token)
        m.deviceToken = deviceToken
        if m.savedToken != nil {
            registerToken(token: deviceToken)
        }
    }

    func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        logger.error("AppDelegate: didFailToRegisterForRemoteNotificationsWithError \(error.localizedDescription)")
    }

    func application(_ application: UIApplication,
                     didReceiveRemoteNotification userInfo: [AnyHashable : Any],
                     fetchCompletionHandler completionHandler: @escaping (UIBackgroundFetchResult) -> Void) {
        logger.debug("AppDelegate: didReceiveRemoteNotification")
        print("*** userInfo", userInfo)
        let m = ChatModel.shared
        if let ntfData = userInfo["notificationData"] as? [AnyHashable : Any],
           m.notificationMode != .off {
            if let verification = ntfData["verification"] as? String,
               let nonce = ntfData["nonce"] as? String {
                if let token = ChatModel.shared.deviceToken {
                    logger.debug("AppDelegate: didReceiveRemoteNotification: verification, confirming \(verification)")
                    Task {
                        do {
                            if case .active = m.tokenStatus {} else { m.tokenStatus = .confirmed }
                            try await apiVerifyToken(token: token, nonce: nonce, code: verification)
                            m.tokenStatus = .active
                        } catch {
                            if let cr = error as? ChatResponse, case .chatCmdError(.errorAgent(.NTF(.AUTH))) = cr {
                                m.tokenStatus = .expired
                            }
                            logger.error("AppDelegate: didReceiveRemoteNotification: apiVerifyToken or apiIntervalNofication error: \(responseError(error))")
                        }
                        completionHandler(.newData)
                    }
                } else {
                    completionHandler(.noData)
                }
            } else if let checkMessages = ntfData["checkMessages"] as? Bool, checkMessages {
                logger.debug("AppDelegate: didReceiveRemoteNotification: checkMessages")
                if appStateGroupDefault.get().inactive {
                    receiveMessages(completionHandler)
                } else {
                    completionHandler(.noData)
                }
            } else {
                completionHandler(.noData)
            }
        } else {
            completionHandler(.noData)
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
