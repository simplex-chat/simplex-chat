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
        ChatModel.shared.deviceToken = deviceToken
        logger.debug("AppDelegate: didRegisterForRemoteNotificationsWithDeviceToken \(deviceToken.map { String(format: "%02hhx", $0) }.joined())")
    }

    func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        logger.error("AppDelegate: didFailToRegisterForRemoteNotificationsWithError \(error.localizedDescription)")
    }
}
