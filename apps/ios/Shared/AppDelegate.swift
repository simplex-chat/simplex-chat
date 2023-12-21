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
        if #available(iOS 17.0, *) { trackKeyboard() }
        return true
    }

    @available(iOS 17.0, *)
    private func trackKeyboard() {
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardWillShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }

    @available(iOS 17.0, *)
    @objc func keyboardWillShow(_ notification: Notification) {
        if let keyboardFrame: NSValue = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? NSValue {
            ChatModel.shared.keyboardHeight = keyboardFrame.cgRectValue.height
        }
    }

    @available(iOS 17.0, *)
    @objc func keyboardWillHide(_ notification: Notification) {
        ChatModel.shared.keyboardHeight = 0
    }

    func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        let token = deviceToken.map { String(format: "%02hhx", $0) }.joined()
        logger.debug("AppDelegate: didRegisterForRemoteNotificationsWithDeviceToken \(token)")
        let m = ChatModel.shared
        let deviceToken = DeviceToken(pushProvider: PushProvider(env: pushEnvironment), token: token)
        m.deviceToken = deviceToken
        // savedToken is set in startChat, when it is started before this method is called
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
        let m = ChatModel.shared
        if let ntfData = userInfo["notificationData"] as? [AnyHashable : Any],
           m.notificationMode != .off {
            if let verification = ntfData["verification"] as? String,
               let nonce = ntfData["nonce"] as? String {
                if let token = m.deviceToken {
                    logger.debug("AppDelegate: didReceiveRemoteNotification: verification, confirming \(verification)")
                    Task {
                        do {
                            if case .active = m.tokenStatus {} else { m.tokenStatus = .confirmed }
                            try await apiVerifyToken(token: token, nonce: nonce, code: verification)
                            m.tokenStatus = .active
                        } catch {
                            if let cr = error as? ChatResponse, case .chatCmdError(_, .errorAgent(.NTF(.AUTH))) = cr {
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
                if m.ntfEnablePeriodic && allowBackgroundRefresh() && BGManager.shared.lastRanLongAgo {
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

    func applicationWillTerminate(_ application: UIApplication) {
        logger.debug("DEBUGGING: AppDelegate: applicationWillTerminate")
        ChatModel.shared.filesToDelete.forEach {
            removeFile($0)
        }
        ChatModel.shared.filesToDelete = []
        terminateChat()
    }

    func application(_ application: UIApplication,
                     configurationForConnecting connectingSceneSession: UISceneSession,
                     options: UIScene.ConnectionOptions) -> UISceneConfiguration {
        let configuration = UISceneConfiguration(name: nil, sessionRole: connectingSceneSession.role)
        if connectingSceneSession.role == .windowApplication {
            configuration.delegateClass = SceneDelegate.self
        }
        return configuration
    }

    private func receiveMessages(_ completionHandler: @escaping (UIBackgroundFetchResult) -> Void) {
        let complete = BGManager.shared.completionHandler {
            logger.debug("AppDelegate: completed BGManager.receiveMessages")
            completionHandler(.newData)
        }

        BGManager.shared.receiveMessages(complete)
    }

    static func keepScreenOn(_ on: Bool) {
        UIApplication.shared.isIdleTimerDisabled = on
    }
}

class SceneDelegate: NSObject, ObservableObject, UIWindowSceneDelegate {
    var window: UIWindow?
    var windowScene: UIWindowScene?

    func scene(_ scene: UIScene, willConnectTo session: UISceneSession, options connectionOptions: UIScene.ConnectionOptions) {
        guard let windowScene = scene as? UIWindowScene else { return }
        self.windowScene = windowScene
        window = windowScene.keyWindow
        window?.tintColor = UIColor(cgColor: getUIAccentColorDefault())
        window?.overrideUserInterfaceStyle = getUserInterfaceStyleDefault()
    }
}
