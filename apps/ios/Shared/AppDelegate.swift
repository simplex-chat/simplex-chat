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
import SwiftUI

class AppDelegate: NSObject, UIApplicationDelegate {
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey : Any]? = nil) -> Bool {
        logger.debug("AppDelegate: didFinishLaunchingWithOptions")
        application.registerForRemoteNotifications()
        if #available(iOS 17.0, *) { trackKeyboard() }
        NotificationCenter.default.addObserver(self, selector: #selector(pasteboardChanged), name: UIPasteboard.changedNotification, object: nil)
        removePasscodesIfReinstalled()
        prepareForLaunch()
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

    @objc func pasteboardChanged() {
        ChatModel.shared.pasteboardHasStrings = UIPasteboard.general.hasStrings
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

    private func removePasscodesIfReinstalled() {
        // Check for the database existence, because app and self destruct passcodes
        // will be saved and restored by iOS when a user deletes and re-installs the app.
        // In this case the database and settings will be deleted, but the passcodes won't be.
        // Deleting passcodes ensures that the user will not get stuck on "Opening app..." screen.
        if (kcAppPassword.get() != nil || kcSelfDestructPassword.get() != nil) &&
           !UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA) && !hasDatabase() {
            _ = kcAppPassword.remove()
            _ = kcSelfDestructPassword.remove()
            _ = kcDatabasePassword.remove()
        }
    }

    private func prepareForLaunch() {
        try? FileManager.default.createDirectory(at: getWallpaperDirectory(), withIntermediateDirectories: true)
    }

    static func keepScreenOn(_ on: Bool) {
        UIApplication.shared.isIdleTimerDisabled = on
    }
}

class SceneDelegate: NSObject, ObservableObject, UIWindowSceneDelegate {
    var window: UIWindow?
    static var windowStatic: UIWindow?
    var windowScene: UIWindowScene?

    func scene(_ scene: UIScene, willConnectTo session: UISceneSession, options connectionOptions: UIScene.ConnectionOptions) {
        UITableView.appearance().backgroundColor = .clear
        guard let windowScene = scene as? UIWindowScene else { return }
        self.windowScene = windowScene
        window = windowScene.keyWindow
        SceneDelegate.windowStatic = windowScene.keyWindow
        runMigrations()
        ThemeManager.applyTheme(currentThemeDefault.get())
        ThemeManager.adjustWindowStyle()
    }

    private func runMigrations() {
        /// For checking migration
//        themeOverridesDefault.set([])
//        currentThemeDefault.set(DefaultTheme.SYSTEM_THEME_NAME)
//        setUIAccentColorDefault(CGColor.init(red: 0.5, green: 0.3, blue: 0.8, alpha: 1))
//        setUserInterfaceStyleDefault(.unspecified)
//        lastMigratedBundleVersionDefault.set(0)

        let bundleVersion = Int(appBuild ?? "0")
        if let bundleVersion, lastMigratedBundleVersionDefault.get() < bundleVersion {
            while true {
                if lastMigratedBundleVersionDefault.get() < 225 {
                    let userInterfaceStyle = getUserInterfaceStyleDefault()
                    let defaultAccentColor = Color(cgColor: CGColor(red: 0.000, green: 0.533, blue: 1.000, alpha: 1))
                    let accentColor = Color(cgColor: getUIAccentColorDefault())
                    let isAccentChanged = accentColor != defaultAccentColor
                    let colors = ThemeColors(primary: isAccentChanged ? accentColor.toReadableHex() : nil)
                    var overrides = themeOverridesDefault.get()
                    var themeIds = currentThemeIdsDefault.get()
                    if isAccentChanged {
                        switch userInterfaceStyle {
                        case .light:
                            let light = ThemeOverrides(base: DefaultTheme.LIGHT, colors: colors, wallpaper: ThemeWallpaper(preset: PresetWallpaper.school.filename))
                            overrides.append(light)
                            themeOverridesDefault.set(overrides)
                            themeIds[DefaultTheme.LIGHT.themeName] = light.themeId
                            currentThemeIdsDefault.set(themeIds)
                            ThemeManager.applyTheme(DefaultTheme.LIGHT.themeName)
                        case .dark:
                            let dark = ThemeOverrides(base: DefaultTheme.DARK, colors: colors, wallpaper: ThemeWallpaper(preset: PresetWallpaper.school.filename))
                            overrides.append(dark)
                            themeOverridesDefault.set(overrides)
                            themeIds[DefaultTheme.DARK.themeName] = dark.themeId
                            currentThemeIdsDefault.set(themeIds)
                            ThemeManager.applyTheme(DefaultTheme.DARK.themeName)
                        case .unspecified:
                            let light = ThemeOverrides(base: DefaultTheme.LIGHT, colors: colors, wallpaper: ThemeWallpaper(preset: PresetWallpaper.school.filename))
                            let dark = ThemeOverrides(base: DefaultTheme.DARK, colors: colors, wallpaper: ThemeWallpaper(preset: PresetWallpaper.school.filename))
                            overrides.append(light)
                            overrides.append(dark)
                            themeOverridesDefault.set(overrides)
                            themeIds[DefaultTheme.LIGHT.themeName] = light.themeId
                            themeIds[DefaultTheme.DARK.themeName] = dark.themeId
                            currentThemeIdsDefault.set(themeIds)
                            ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
                        @unknown default: ()
                        }
                    } else {
                        let themeName = switch userInterfaceStyle {
                            case .light: DefaultTheme.LIGHT.themeName
                            case .dark: DefaultTheme.DARK.themeName
                            default: DefaultTheme.SYSTEM_THEME_NAME
                        }
                        ThemeManager.applyTheme(themeName)
                    }
                    lastMigratedBundleVersionDefault.set(225)
                } else {
                    lastMigratedBundleVersionDefault.set(bundleVersion)
                    break
                }
            }
        }
    }
}
