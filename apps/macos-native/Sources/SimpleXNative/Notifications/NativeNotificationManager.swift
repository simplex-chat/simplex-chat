import AppKit
import Foundation
@preconcurrency import UserNotifications

@MainActor
final class NativeNotificationManager: NSObject, ObservableObject, UNUserNotificationCenterDelegate {
    @Published private(set) var permissionState: NotificationPermissionState = .unknown
    @Published var showingPermissionExplanation = false
    @Published var previewMode: NotificationPreviewMode {
        didSet { UserDefaults.standard.set(previewMode.rawValue, forKey: Self.previewModeKey) }
    }
    @Published var soundsEnabled: Bool {
        didSet { UserDefaults.standard.set(soundsEnabled, forKey: Self.soundsKey) }
    }

    weak var model: AppModel?

    private let center = UNUserNotificationCenter.current()
    private static let previewModeKey = "nativeNotificationPreviewMode"
    private static let soundsKey = "nativeNotificationSounds"
    private static let explanationKey = "nativeNotificationExplanationShown"

    override init() {
        previewMode = NotificationPreviewMode(
            rawValue: UserDefaults.standard.string(forKey: Self.previewModeKey) ?? ""
        ) ?? .message
        if UserDefaults.standard.object(forKey: Self.soundsKey) == nil {
            soundsEnabled = true
        } else {
            soundsEnabled = UserDefaults.standard.bool(forKey: Self.soundsKey)
        }
        super.init()
        center.delegate = self
        registerCategories()
        refreshPermissionState()
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(windowBecameFocused),
            name: NSWindow.didBecomeKeyNotification,
            object: nil
        )
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(applicationBecameActive),
            name: NSApplication.didBecomeActiveNotification,
            object: nil
        )
    }

    func chatSetupReady() {
        center.getNotificationSettings { [weak self] settings in
            Task { @MainActor in
                guard let self else { return }
                self.permissionState = self.permissionState(for: settings.authorizationStatus)
                if self.permissionState == .notDetermined,
                   !UserDefaults.standard.bool(forKey: Self.explanationKey) {
                    self.showingPermissionExplanation = true
                }
            }
        }
    }

    func respondToPermissionExplanation(requestPermission: Bool) {
        UserDefaults.standard.set(true, forKey: Self.explanationKey)
        showingPermissionExplanation = false
        guard requestPermission else { return }
        Task {
            do {
                _ = try await center.requestAuthorization(options: [.alert, .badge, .sound])
            } catch {
                permissionState = .denied
            }
            refreshPermissionState()
        }
    }

    func handleCoreEvent(_ data: Data) {
        guard permissionState == .authorized || permissionState == .provisional,
              let payload = NativeNotificationParser.payload(from: data),
              let model else { return }
        let windowFocused = NSApp.isActive && NSApp.keyWindow?.isKeyWindow == true
        guard !NativeNotificationParser.shouldSuppress(
            windowFocused: windowFocused,
            activeUserID: model.profile?.userID,
            activeRemoteHostID: nil,
            activeChatID: model.selectedChatID,
            route: payload.route
        ) else {
            return
        }

        let content = UNMutableNotificationContent()
        let preview = NativeNotificationParser.preview(for: payload, mode: previewMode)
        content.title = preview.title
        content.body = preview.body
        content.categoryIdentifier = payload.category.rawValue
        content.sound = soundsEnabled ? .default : nil
        content.userInfo = routeDictionary(payload.route)

        center.add(UNNotificationRequest(
            identifier: payload.route.identifier,
            content: content,
            trigger: nil
        ))
    }

    func removeDeliveredNotifications(chatID: String) {
        center.getDeliveredNotifications { [center] notifications in
            let identifiers = notifications.compactMap { notification -> String? in
                notification.request.content.userInfo["chatID"] as? String == chatID
                    ? notification.request.identifier
                    : nil
            }
            center.removeDeliveredNotifications(withIdentifiers: identifiers)
            center.removePendingNotificationRequests(withIdentifiers: identifiers)
        }
    }

    func openSystemSettings() {
        guard let url = URL(string: "x-apple.systempreferences:com.apple.Notifications-Settings.extension") else { return }
        NSWorkspace.shared.open(url)
    }

    @objc private func windowBecameFocused() {
        model?.markSelectedChatReadIfVisible()
    }

    @objc private func applicationBecameActive() {
        model?.markSelectedChatReadIfVisible()
    }

    nonisolated func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        willPresent notification: UNNotification
    ) async -> UNNotificationPresentationOptions {
        await MainActor.run {
            guard let model,
                  let route = route(from: notification.request.content.userInfo) else {
                return [.banner, .sound]
            }
            let focused = NSApp.isActive && NSApp.keyWindow?.isKeyWindow == true
            if NativeNotificationParser.shouldSuppress(
                windowFocused: focused,
                activeUserID: model.profile?.userID,
                activeRemoteHostID: nil,
                activeChatID: model.selectedChatID,
                route: route
            ) {
                return []
            }
            return soundsEnabled ? [.banner, .sound] : [.banner]
        }
    }

    nonisolated func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        didReceive response: UNNotificationResponse
    ) async {
        await MainActor.run {
            guard let route = route(from: response.notification.request.content.userInfo) else { return }
            NSApp.activate(ignoringOtherApps: true)
            model?.openNotificationRoute(route)
        }
    }

    private func registerCategories() {
        let open = UNNotificationAction(identifier: "SIMPLEX_OPEN", title: "Open", options: [.foreground])
        center.setNotificationCategories([
            UNNotificationCategory(
                identifier: DesktopNotificationCategory.message.rawValue,
                actions: [open],
                intentIdentifiers: []
            ),
            UNNotificationCategory(
                identifier: DesktopNotificationCategory.contactRequest.rawValue,
                actions: [open],
                intentIdentifiers: []
            ),
            UNNotificationCategory(
                identifier: DesktopNotificationCategory.incomingCall.rawValue,
                actions: [open],
                intentIdentifiers: []
            ),
        ])
    }

    private func refreshPermissionState() {
        center.getNotificationSettings { [weak self] settings in
            Task { @MainActor in
                self?.permissionState = self?.permissionState(for: settings.authorizationStatus) ?? .unknown
            }
        }
    }

    private func permissionState(for status: UNAuthorizationStatus) -> NotificationPermissionState {
        switch status {
        case .notDetermined: .notDetermined
        case .denied: .denied
        case .authorized: .authorized
        case .provisional: .provisional
        case .ephemeral: .unknown
        @unknown default: .unknown
        }
    }

    private func routeDictionary(_ route: NotificationRoute) -> [AnyHashable: Any] {
        var dictionary: [AnyHashable: Any] = ["chatID": route.chatID]
        if let userID = route.userID { dictionary["userID"] = userID }
        if let remoteHostID = route.remoteHostID { dictionary["remoteHostID"] = remoteHostID }
        if let messageID = route.messageID { dictionary["messageID"] = messageID }
        return dictionary
    }

    private func route(from dictionary: [AnyHashable: Any]) -> NotificationRoute? {
        guard let chatID = dictionary["chatID"] as? String else { return nil }
        return NotificationRoute(
            userID: (dictionary["userID"] as? NSNumber)?.int64Value,
            remoteHostID: (dictionary["remoteHostID"] as? NSNumber)?.int64Value,
            chatID: chatID,
            messageID: (dictionary["messageID"] as? NSNumber)?.int64Value
        )
    }
}
