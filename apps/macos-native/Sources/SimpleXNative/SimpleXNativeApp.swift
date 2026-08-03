import AppKit
import SwiftUI

@main
struct SimpleXNativeApp: App {
    @StateObject private var model: AppModel
    @StateObject private var notifications: NativeNotificationManager

    init() {
        let notifications = NativeNotificationManager()
        _notifications = StateObject(wrappedValue: notifications)
        _model = StateObject(wrappedValue: AppModel(notificationManager: notifications))
    }

    var body: some Scene {
        WindowGroup {
            RootView(model: model, notifications: notifications)
                .frame(minWidth: 760, minHeight: 520)
        }
        .defaultSize(width: 1120, height: 720)
        .windowToolbarStyle(.unifiedCompact)
        .commands {
            SidebarCommands()
            CommandGroup(replacing: .pasteboard) {
                Button("Cut") { sendFirstResponderAction("cut:") }
                    .keyboardShortcut("x")
                Button("Copy") {
                    if model.transcriptFocused {
                        model.copySelectedMessages()
                    } else {
                        sendFirstResponderAction("copy:")
                    }
                }
                    .keyboardShortcut("c")
                    .disabled(model.transcriptFocused && model.selectedMessageIDs.isEmpty)
                Button("Paste") { sendFirstResponderAction("paste:") }
                    .keyboardShortcut("v")
                Button("Select All") {
                    if model.transcriptFocused {
                        model.selectAllMessages()
                    } else {
                        sendFirstResponderAction("selectAll:")
                    }
                }
                .keyboardShortcut("a")
            }
            CommandGroup(after: .pasteboard) {
                Divider()
                Button("Find in Conversation") { model.beginConversationSearch() }
                    .keyboardShortcut("f")
                    .disabled(model.selectedChat == nil)
                Button("Search Chats") { model.sidebarSearchPresented = true }
                    .keyboardShortcut("k")
                    .disabled(model.phase != .ready)
            }
            CommandGroup(after: .textEditing) {
                Button("Reply to Selected Message") { model.replyToSelectedMessage() }
                    .keyboardShortcut("r", modifiers: [.command, .shift])
                    .disabled(!model.canReplyToSelectedMessage)
                Button("Delete Selected Messages") { model.requestDeleteSelectedMessages() }
                    .keyboardShortcut(.delete, modifiers: [])
                    .disabled(!model.transcriptFocused || !model.canDeleteSelectedMessages)
                Button("Cancel Current Action") { model.dismissNearestState() }
                    .keyboardShortcut(.escape, modifiers: [])
                    .disabled(!model.canDismissNearestState)
            }
            CommandMenu("Conversation") {
                Button("Refresh") { model.refresh() }
                    .keyboardShortcut("r")
                Divider()
                Picker("Density", selection: $model.density) {
                    ForEach(DesktopChatDensity.allCases) { density in
                        Text(density.title).tag(density)
                    }
                }
            }
        }

        Settings {
            Form {
                Section("Interface") {
                    LabeledContent("Appearance", value: "Follows macOS")
                    LabeledContent("Profile", value: model.profile?.displayName ?? "Locked")
                    Picker("Chat density", selection: $model.density) {
                        ForEach(DesktopChatDensity.allCases) { density in
                            Text(density.title).tag(density)
                        }
                    }
                    .pickerStyle(.radioGroup)
                }

                Section("Notifications") {
                    LabeledContent("Permission", value: notifications.permissionState.title)
                    Picker("Show previews", selection: $notifications.previewMode) {
                        ForEach(NotificationPreviewMode.allCases) { mode in
                            Text(mode.title).tag(mode)
                        }
                    }
                    Toggle("Play notification sounds", isOn: $notifications.soundsEnabled)
                    Button("Open Mac Notification Settings", action: notifications.openSystemSettings)
                }

                Section("Security") {
                    LabeledContent(
                        "Database passphrase",
                        value: keychainPassphraseStatus
                    )
                    Button("Forget Saved Passphrase", role: .destructive, action: model.forgetSavedPassphrase)
                        .disabled(!model.keychainPassphraseStorageAvailable || !model.hasStoredPassphrase)
                    if let message = model.keychainStatusMessage {
                        Text(message)
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                }
            }
            .formStyle(.grouped)
            .padding(20)
            .frame(width: 420)
        }
    }

    private func sendFirstResponderAction(_ name: String) {
        NSApp.sendAction(Selector(name), to: nil, from: nil)
    }

    private var keychainPassphraseStatus: String {
        guard model.keychainPassphraseStorageAvailable else {
            return "Unavailable in this development build"
        }
        return model.hasStoredPassphrase ? "Saved in Mac Keychain" : "Not saved"
    }
}
