//
//  NotificationsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct NotificationsView: View {
    @EnvironmentObject var m: ChatModel
    @State private var notificationMode: NotificationsMode = ChatModel.shared.notificationMode
    @State private var showAlert: NotificationAlert?
    @State private var legacyDatabase = dbContainerGroupDefault.get() == .documents

    var body: some View {
        List {
            Section {
                NavigationLink {
                    List {
                        Section {
                            SelectionListView(list: NotificationsMode.values, selection: $notificationMode) { mode in
                                showAlert = .setMode(mode: mode)
                            }
                        } footer: {
                            VStack(alignment: .leading) {
                                Text(ntfModeDescription(notificationMode))
                            }
                            .font(.callout)
                            .padding(.top, 1)
                        }
                    }
                    .navigationTitle("Send notifications")
                    .navigationBarTitleDisplayMode(.inline)
                    .alert(item: $showAlert) { alert in
                        if let token = m.deviceToken {
                            return notificationAlert(alert, token)
                        } else {
                            return  Alert(title: Text("No device token!"))
                        }
                    }
                } label: {
                    HStack {
                        Text("Send notifications")
                        Spacer()
                        Text(m.notificationMode.label)
                    }
                }

                NavigationLink {
                    List {
                        Section {
                            SelectionListView(list: NotificationPreviewMode.values, selection: $m.notificationPreview) { previewMode in
                                ntfPreviewModeGroupDefault.set(previewMode)
                                m.notificationPreview = previewMode
                            }
                        } footer: {
                            VStack(alignment: .leading, spacing: 1) {
                                Text("You can set lock screen notification preview via settings.")
                                Button("Open Settings") {
                                    DispatchQueue.main.async {
                                        UIApplication.shared.open(URL(string: UIApplication.openSettingsURLString)!, options: [:], completionHandler: nil)
                                    }
                                }
                            }
                        }
                    }
                    .navigationTitle("Show preview")
                    .navigationBarTitleDisplayMode(.inline)
                } label: {
                    HStack {
                        Text("Show preview")
                        Spacer()
                        Text(m.notificationPreview.label)
                    }
                }
            } header: {
                Text("Push notifications")
            } footer: {
                if legacyDatabase {
                    Text("Please restart the app and migrate the database to enable push notifications.")
                        .font(.callout)
                        .padding(.top, 1)
                }
            }
        }
        .disabled(legacyDatabase)
    }

    private func notificationAlert(_ alert: NotificationAlert, _ token: DeviceToken) -> Alert {
        switch alert {
        case let .setMode(mode):
            return Alert(
                title: Text(ntfModeAlertTitle(mode)),
                message: Text(ntfModeDescription(mode)),
                primaryButton: .default(Text(mode == .off ? "Turn off" : "Enable")) {
                    setNotificationsMode(token, mode)
                },
                secondaryButton: .cancel() {
                    notificationMode = m.notificationMode
                }
            )
        case let .error(title, error):
            return Alert(title: Text(title), message: Text(error))
        }
    }

    private func ntfModeAlertTitle(_ mode: NotificationsMode) -> LocalizedStringKey {
        switch mode {
        case .off: return "Use only local notifications?"
        case .periodic: return "Enable periodic notifications?"
        case .instant: return "Enable instant notifications?"
        }
    }

    private func setNotificationsMode(_ token: DeviceToken, _ mode: NotificationsMode) {
        Task {
            switch mode {
            case .off:
                do {
                    try await apiDeleteToken(token: token)
                    await MainActor.run {
                        m.tokenStatus = .new
                        notificationMode = .off
                        m.notificationMode = .off
                    }
                } catch let error {
                    await MainActor.run {
                        let err = responseError(error)
                        logger.error("apiDeleteToken error: \(err)")
                        showAlert = .error(title: "Error deleting token", error: err)
                    }
                }
            default:
                do {
                    let status = try await apiRegisterToken(token: token, notificationMode: mode)
                    await MainActor.run {
                        m.tokenStatus = status
                        notificationMode = mode
                        m.notificationMode = mode
                    }
                } catch let error {
                    await MainActor.run {
                        let err = responseError(error)
                        logger.error("apiRegisterToken error: \(err)")
                        showAlert = .error(title: "Error enabling notifications", error: err)
                    }
                }
            }
        }
    }
}

func ntfModeDescription(_ mode: NotificationsMode) -> LocalizedStringKey {
    switch mode {
    case .off: return "**Most private**: do not use SimpleX Chat notifications server, check messages periodically in the background (depends on how often you use the app)."
    case .periodic: return "**More private**: check new messages every 20 minutes. Device token is shared with SimpleX Chat server, but not how many contacts or messages you have."
    case .instant: return "**Recommended**: device token and notifications are sent to SimpleX Chat notification server, but not the message content, size or who it is from."
    }
}

struct SelectionListView<Item: SelectableItem>: View {
    var list: [Item]
    @Binding var selection: Item
    var onSelection: ((Item) -> Void)?
    @State private var tapped: Item? = nil

    var body: some View {
        ForEach(list) { item in
            HStack {
                Text(item.label)
                Spacer()
                if selection == item {
                    Image(systemName: "checkmark")
                        .resizable().scaledToFit().frame(width: 16)
                        .foregroundColor(.accentColor)
                }
            }
            .contentShape(Rectangle())
            .listRowBackground(Color(uiColor: tapped == item ? .secondarySystemFill : .systemBackground))
            .onTapGesture {
                if selection == item { return }
                if let f = onSelection {
                    f(item)
                } else {
                    selection = item
                }
            }
            ._onButtonGesture { down in
                if down {
                    tapped = item
                } else {
                    tapped = nil
                }
            } perform: {}
        }
        .environment(\.editMode, .constant(.active))
    }
}

enum NotificationAlert: Identifiable {
    case setMode(mode: NotificationsMode)
    case error(title: LocalizedStringKey, error: String)

    var id: String {
        switch self {
        case let .setMode(mode): return "enable \(mode.rawValue)"
        case let .error(title, error): return "error \(title): \(error)"
        }
    }
}

struct NotificationsView_Previews: PreviewProvider {
    static var previews: some View {
        NotificationsView()
    }
}
