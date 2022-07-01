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
    @State private var notificationMode: NotificationsMode?
    @State private var showAlert: NotificationAlert?
    @State private var dbContainer: DBContainer = dbContainerGroupDefault.get()

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
                                if let mode = notificationMode {
                                    Text(ntfModeDescription(mode))
                                }
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
                    .onAppear { notificationMode = m.notificationMode }
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
                            SelectionListView(list: NotificationPreviewMode.values, selection: $m.notificationPreview)
                        } footer: {

                        }
                    }
                    .navigationTitle("Show preview")
                    .navigationBarTitleDisplayMode(.inline)
                } label: {
                    HStack {
                        Text("Show preview")
                        Spacer()
                        Text(m.notificationPreview?.label ?? "")
                    }
                }
            } header: {
                Text("Push notifications")
            } footer: {
                if dbContainer == .documents {
                    Text("Please restart the app and migrate database to enable notifications")
                }
            }
            .disabled(dbContainer == .documents)
        }
    }

    private func notificationAlert(_ alert: NotificationAlert, _ token: DeviceToken) -> Alert {
        switch alert {
        case let .setMode(mode):
            return Alert(
                title: Text(ntfModeAlertTitle(mode)),
                message: Text(ntfModeDescription(mode)),
                primaryButton: .default(Text(mode == .off ? "Turn off" : "Enable")) {
                    setNotificationsMode(mode, token)
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
        case .off: return "Turn off notifications?"
        case .periodic: return "Enable periodic notifications?"
        case .instant: return "Enable instant notifications?"
        }
    }

    private func setNotificationsMode(_ mode: NotificationsMode, _ token: DeviceToken) {
        Task {
            switch mode {
            case .off:
                do {
                    try await apiDeleteToken(token: token)
                    m.tokenStatus = .new
                    notificationMode = .off
                    m.notificationMode = .off
                } catch let error {
                    DispatchQueue.main.async {
                        let err = responseError(error)
                        logger.error("apiDeleteToken error: \(err)")
                        showAlert = .error(title: "Error deleting token", error: err)
                    }
                }
            default:
                do {
                    do {
                        m.tokenStatus = try await apiRegisterToken(token: token, notificationMode: mode)
                        notificationMode = mode
                        m.notificationMode = mode
                    } catch let error {
                        DispatchQueue.main.async {
                            let err = responseError(error)
                            logger.error("apiRegisterToken error: \(err)")
                            showAlert = .error(title: "Error enabling notifications", error: err)
                        }
                    }
                }
            }
        }
    }
}

func ntfModeDescription(_ mode: NotificationsMode) -> LocalizedStringKey {
    switch mode {
    case .off: return "**Maximum privacy**: push notifications are off.\nNo meta-data is shared with SimpleX Chat notification server."
    case .periodic: return "**High privacy**: new messages are checked every 20 minutes.\nYour device token is shared with SimpleX Chat notification server, but it cannot see how many connections you have or how many messages you receive."
    case .instant: return "**Medium privacy** (recommended): notifications are sent instantly.\nYour device token and notifications are sent to SimpleX Chat notification server, but it cannot access the message content, size or who is it from."
    }
}

struct SelectionListView<Item: SelectableItem>: View {
    var list: [Item]
    @Binding var selection: Item?
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
