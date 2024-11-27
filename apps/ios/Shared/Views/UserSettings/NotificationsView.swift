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
    @EnvironmentObject var theme: AppTheme
    @State private var notificationMode: NotificationsMode = ChatModel.shared.notificationMode
    @State private var showAlert: NotificationAlert?
    @State private var legacyDatabase = dbContainerGroupDefault.get() == .documents
    @State private var testing = false
    @State private var testedSuccess: Bool? = nil

    var body: some View {
        ZStack {
            viewBody()
            if testing {
                ProgressView().scaleEffect(2)
            }
        }
        .alert(item: $showAlert) { alert in
            if let token = m.deviceToken {
                return notificationAlert(alert, token)
            } else {
                return Alert(title: Text("No device token!"))
            }
        }
    }

    private func viewBody() -> some View {
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
                                    .foregroundColor(theme.colors.secondary)
                            }
                            .font(.callout)
                            .padding(.top, 1)
                        }
                    }
                    .navigationTitle("Send notifications")
                    .modifier(ThemedBackground(grouped: true))
                    .navigationBarTitleDisplayMode(.inline)
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
                                    .foregroundColor(theme.colors.secondary)
                                Button("Open Settings") {
                                    DispatchQueue.main.async {
                                        UIApplication.shared.open(URL(string: UIApplication.openSettingsURLString)!, options: [:], completionHandler: nil)
                                    }
                                }
                            }
                        }
                    }
                    .navigationTitle("Show preview")
                    .modifier(ThemedBackground(grouped: true))
                    .navigationBarTitleDisplayMode(.inline)
                } label: {
                    HStack {
                        Text("Show preview")
                        Spacer()
                        Text(m.notificationPreview.label)
                    }
                }

                if let server = m.notificationServer {
                    smpServers("Push server", [server], theme.colors.secondary)
                    testServerButton(server)
                }
            } header: {
                Text("Push notifications")
                    .foregroundColor(theme.colors.secondary)
            } footer: {
                if legacyDatabase {
                    Text("Please restart the app and migrate the database to enable push notifications.")
                        .foregroundColor(theme.colors.secondary)
                        .font(.callout)
                        .padding(.top, 1)
                }
            }
        }
        .disabled(legacyDatabase)
        .onAppear {
            (m.savedToken, m.tokenStatus, m.notificationMode, m.notificationServer) = apiGetNtfToken()
        }
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
        case let .testFailure(testFailure):
            return Alert(
                title: Text("Server test failed!"),
                message: Text(testFailure.localizedDescription)
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
                        m.notificationServer = nil
                        testedSuccess = nil
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
                    let _ = try await apiRegisterToken(token: token, notificationMode: mode)
                    let (_, tknStatus, ntfMode, ntfServer) = apiGetNtfToken()
                    await MainActor.run {
                        m.tokenStatus = tknStatus
                        notificationMode = ntfMode
                        m.notificationMode = ntfMode
                        m.notificationServer = ntfServer
                        testedSuccess = nil
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

    private func testServerButton(_ server: String) -> some View {
        HStack {
            Button("Test server") {
                testing = true
                Task {
                    await testServer(server)
                    await MainActor.run { testing = false }
                }
            }
            .disabled(testing)
            if !testing {
                Spacer()
                showTestStatus()
            }
        }
    }

    @ViewBuilder func showTestStatus() -> some View {
        if testedSuccess == true {
            Image(systemName: "checkmark")
                .foregroundColor(.green)
        } else if testedSuccess == false {
            Image(systemName: "multiply")
                .foregroundColor(.red)
        }
    }

    private func testServer(_ server: String) async {
        do {
            let r = try await testProtoServer(server: server)
            switch r {
            case .success:
                await MainActor.run {
                    testedSuccess = true
                }
            case let .failure(f):
                await MainActor.run {
                    showAlert = .testFailure(testFailure: f)
                    testedSuccess = false
                }
            }
        } catch let error {
            logger.error("testServerConnection \(responseError(error))")
        }
    }
}

func ntfModeDescription(_ mode: NotificationsMode) -> LocalizedStringKey {
    switch mode {
    case .off: return "**Most private**: do not use SimpleX Chat push server. The app will check messages in background, when the system allows it, depending on how often you use the app."
    case .periodic: return "**More private**: check new messages every 20 minutes. Only device token is shared with our push server. It doesn't see how many contacts you have, or any message metadata."
    case .instant: return "**Recommended**: device token and end-to-end encrypted notifications are sent to SimpleX Chat push server, but it does not see the message content, size or who it is from."
    }
}

func ntfModeShortDescription(_ mode: NotificationsMode) -> LocalizedStringKey {
    switch mode {
    case .off: return "Check messages when allowed."
    case .periodic: return "Check messages every 20 min."
    case .instant: return "E2E encrypted notifications."
    }
}

struct SelectionListView<Item: SelectableItem>: View {
    @EnvironmentObject var theme: AppTheme
    var list: [Item]
    @Binding var selection: Item
    var onSelection: ((Item) -> Void)?
    @State private var tapped: Item? = nil

    var body: some View {
        ForEach(list) { item in
            Button {
                if selection == item { return }
                if let f = onSelection {
                    f(item)
                } else {
                    selection = item
                }
            } label: {
                HStack {
                    Text(item.label).foregroundColor(theme.colors.onBackground)
                    Spacer()
                    if selection == item {
                        Image(systemName: "checkmark")
                            .resizable().scaledToFit().frame(width: 16)
                            .foregroundColor(theme.colors.primary)
                    }
                }
            }
        }
        .environment(\.editMode, .constant(.active))
    }
}

enum NotificationAlert: Identifiable {
    case setMode(mode: NotificationsMode)
    case testFailure(testFailure: ProtocolTestFailure)
    case error(title: LocalizedStringKey, error: String)

    var id: String {
        switch self {
        case let .setMode(mode): return "enable \(mode.rawValue)"
        case let .testFailure(testFailure): return "testFailure \(testFailure.testStep) \(testFailure.testError)"
        case let .error(title, error): return "error \(title): \(error)"
        }
    }
}

struct NotificationsView_Previews: PreviewProvider {
    static var previews: some View {
        NotificationsView()
    }
}
