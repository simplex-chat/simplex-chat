//
//  NotificationsModeView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/07/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SetNotificationsMode: View {
    @EnvironmentObject var m: ChatModel
    @State private var notificationMode = NotificationsMode.instant
    @State private var showAlert: NotificationAlert?

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                Text("Push notifications")
                    .font(.largeTitle)
                    .bold()
                    .frame(maxWidth: .infinity)

                Text("Send notifications:")
                ForEach(NotificationsMode.values) { mode in
                    NtfModeSelector(mode: mode, selection: $notificationMode)
                }

                Spacer()

                Button {
                    if let token = m.deviceToken {
                        setNotificationsMode(token, notificationMode)
                    } else {
                        AlertManager.shared.showAlertMsg(title: "No device token!")
                    }
                    onboardingStageDefault.set(.onboardingComplete)
                    m.onboardingStage = .onboardingComplete
                } label: {
                    if case .off = notificationMode {
                        Text("Use chat")
                    } else {
                        Text("Enable notifications")
                    }
                }
                .font(.title)
                .frame(maxWidth: .infinity)
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .bottom)
        }
    }

    private func setNotificationsMode(_ token: DeviceToken, _ mode: NotificationsMode) {
        switch mode {
        case .off:
            m.tokenStatus = .new
            m.notificationMode = .off
        default:
            Task {
                do {
                    let status = try await apiRegisterToken(token: token, notificationMode: mode)
                    await MainActor.run {
                        m.tokenStatus = status
                        m.notificationMode = mode
                    }
                } catch let error {
                    let a = getErrorAlert(error, "Error enabling notifications")
                    AlertManager.shared.showAlertMsg(
                        title: a.title,
                        message: a.message
                    )
                }
            }
        }
    }
}

struct NtfModeSelector: View {
    @EnvironmentObject var MaterialTheme: MaterialTheme
    var mode: NotificationsMode
    @Binding var selection: NotificationsMode
    @State private var tapped = false

    var body: some View {
        ZStack {
            VStack(alignment: .leading, spacing: 4) {
                Text(mode.label)
                    .font(.headline)
                    .foregroundColor(selection == mode ? MaterialTheme.colors.primary : MaterialTheme.colors.secondary)
                Text(ntfModeDescription(mode))
                    .lineLimit(10)
                    .font(.subheadline)
            }
            .padding(12)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(tapped ? Color(uiColor: .secondarySystemFill) : MaterialTheme.colors.background)
        .clipShape(RoundedRectangle(cornerRadius: 18))
        .overlay(
            RoundedRectangle(cornerRadius: 18)
                .stroke(selection == mode ? MaterialTheme.colors.primary : Color(uiColor: .secondarySystemFill), lineWidth: 2)
        )
        ._onButtonGesture { down in
            tapped = down
            if down { selection = mode }
        } perform: {}
    }
}

struct NotificationsModeView_Previews: PreviewProvider {
    static var previews: some View {
        SetNotificationsMode()
    }
}
