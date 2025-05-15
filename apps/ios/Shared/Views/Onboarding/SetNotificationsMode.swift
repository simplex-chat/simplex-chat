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
    @State private var showInfo: Bool = false

    var body: some View {
        GeometryReader { g in
            let v = ScrollView {
                VStack(alignment: .center, spacing: 20) {
                    Text("Push notifications")
                        .font(.largeTitle)
                        .bold()
                        .padding(.top, 25)
                    
                    infoText()
                    
                    Spacer()

                    ForEach(NotificationsMode.values) { mode in
                        NtfModeSelector(mode: mode, selection: $notificationMode)
                    }
                    
                    Spacer()
                    
                    VStack(spacing: 10) {
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
                        .buttonStyle(OnboardingButtonStyle())
                        onboardingButtonPlaceholder()
                    }
                }
                .padding(25)
                .frame(minHeight: g.size.height)
            }
            if #available(iOS 16.4, *) {
                v.scrollBounceBehavior(.basedOnSize)
            } else {
                v
            }
        }
        .frame(maxHeight: .infinity)
        .sheet(isPresented: $showInfo) {
            NotificationsInfoView()
        }
        .navigationBarHidden(true) // necessary on iOS 15
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
    
    private func infoText() -> some View {
        Button {
            showInfo = true
        } label: {
            Label("How it affects privacy", systemImage: "info.circle")
                .font(.headline)
        }
    }
}

struct NtfModeSelector: View {
    @EnvironmentObject var theme: AppTheme
    var mode: NotificationsMode
    @Binding var selection: NotificationsMode
    @State private var tapped = false

    var body: some View {
        ZStack {
            HStack(spacing: 16) {
                Image(systemName: mode.icon)
                    .resizable()
                    .scaledToFill()
                    .frame(width: mode.icon == "bolt" ? 14 : 18, height: 18)
                    .foregroundColor(selection == mode ? theme.colors.primary : theme.colors.secondary)
                VStack(alignment: .leading, spacing: 4) {
                    Text(mode.label)
                        .font(.headline)
                        .foregroundColor(selection == mode ? theme.colors.primary : theme.colors.secondary)
                    Text(ntfModeShortDescription(mode))
                        .lineLimit(2)
                        .font(.callout)
                        .fixedSize(horizontal: false, vertical: true)
                }
            }
            .padding(.vertical, 12)
            .padding(.trailing, 12)
            .padding(.leading, 16)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(tapped ? Color(uiColor: .secondarySystemFill) : theme.colors.background)
        .clipShape(RoundedRectangle(cornerRadius: 18))
        .overlay(
            RoundedRectangle(cornerRadius: 18)
                .stroke(selection == mode ? theme.colors.primary : Color(uiColor: .secondarySystemFill), lineWidth: 2)
        )
        ._onButtonGesture { down in
            tapped = down
            if down { selection = mode }
        } perform: {}
    }
}

struct NotificationsInfoView: View {
    var body: some View {
        VStack(alignment: .leading) {
            Text("Notifications privacy")
                .font(.largeTitle)
                .bold()
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                        ForEach(NotificationsMode.values) { mode in
                            VStack(alignment: .leading, spacing: 4) {
                                (Text(Image(systemName: mode.icon)) + textSpace + Text(mode.label))
                                    .font(.headline)
                                    .foregroundColor(.secondary)
                                Text(ntfModeDescription(mode))
                                    .lineLimit(10)
                                    .font(.callout)
                            }
                        }
                    }
                    .padding(.bottom)
                }
            }
        }
        .padding()
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .modifier(ThemedBackground())
    }
}

struct NotificationsModeView_Previews: PreviewProvider {
    static var previews: some View {
        SetNotificationsMode()
    }
}
