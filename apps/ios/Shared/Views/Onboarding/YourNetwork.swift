//
//  YourNetwork.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 22/04/2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private enum YourNetworkSheet: Identifiable {
    case configureOperators
    case configureNotifications

    var id: String {
        switch self {
        case .configureOperators: return "configureOperators"
        case .configureNotifications: return "configureNotifications"
        }
    }
}

struct YourNetworkView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @State private var serverOperators: [ServerOperator] = []
    @State private var selectedOperatorIds = Set<Int64>()
    @State private var notificationMode: NotificationsMode = .instant
    @State private var sheetItem: YourNetworkSheet? = nil
    @State private var nextStepNavLinkActive = false
    @State private var justOpened = true

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .center, spacing: 10) {
                Spacer(minLength: 0)

                #if SIMPLEX_ASSETS
                Image(colorScheme == .light ? "your-network" : "your-network-light")
                    .resizable()
                    .scaledToFit()
                    .frame(maxWidth: .infinity)
                #else
                ZStack {
                    let gp = OnboardingCardView.gradientPoints(aspectRatio: 1.0, scale: colorScheme == .light ? 1.2 : 1.5)
                    LinearGradient(
                        stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
                        startPoint: gp.start,
                        endPoint: gp.end
                    )
                    Image(systemName: "network")
                        .font(.system(size: 72))
                        .foregroundColor(theme.colors.primary)
                }
                .aspectRatio(1.0, contentMode: .fit)
                .clipShape(RoundedRectangle(cornerRadius: 24))
                .padding(.horizontal, 25)
                .frame(maxWidth: .infinity)
                #endif

                Text("Your network")
                    .font(.largeTitle)
                    .bold()
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.top, 15)

                Text("Network routers cannot know\nwho talks to whom")
                    .font(.title3)
                    .fontWeight(.medium)
                    .foregroundColor(theme.colors.secondary)
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)

                VStack(alignment: .leading, spacing: 20) {
                    configureRoutersButton()
                    configureNotificationsButton()
                }
                .padding(.top, 15)
                .padding(.bottom, 15)

                Spacer(minLength: 0)

                continueButton()
                    .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
            }
            .padding(.horizontal, 25)
            .padding(.top, 8)
            .padding(.bottom, 20)
            .frame(minHeight: g.size.height)
        }
        .onAppear {
            if justOpened {
                serverOperators = ChatModel.shared.conditions.serverOperators
                selectedOperatorIds = Set(serverOperators.filter { $0.enabled }.map { $0.operatorId })
                justOpened = false
            }
        }
        .sheet(item: $sheetItem) { item in
            switch item {
            case .configureOperators:
                ChooseServerOperators(serverOperators: serverOperators, selectedOperatorIds: $selectedOperatorIds)
                    .modifier(ThemedBackground())
            case .configureNotifications:
                SetNotificationsMode(notificationMode: $notificationMode)
                    .modifier(ThemedBackground())
            }
        }
        .frame(maxHeight: .infinity)
        .navigationBarHidden(true)
    }

    private func configureRoutersButton() -> some View {
        Button {
            sheetItem = .configureOperators
        } label: {
            HStack(spacing: 6) {
                Text("Setup routers")
                    .fontWeight(.medium)
                ForEach(serverOperators.reversed()) { op in
                    Image(op.logo(colorScheme))
                        .resizable()
                        .scaledToFit()
                        .frame(width: 22, height: 22)
                        .grayscale(selectedOperatorIds.contains(op.operatorId) ? 0.0 : 1.0)
                }
            }
        }
    }

    private func configureNotificationsButton() -> some View {
        Button {
            sheetItem = .configureNotifications
        } label: {
            HStack(spacing: 4) {
                Text("Setup notifications")
                    .fontWeight(.medium)
                Image(systemName: notificationMode.icon)
            }
        }
    }

    private func continueButton() -> some View {
        ZStack {
            Button {
                applyNotificationMode()
                onboardingStageDefault.set(.step4_NetworkCommitments)
                nextStepNavLinkActive = true
            } label: {
                Text("Continue")
            }
            .buttonStyle(OnboardingButtonStyle())

            NavigationLink(isActive: $nextStepNavLinkActive) {
                OnboardingConditionsView(selectedOperatorIds: selectedOperatorIds)
                    .navigationBarBackButtonHidden(true)
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func applyNotificationMode() {
        let m = ChatModel.shared
        if let token = m.deviceToken {
            switch notificationMode {
            case .off:
                m.tokenStatus = .new
                m.notificationMode = .off
            default:
                Task {
                    do {
                        let status = try await apiRegisterToken(token: token, notificationMode: notificationMode)
                        await MainActor.run {
                            m.tokenStatus = status
                            m.notificationMode = notificationMode
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
}
