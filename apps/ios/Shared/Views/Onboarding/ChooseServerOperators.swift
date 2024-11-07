//
//  ChooseServerOperators.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 31.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct OnboardingButtonStyle: ButtonStyle {
    @EnvironmentObject var theme: AppTheme

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.system(size: 17, weight: .semibold))
            .padding()
            .frame(maxWidth: .infinity)
            .background(theme.colors.primary)
            .foregroundColor(theme.colors.isLight ? .white : .black) // TODO review
            .cornerRadius(16)
            .scaleEffect(configuration.isPressed ? 0.95 : 1.0)
    }
}

struct ChooseServerOperators: View {
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme
    @State private var showInfoSheet = false
    @State private var serverOperators: [ServerOperator] = []
    @State private var selectedOperators = Set<Int64>()
    @State private var customServersNavLinkActive = false
    @State private var reviewConditionsNavLinkActive = false
    @State private var currSMPServers: [UserServer] = []
    @State private var smpServers: [UserServer] = []
    @State private var currXFTPServers: [UserServer] = []
    @State private var xftpServers: [UserServer] = []
    @State private var justOpened = true

    var body: some View {
        NavigationView {
            GeometryReader { g in
                ScrollView {
                    VStack(alignment: .leading, spacing: 20) {
                        Text("Choose operators")
                            .font(.largeTitle)
                            .bold()

                        infoText()

                        Spacer()

                        ForEach(serverOperators) { srvOperator in
                            operatorCheckView(srvOperator)
                        }

                        customServersButton()

                        Spacer()
                        Spacer()

                        reviewConditionsButton()
                            .padding(.bottom)
                    }
                    .frame(minHeight: g.size.height)
                }
            }
            .frame(maxHeight: .infinity)
            .padding()
            .onAppear {
                if justOpened {
                    serverOperators = ChatModel.shared.serverOperators
                    selectedOperators = Set(serverOperators.filter { $0.enabled }.map { $0.operatorId })
                    justOpened = false
                }
            }
            .sheet(isPresented: $showInfoSheet) {
                ChooseServerOperatorsInfoView()
            }
        }
    }

    var acceptForOperators: [ServerOperator] { serverOperators.filter { selectedOperators.contains($0.operatorId) } }

    private func infoText() -> some View {
        HStack(spacing: 12) {
            Image(systemName: "info.circle")
                .resizable()
                .scaledToFit()
                .frame(width: 20, height: 20)
                .foregroundColor(theme.colors.primary)
                .onTapGesture {
                    showInfoSheet = true
                }

            Text("Select operators, whose servers you will be using.")
        }
    }

    @ViewBuilder private func operatorCheckView(_ serverOperator: ServerOperator) -> some View {
        let checked = selectedOperators.contains(serverOperator.operatorId)
        let icon = checked ? "checkmark.circle.fill" : "circle"
        let iconColor = checked ? theme.colors.primary : Color(uiColor: .tertiaryLabel).asAnotherColorFromSecondary(theme)
        HStack(spacing: 10) {
            Image(serverOperator.largeLogo(colorScheme))
                .resizable()
                .scaledToFit()
                .frame(height: 48)
            Spacer()
            Image(systemName: icon)
                .resizable()
                .scaledToFit()
                .frame(width: 26, height: 26)
                .foregroundColor(iconColor)
        }
        .background(Color(.systemBackground))
        .padding()
        .clipShape(RoundedRectangle(cornerRadius: 18))
        .overlay(
            RoundedRectangle(cornerRadius: 18)
                .stroke(Color(uiColor: .secondarySystemFill), lineWidth: 2)
        )
        .padding(.horizontal, 2)
        .onTapGesture {
            if checked {
                selectedOperators.remove(serverOperator.operatorId)
            } else {
                selectedOperators.insert(serverOperator.operatorId)
            }
        }
    }

    private func customServersButton() -> some View {
        ZStack {
            HStack(spacing: 10) {
                ZStack {
                    Image(systemName: "externaldrive.connected.to.line.below")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 22, height: 22)
                        .foregroundColor(theme.colors.secondary)
                }
                .frame(width: 30, height: 30)

                Text("Custom servers")
                    .font(.title3)

                Spacer()

                Text("Configure…")
                    .font(.callout)
                    .foregroundColor(theme.colors.primary)
            }
            .background(Color(.systemBackground))
            .padding()
            .clipShape(RoundedRectangle(cornerRadius: 18))
            .overlay(
                RoundedRectangle(cornerRadius: 18)
                    .stroke(Color(uiColor: .secondarySystemFill), lineWidth: 2)
            )
            .padding(.horizontal, 2)
            .onTapGesture {
                customServersNavLinkActive = true
            }

            NavigationLink(isActive: $customServersNavLinkActive) {
                customServersDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func customServersDestinationView() -> some View {
        YourServersView(
            currSMPServers: $currSMPServers,
            smpServers: $smpServers,
            currXFTPServers: $currXFTPServers,
            xftpServers: $xftpServers
        )
        .navigationTitle("Custom servers")
        .modifier(ThemedBackground(grouped: true))
    }

    private func reviewConditionsButton() -> some View {
        ZStack {
            Button {
                reviewConditionsNavLinkActive = true
            } label: {
                Text("Review conditions")
            }
            .buttonStyle(OnboardingButtonStyle())
            .padding(.horizontal)
            .disabled(selectedOperators.isEmpty)

            NavigationLink(isActive: $reviewConditionsNavLinkActive) {
                reviewConditionsDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func reviewConditionsDestinationView() -> some View {
        UsageConditionsView(
            showTitle: false,
            dismissOnAccept: false,
            conditionsAction: .review(operators: acceptForOperators, deadline: nil, showNotice: false),
            onAcceptAction: { date in
                ChatModel.shared.acceptConditionsForOperators(acceptForOperators, date, enable: true)
                withAnimation {
                    onboardingStageDefault.set(.step4_SetNotificationsMode)
                    ChatModel.shared.onboardingStage = .step4_SetNotificationsMode
                }
            }
        )
        .navigationTitle("Conditions of use")
        .navigationBarTitleDisplayMode(.large)
        .modifier(ThemedBackground(grouped: true))
    }
}

// TODO fix padding, dark mode
struct ChooseServerOperatorsInfoView: View {
    var body: some View {
        VStack(alignment: .leading) {
            Text("Why choose multiple operators")
                .font(.largeTitle)
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                        Text("Selecting multiple operators improves protection of your communication graph.")
                        Text("TODO Better explanation")
                    }
                    .padding(.bottom)
                }
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
        .modifier(ThemedBackground())
    }
}

#Preview {
    ChooseServerOperators()
}
