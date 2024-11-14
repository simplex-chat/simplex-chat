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
    var isDisabled: Bool

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.system(size: 17, weight: .semibold))
            .padding()
            .frame(maxWidth: .infinity)
            .background(
                isDisabled
                ? (
                    theme.colors.isLight
                    ? .gray.opacity(0.17)
                    : .gray.opacity(0.27)
                )
                : theme.colors.primary
            )
            .foregroundColor(
                isDisabled
                ? (
                    theme.colors.isLight
                    ? .gray.opacity(0.4)
                    : .white.opacity(0.2)
                )
                : .white
            )
            .cornerRadius(16)
            .scaleEffect(configuration.isPressed ? 0.95 : 1.0)
    }
}

struct ChooseServerOperators: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme
    var onboarding: Bool
    @State private var showInfoSheet = false
    @State private var serverOperators: [ServerOperator] = []
    @State private var selectedOperatorIds = Set<Int64>()
    @State private var reviewConditionsNavLinkActive = false
    @State private var justOpened = true

    var selectedOperators: [ServerOperator] { serverOperators.filter { selectedOperatorIds.contains($0.operatorId) } }

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

                        Spacer()

                        let reviewForOperators = selectedOperators.filter { !$0.conditionsAcceptance.conditionsAccepted }
                        let canReviewLater = reviewForOperators.allSatisfy { $0.conditionsAcceptance.usageAllowed }

                        VStack(spacing: 8) {
                            if !reviewForOperators.isEmpty {
                                reviewConditionsButton()
                            } else {
                                continueButton()
                            }
                            if onboarding {
                                Text("You can disable operators and configure your servers in Network & servers settings.")
                                    .multilineTextAlignment(.center)
                                    .font(.footnote)
                                    .padding(.horizontal, 32)
                            }
                        }
                        .padding(.bottom)

                        if !onboarding && !reviewForOperators.isEmpty {
                            VStack(spacing: 8) {
                                reviewLaterButton()
                                Text("Conditions will be considered accepted for used operators after 30 days.")
                                    .multilineTextAlignment(.center)
                                    .font(.footnote)
                                    .padding(.horizontal, 32)
                            }
                            .disabled(!canReviewLater)
                            .padding(.bottom)
                        }
                    }
                    .frame(minHeight: g.size.height)
                }
            }
            .frame(maxHeight: .infinity)
            .padding()
            .onAppear {
                if justOpened {
                    serverOperators = ChatModel.shared.serverOperators
                    selectedOperatorIds = Set(serverOperators.filter { $0.enabled }.map { $0.operatorId })
                    justOpened = false
                }
            }
            .sheet(isPresented: $showInfoSheet) {
                ChooseServerOperatorsInfoView()
            }
        }
    }

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
        let checked = selectedOperatorIds.contains(serverOperator.operatorId)
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
                selectedOperatorIds.remove(serverOperator.operatorId)
            } else {
                selectedOperatorIds.insert(serverOperator.operatorId)
            }
        }
    }

    private func reviewConditionsButton() -> some View {
        ZStack {
            Button {
                reviewConditionsNavLinkActive = true
            } label: {
                Text("Review conditions")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: selectedOperatorIds.isEmpty))
            .disabled(selectedOperatorIds.isEmpty)

            NavigationLink(isActive: $reviewConditionsNavLinkActive) {
                reviewConditionsDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    @ViewBuilder private func reviewConditionsDestinationView() -> some View {
        let acceptForOperators = selectedOperators.filter { !$0.conditionsAcceptance.conditionsAccepted }
        UsageConditionsView(
            onSheet: false,
            conditionsAction: .review(operators: acceptForOperators, deadline: nil, showNotice: false),
            onAcceptAction: { continueToNextStep() }
        )
        .navigationTitle("Conditions of use")
        .navigationBarTitleDisplayMode(.large)
        .modifier(ThemedBackground(grouped: true))
    }

    private func continueButton() -> some View {
        Button {
            continueToNextStep()
        } label: {
            Text("Continue")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: selectedOperatorIds.isEmpty))
        .disabled(selectedOperatorIds.isEmpty)
    }

    private func reviewLaterButton() -> some View {
        Button {
            continueToNextStep()
        } label: {
            Text("Review later")
        }
        .buttonStyle(.borderless)
    }

    private func continueToNextStep() {
        if onboarding {
            // TODO setServerOperators (to enable/disable operator(s) and set roles)
            withAnimation {
                onboardingStageDefault.set(.step4_SetNotificationsMode)
                ChatModel.shared.onboardingStage = .step4_SetNotificationsMode
            }
        } else {
            dismiss()
        }
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
    ChooseServerOperators(onboarding: true)
}
