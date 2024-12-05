//
//  ChooseServerOperators.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 31.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let conditionsURL = URL(string: "https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md")!

struct OnboardingButtonStyle: ButtonStyle {
    @EnvironmentObject var theme: AppTheme
    var isDisabled: Bool = false

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

private enum ChooseServerOperatorsSheet: Identifiable {
    case showInfo
    case showConditions

    var id: String {
        switch self {
        case .showInfo: return "showInfo"
        case .showConditions: return "showConditions"
        }
    }
}

struct ChooseServerOperators: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme
    var onboarding: Bool
    @State private var serverOperators: [ServerOperator] = []
    @State private var selectedOperatorIds = Set<Int64>()
    @State private var reviewConditionsNavLinkActive = false
    @State private var sheetItem: ChooseServerOperatorsSheet? = nil
    @State private var notificationsModeNavLinkActive = false
    @State private var justOpened = true

    var selectedOperators: [ServerOperator] { serverOperators.filter { selectedOperatorIds.contains($0.operatorId) } }

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .leading, spacing: 20) {
                    let title = Text("Server operators")
                        .font(.largeTitle)
                        .bold()
                        .frame(maxWidth: .infinity, alignment: .center)
                    
                    if onboarding {
                        title.padding(.top, 50)
                    } else {
                        title
                    }

                    infoText()
                        .frame(maxWidth: .infinity, alignment: .center)

                    Spacer()
                    
                    ForEach(serverOperators) { srvOperator in
                        operatorCheckView(srvOperator)
                    }
                    Text("You can configure servers via settings.")
                        .font(.footnote)
                        .multilineTextAlignment(.center)
                        .frame(maxWidth: .infinity, alignment: .center)
                        .padding(.horizontal, 32)
                    
                    Spacer()
                    
                    let reviewForOperators = selectedOperators.filter { !$0.conditionsAcceptance.conditionsAccepted }
                    let canReviewLater = reviewForOperators.allSatisfy { $0.conditionsAcceptance.usageAllowed }
                    let currEnabledOperatorIds = Set(serverOperators.filter { $0.enabled }.map { $0.operatorId })
                    
                    VStack(spacing: 8) {
                        if !reviewForOperators.isEmpty {
                            reviewConditionsButton()
                        } else if selectedOperatorIds != currEnabledOperatorIds && !selectedOperatorIds.isEmpty {
                            setOperatorsButton()
                        } else {
                            continueButton()
                        }
                        if onboarding {
                            Group {
                                if reviewForOperators.isEmpty {
                                    Button("Conditions of use") {
                                        sheetItem = .showConditions
                                    }
                                } else {
                                    Text("Conditions of use")
                                        .foregroundColor(.clear)
                                }
                            }
                            .font(.system(size: 17, weight: .semibold))
                            .frame(minHeight: 40)
                        }
                    }
                    
                    if !onboarding && !reviewForOperators.isEmpty {
                        VStack(spacing: 8) {
                            reviewLaterButton()
                            (
                                Text("Conditions will be accepted for enabled operators after 30 days.")
                                + textSpace
                                + Text("You can configure operators in Network & servers settings.")
                            )
                            .multilineTextAlignment(.center)
                            .font(.footnote)
                            .padding(.horizontal, 32)
                        }
                        .frame(maxWidth: .infinity)
                        .disabled(!canReviewLater)
                        .padding(.bottom)
                    }
                }
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
                case .showInfo:
                    ChooseServerOperatorsInfoView()
                case .showConditions:
                    UsageConditionsView(
                        currUserServers: Binding.constant([]),
                        userServers: Binding.constant([])
                    )
                    .modifier(ThemedBackground(grouped: true))
                }
            }
        }
        .frame(maxHeight: .infinity)
        .padding(onboarding ? 25 : 16)
    }

    private func infoText() -> some View {
        Button {
            sheetItem = .showInfo
        } label: {
            Label("How it helps privacy", systemImage: "info.circle")
                .font(.headline)
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
        .background(theme.colors.background)
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

    private func setOperatorsButton() -> some View {
        notificationsModeNavLinkButton {
            Button {
                Task {
                    if let enabledOperators = enabledOperators(serverOperators) {
                        let r = try await setServerOperators(operators: enabledOperators)
                        await MainActor.run {
                            ChatModel.shared.conditions = r
                            continueToNextStep()
                        }
                    } else {
                        await MainActor.run {
                            continueToNextStep()
                        }
                    }
                }
            } label: {
                Text("Update")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: selectedOperatorIds.isEmpty))
            .disabled(selectedOperatorIds.isEmpty)
        }
    }

    private func continueButton() -> some View {
        notificationsModeNavLinkButton {
            Button {
                continueToNextStep()
            } label: {
                Text("Continue")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: selectedOperatorIds.isEmpty))
            .disabled(selectedOperatorIds.isEmpty)
        }
    }

    private func reviewLaterButton() -> some View {
        notificationsModeNavLinkButton {
            Button {
                continueToNextStep()
            } label: {
                Text("Review later")
            }
            .buttonStyle(.borderless)
        }
    }

    private func continueToNextStep() {
        if onboarding {
            onboardingStageDefault.set(.step4_SetNotificationsMode)
            notificationsModeNavLinkActive = true
        } else {
            dismiss()
        }
    }

    func notificationsModeNavLinkButton(_ button: @escaping (() -> some View)) -> some View {
        ZStack {
            button()

            NavigationLink(isActive: $notificationsModeNavLinkActive) {
                notificationsModeDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func notificationsModeDestinationView() -> some View {
        SetNotificationsMode()
            .navigationBarBackButtonHidden(true)
            .modifier(ThemedBackground())
    }

    private func reviewConditionsDestinationView() -> some View {
        reviewConditionsView()
            .navigationTitle("Conditions of use")
            .navigationBarTitleDisplayMode(.large)
            .toolbar { ToolbarItem(placement: .navigationBarTrailing, content: conditionsLinkButton) }
            .modifier(ThemedBackground(grouped: true))
    }

    @ViewBuilder private func reviewConditionsView() -> some View {
        let operatorsWithConditionsAccepted = ChatModel.shared.conditions.serverOperators.filter { $0.conditionsAcceptance.conditionsAccepted }
        let acceptForOperators = selectedOperators.filter { !$0.conditionsAcceptance.conditionsAccepted }
        VStack(alignment: .leading, spacing: 20) {
            if !operatorsWithConditionsAccepted.isEmpty {
                Text("Conditions are already accepted for following operator(s): **\(operatorsWithConditionsAccepted.map { $0.legalName_ }.joined(separator: ", "))**.")
                Text("The same conditions will apply to operator(s): **\(acceptForOperators.map { $0.legalName_ }.joined(separator: ", "))**.")
            } else {
                Text("Conditions will be accepted for operator(s): **\(acceptForOperators.map { $0.legalName_ }.joined(separator: ", "))**.")
            }
            ConditionsTextView()
                .frame(maxHeight: .infinity)
            acceptConditionsButton()
                .padding(.bottom)
                .padding(.bottom)
        }
        .padding(.horizontal, 25)
    }

    private func acceptConditionsButton() -> some View {
        notificationsModeNavLinkButton {
            Button {
                Task {
                    do {
                        let conditionsId = ChatModel.shared.conditions.currentConditions.conditionsId
                        let acceptForOperators = selectedOperators.filter { !$0.conditionsAcceptance.conditionsAccepted }
                        let operatorIds = acceptForOperators.map { $0.operatorId }
                        let r = try await acceptConditions(conditionsId: conditionsId, operatorIds: operatorIds)
                        await MainActor.run {
                            ChatModel.shared.conditions = r
                        }
                        if let enabledOperators = enabledOperators(r.serverOperators) {
                            let r2 = try await setServerOperators(operators: enabledOperators)
                            await MainActor.run {
                                ChatModel.shared.conditions = r2
                                continueToNextStep()
                            }
                        } else {
                            await MainActor.run {
                                continueToNextStep()
                            }
                        }
                    } catch let error {
                        await MainActor.run {
                            showAlert(
                                NSLocalizedString("Error accepting conditions", comment: "alert title"),
                                message: responseError(error)
                            )
                        }
                    }
                }
            } label: {
                Text("Accept conditions")
            }
            .buttonStyle(OnboardingButtonStyle())
        }
    }

    private func enabledOperators(_ operators: [ServerOperator]) -> [ServerOperator]? {
        var ops = operators
        if !ops.isEmpty {
            for i in 0..<ops.count {
                var op = ops[i]
                op.enabled = selectedOperatorIds.contains(op.operatorId)
                ops[i] = op
            }
            let haveSMPStorage = ops.contains(where: { $0.enabled && $0.smpRoles.storage })
            let haveSMPProxy = ops.contains(where: { $0.enabled && $0.smpRoles.proxy })
            let haveXFTPStorage = ops.contains(where: { $0.enabled && $0.xftpRoles.storage })
            let haveXFTPProxy = ops.contains(where: { $0.enabled && $0.xftpRoles.proxy })
            if haveSMPStorage && haveSMPProxy && haveXFTPStorage && haveXFTPProxy {
                return ops
            } else if let firstEnabledIndex = ops.firstIndex(where: { $0.enabled }) {
                var op = ops[firstEnabledIndex]
                if !haveSMPStorage { op.smpRoles.storage = true }
                if !haveSMPProxy { op.smpRoles.proxy = true }
                if !haveXFTPStorage { op.xftpRoles.storage = true }
                if !haveXFTPProxy { op.xftpRoles.proxy = true }
                ops[firstEnabledIndex] = op
                return ops
            } else { // Shouldn't happen - view doesn't let to proceed if no operators are enabled
                return nil
            }
        } else {
            return nil
        }
    }
}

let operatorsPostLink = URL(string: "https://simplex.chat/blog/20241125-servers-operated-by-flux-true-privacy-and-decentralization-for-all-users.html")!

struct ChooseServerOperatorsInfoView: View {
    var body: some View {
        VStack(alignment: .leading) {
            Text("Server operators")
                .font(.largeTitle)
                .bold()
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                        Text("The app protects your privacy by using different operators in each conversation.")
                        Text("When more than one operator is enabled, none of them has metadata to learn who communicates with whom.")
                        Text("For example, if your contact receives messages via a SimpleX Chat server, your app will deliver them via a Flux server.")
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

#Preview {
    ChooseServerOperators(onboarding: true)
}
