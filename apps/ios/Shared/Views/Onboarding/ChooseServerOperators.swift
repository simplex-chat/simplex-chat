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

private enum OnboardingConditionsViewSheet: Identifiable {
    case showConditions
    case configureOperators

    var id: String {
        switch self {
        case .showConditions: return "showConditions"
        case .configureOperators: return "configureOperators"
        }
    }
}

struct OnboardingConditionsView: View {
    @EnvironmentObject var theme: AppTheme
    @State private var serverOperators: [ServerOperator] = []
    @State private var selectedOperatorIds = Set<Int64>()
    @State private var sheetItem: OnboardingConditionsViewSheet? = nil
    @State private var notificationsModeNavLinkActive = false
    @State private var justOpened = true

    var selectedOperators: [ServerOperator] { serverOperators.filter { selectedOperatorIds.contains($0.operatorId) } }

    var body: some View {
        GeometryReader { g in
            let v = ScrollView {
                VStack(alignment: .leading, spacing: 20) {
                    Text("Conditions of use")
                        .font(.largeTitle)
                        .bold()
                        .frame(maxWidth: .infinity, alignment: .center)
                        .padding(.top, 25)

                    Spacer()

                    VStack(alignment: .leading, spacing: 20) {
                        Text("Private chats, groups and your contacts are not accessible to server operators.")
                            .lineSpacing(2)
                            .frame(maxWidth: .infinity, alignment: .leading)
                        Text("""
                        By using SimpleX Chat you agree to:
                        - send only legal content in public groups.
                        - respect other users – no spam.
                        """)
                        .lineSpacing(2)
                        .frame(maxWidth: .infinity, alignment: .leading)

                        Button("Privacy policy and conditions of use.") {
                            sheetItem = .showConditions
                        }
                        .frame(maxWidth: .infinity, alignment: .leading)
                    }
                    .padding(.horizontal, 4)

                    Spacer()

                    VStack(spacing: 12) {
                        acceptConditionsButton()

                        Button("Configure server operators") {
                            sheetItem = .configureOperators
                        }
                        .frame(minHeight: 40)
                    }
                }
                .padding(25)
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
                case .showConditions:
                    SimpleConditionsView()
                        .modifier(ThemedBackground(grouped: true))
                case .configureOperators:
                    ChooseServerOperators(serverOperators: serverOperators, selectedOperatorIds: $selectedOperatorIds)
                        .modifier(ThemedBackground())
                }
            }
            .frame(maxHeight: .infinity, alignment: .top)
            if #available(iOS 16.4, *) {
                v.scrollBounceBehavior(.basedOnSize)
            } else {
                v
            }
        }
        .frame(maxHeight: .infinity, alignment: .top)
        .navigationBarHidden(true) // necessary on iOS 15
    }

    private func continueToNextStep() {
        onboardingStageDefault.set(.step4_SetNotificationsMode)
        notificationsModeNavLinkActive = true
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
                Text("Accept")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: selectedOperatorIds.isEmpty))
            .disabled(selectedOperatorIds.isEmpty)
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

private enum ChooseServerOperatorsSheet: Identifiable {
    case showInfo

    var id: String {
        switch self {
        case .showInfo: return "showInfo"
        }
    }
}

struct ChooseServerOperators: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme
    var serverOperators: [ServerOperator]
    @Binding var selectedOperatorIds: Set<Int64>
    @State private var sheetItem: ChooseServerOperatorsSheet? = nil

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .leading, spacing: 20) {
                    Text("Server operators")
                        .font(.largeTitle)
                        .bold()
                        .frame(maxWidth: .infinity, alignment: .center)
                        .padding(.top, 25)

                    infoText()
                        .frame(maxWidth: .infinity, alignment: .center)

                    Spacer()
                    
                    ForEach(serverOperators) { srvOperator in
                        operatorCheckView(srvOperator)
                    }
                    VStack {
                        Text("SimpleX Chat and Flux made an agreement to include Flux-operated servers into the app.").padding(.bottom, 8)
                        Text("You can configure servers via settings.")
                    }
                    .font(.footnote)
                    .multilineTextAlignment(.center)
                    .frame(maxWidth: .infinity, alignment: .center)
                    .padding(.horizontal, 16)
                    
                    Spacer()

                    VStack(spacing: 8) {
                        setOperatorsButton()
                        onboardingButtonPlaceholder()
                    }
                }
                .frame(minHeight: g.size.height)
            }
            .sheet(item: $sheetItem) { item in
                switch item {
                case .showInfo:
                    ChooseServerOperatorsInfoView()
                }
            }
            .frame(maxHeight: .infinity, alignment: .top)
        }
        .frame(maxHeight: .infinity, alignment: .top)
        .padding(25)
        .interactiveDismissDisabled(selectedOperatorIds.isEmpty)
    }

    private func infoText() -> some View {
        Button {
            sheetItem = .showInfo
        } label: {
            Label("How it helps privacy", systemImage: "info.circle")
                .font(.headline)
        }
    }

    private func operatorCheckView(_ serverOperator: ServerOperator) -> some View {
        let checked = selectedOperatorIds.contains(serverOperator.operatorId)
        let icon = checked ? "checkmark.circle.fill" : "circle"
        let iconColor = checked ? theme.colors.primary : Color(uiColor: .tertiaryLabel).asAnotherColorFromSecondary(theme)
        return HStack(spacing: 10) {
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

    private func setOperatorsButton() -> some View {
        Button {
            dismiss()
        } label: {
            Text("OK")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: selectedOperatorIds.isEmpty))
        .disabled(selectedOperatorIds.isEmpty)
    }
}

let operatorsPostLink = URL(string: "https://simplex.chat/blog/20241125-servers-operated-by-flux-true-privacy-and-decentralization-for-all-users.html")!

struct ChooseServerOperatorsInfoView: View {
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        NavigationView {
            List {
                VStack(alignment: .leading, spacing: 12) {
                    Text("The app protects your privacy by using different operators in each conversation.")
                    Text("When more than one operator is enabled, none of them has metadata to learn who communicates with whom.")
                    Text("For example, if your contact receives messages via a SimpleX Chat server, your app will deliver them via a Flux server.")
                }
                .fixedSize(horizontal: false, vertical: true)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .padding(.top)

                Section {
                    ForEach(ChatModel.shared.conditions.serverOperators) { op in
                        operatorInfoNavLinkView(op)
                    }
                } header: {
                    Text("About operators")
                        .foregroundColor(theme.colors.secondary)
                }
            }
            .navigationTitle("Server operators")
            .navigationBarTitleDisplayMode(.large)
            .modifier(ThemedBackground(grouped: true))
        }
    }

    private func operatorInfoNavLinkView(_ op: ServerOperator) -> some View {
        NavigationLink() {
            OperatorInfoView(serverOperator: op)
                .navigationBarTitle("Network operator")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Image(op.logo(colorScheme))
                    .resizable()
                    .scaledToFit()
                    .frame(width: 24, height: 24)
                Text(op.tradeName)
            }
        }
    }
}

#Preview {
    OnboardingConditionsView()
}
