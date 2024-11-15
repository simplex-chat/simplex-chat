//
//  NetworkServersView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 02/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private enum NetworkAlert: Identifiable {
    case error(err: String)

    var id: String {
        switch self {
        case let .error(err): return "error \(err)"
        }
    }
}

private enum NetworkAndServersSheet: Identifiable {
    case showConditions(conditionsAction: UsageConditionsAction)

    var id: String {
        switch self {
        case .showConditions: return "showConditions"
        }
    }
}

struct NetworkAndServers: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme
    @Binding var currUserServers: [UserOperatorServers]
    @Binding var userServers: [UserOperatorServers]
    @State private var sheetItem: NetworkAndServersSheet? = nil
    @State private var justOpened = true
    @State private var showSaveDialog = false

    var body: some View {
        VStack {
            List {
                let conditionsAction = m.conditions.conditionsAction
                Section {
                    ForEach(userServers.enumerated().map { $0 }, id: \.element.id) { idx, userOperatorServers in
                        if let serverOperator = userOperatorServers.operator {
                            serverOperatorView(idx, serverOperator)
                        } else {
                            EmptyView()
                        }
                    }

                    if let conditionsAction = conditionsAction {
                        conditionsButton(conditionsAction)
                    }
                } header: {
                    Text("Preset servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    switch conditionsAction {
                    case let .review(_, deadline, _):
                        if let deadline = deadline {
                            Text("Conditions will be considered accepted for enabled operators after: \(conditionsTimestamp(deadline)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    default:
                        EmptyView()
                    }
                }

                Section {
                    if let idx = userServers.firstIndex(where: { $0.operator == nil }) {
                        NavigationLink {
                            YourServersView(
                                userServers: $userServers,
                                operatorServersIndex: idx
                            )
                            .navigationTitle("Your servers")
                            .modifier(ThemedBackground(grouped: true))
                        } label: {
                            HStack {
                                Text("Your servers")
                                
                                if userServers[idx] != currUserServers[idx] {
                                    Spacer()
                                    unsavedChangesIndicator()
                                }
                            }
                        }
                    }

                    NavigationLink {
                        AdvancedNetworkSettings()
                            .navigationTitle("Advanced settings")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Advanced network settings")
                    }
                } header: {
                    Text("Messages & files")
                        .foregroundColor(theme.colors.secondary)
                }

                Section {
                    Button("Save servers", action: { saveServers($currUserServers, $userServers) })
                        .disabled(userServers == currUserServers)
                }

                Section(header: Text("Calls").foregroundColor(theme.colors.secondary)) {
                    NavigationLink {
                        RTCServers()
                            .navigationTitle("Your ICE servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("WebRTC ICE servers")
                    }
                }

                Section(header: Text("Network connection").foregroundColor(theme.colors.secondary)) {
                    HStack {
                        Text(m.networkInfo.networkType.text)
                        Spacer()
                        Image(systemName: "circle.fill").foregroundColor(m.networkInfo.online ? .green : .red)
                    }
                }
            }
        }
        .task {
            // this condition is needed to prevent re-setting the servers when exiting single server view
            if justOpened {
                do {
                    currUserServers = try await getUserServers()
                    userServers = currUserServers
                } catch let error {
                    await MainActor.run {
                        showAlert(
                            NSLocalizedString("Error loading servers", comment: "alert title"),
                            message: responseError(error)
                        )
                    }
                }
                justOpened = false
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if userServers != currUserServers {
                showSaveDialog = true
            } else {
                dismiss()
            }
        })
        .confirmationDialog("Save servers?", isPresented: $showSaveDialog, titleVisibility: .visible) {
            Button("Save") {
                saveServers($currUserServers, $userServers)
                dismiss()
            }
            Button("Exit without saving") { dismiss() }
        }
        // TODO smarter onDismiss - apply model operators conditions state to currUserServers/userServers
        // .sheet(item: $sheetItem, onDismiss: { serverOperators = ChatModel.shared.serverOperators }) { item in
        .sheet(item: $sheetItem) { item in
            switch item {
            case let .showConditions(conditionsAction):
                UsageConditionsView(
                    conditionsAction: conditionsAction,
                    currUserServers: $currUserServers,
                    userServers: $userServers
                )
                .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    private func serverOperatorView(_ operatorServersIndex: Int, _ serverOperator: ServerOperator) -> some View {
        NavigationLink() {
            OperatorView(
                userServers: $userServers,
                operatorServersIndex: operatorServersIndex,
                serverOperatorToEdit: serverOperator,
                useOperator: serverOperator.enabled
            )
            .navigationBarTitle("\(serverOperator.tradeName) servers")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Image(serverOperator.logo(colorScheme))
                    .resizable()
                    .scaledToFit()
                    .grayscale(serverOperator.enabled ? 0.0 : 1.0)
                    .frame(width: 24, height: 24)
                Text(serverOperator.tradeName)
                    .foregroundColor(serverOperator.enabled ? theme.colors.onBackground : theme.colors.secondary)

                if userServers[operatorServersIndex] != currUserServers[operatorServersIndex] {
                    Spacer()
                    unsavedChangesIndicator()
                }
            }
        }
    }

    private func unsavedChangesIndicator() -> some View {
        Image(systemName: "pencil")
            .foregroundColor(theme.colors.secondary)
            .symbolRenderingMode(.monochrome)
            .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
    }

    private func conditionsButton(_ conditionsAction: UsageConditionsAction) -> some View {
        Button {
            sheetItem = .showConditions(conditionsAction: conditionsAction)
        } label: {
            switch conditionsAction {
            case .review:
                Text("Review conditions")
            case .accepted:
                Text("Accepted conditions")
            }
        }
    }
}

struct UsageConditionsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    var conditionsAction: UsageConditionsAction
    @Binding var currUserServers: [UserOperatorServers]
    @Binding var userServers: [UserOperatorServers]

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            Text("Conditions of use")
                .font(.largeTitle)
                .bold()
                .padding(.top)
                .padding(.top)

            switch conditionsAction {

            case let .review(operators, _, _):
                Text("Conditions will be accepted for following operator(s): **\(operators.map { $0.legalName_ }.joined(separator: ", "))**.")
                ConditionsTextView()
                acceptConditionsButton(operators)
                    .padding(.bottom)
                    .padding(.bottom)

            case let .accepted(operators):
                Text("Conditions are accepted for following operator(s): **\(operators.map { $0.legalName_ }.joined(separator: ", "))**.")
                ConditionsTextView()
                    .padding(.bottom)
                    .padding(.bottom)
            }
        }
        .padding(.horizontal)
        .frame(maxHeight: .infinity)
    }

    private func acceptConditionsButton(_ operators: [ServerOperator]) -> some View {
        Button {
            Task {
                do {
                    let conditionsId = ChatModel.shared.conditions.currentConditions.conditionsId
                    let operatorIds = operators.map { $0.operatorId }
                    let r = try await acceptConditions(conditionsId: conditionsId, operatorIds: operatorIds)
                    await MainActor.run {
                        ChatModel.shared.conditions = r
                        updateOperators($currUserServers, r.serverOperators)
                        updateOperators($userServers, r.serverOperators)
                        dismiss()
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
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }

    private func updateOperators(_ operators: Binding<[UserOperatorServers]>, _ updatedOperators: [ServerOperator]) {
        for i in 0..<operators.wrappedValue.count {
            if let updatedOperator = updatedOperators.first(where: { $0.operatorId == operators.wrappedValue[i].operator?.operatorId }) {
                operators.wrappedValue[i].operator?.conditionsAcceptance = updatedOperator.conditionsAcceptance
            }
        }
    }
}

func saveServers(_ currUserServers: Binding<[UserOperatorServers]>, _ userServers: Binding<[UserOperatorServers]>) {
    let userServersToSave = userServers.wrappedValue
    Task {
        do {
            try await setUserServers(userServers: userServersToSave)
            // Get updated servers for new server ids (otherwise it messes up delete of newly added and saved servers)
            do {
                let updatedServers = try await getUserServers()
                await MainActor.run {
                    currUserServers.wrappedValue = updatedServers
                    userServers.wrappedValue = updatedServers
                }
            } catch let error {
                logger.error("saveServers getUserServers error: \(responseError(error))")
                await MainActor.run {
                    currUserServers.wrappedValue = userServersToSave
                }
            }
        } catch let error {
            logger.error("saveServers setUserServers error: \(responseError(error))")
            await MainActor.run {
                showAlert(
                    NSLocalizedString("Error saving servers", comment: "alert title"),
                    message: responseError(error)
                )
            }
        }
    }
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers(
            currUserServers: Binding.constant([UserOperatorServers.sampleData1, UserOperatorServers.sampleDataNilOperator]),
            userServers: Binding.constant([UserOperatorServers.sampleData1, UserOperatorServers.sampleDataNilOperator])
        )
    }
}
