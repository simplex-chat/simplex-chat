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
    @State private var currUserServers: [UserOperatorServers] = []
    @State private var userServers: [UserOperatorServers] = []
    @State private var sheetItem: NetworkAndServersSheet? = nil
    @State private var justOpened = true
    @State private var showSaveDialog = false

    var body: some View {
        VStack {
            List {
                let conditionsAction = m.usageConditionsAction
                Section {
                    ForEach(userServers.enumerated().filter { $0.element.operator != nil }, id: \.offset) { idx, userOperatorServers in
                        serverOperatorView(idx, userOperatorServers)
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
                            Text("Review conditions until: \(conditionsTimestamp(deadline)).")
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
                                operatorServersIndex: idx,
                                customServers: userServers[idx]
                            )
                            .navigationTitle("Your servers")
                            .modifier(ThemedBackground(grouped: true))
                        } label: {
                            Text("Your servers")
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
                    Button("Save servers", action: saveServers)
                        .disabled(saveDisabled)
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
                    showAlert(
                        NSLocalizedString("Error loading servers", comment: "alert title"),
                        message: responseError(error)
                    )
                }
                justOpened = false
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if saveDisabled {
                dismiss()
                justOpened = false
            } else {
                showSaveDialog = true
            }
        })
        .confirmationDialog("Save servers?", isPresented: $showSaveDialog, titleVisibility: .visible) {
            Button("Save") {
                saveServers()
                dismiss()
                justOpened = false
            }
            Button("Exit without saving") { dismiss() }
        }
        // TODO smarter onDismiss - apply model operators conditions state to currUserServers/userServers
        // .sheet(item: $sheetItem, onDismiss: { serverOperators = ChatModel.shared.serverOperators }) { item in
        .sheet(item: $sheetItem) { item in
            switch item {
            case let .showConditions(conditionsAction):
                UsageConditionsView(
                    showTitle: true,
                    dismissOnAccept: true,
                    conditionsAction: conditionsAction,
                    onAcceptAction: { date in
                        switch conditionsAction {
                        case let .review(operators, _, _): ChatModel.shared.acceptConditionsForOperators(operators, date)
                        default: break
                        }
                    }
                )
                .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    @ViewBuilder private func serverOperatorView(_ operatorServersIndex: Int, _ userOperatorServers: UserOperatorServers) -> some View {
        if let srvOperator = userOperatorServers.operator {
            NavigationLink() {
                OperatorView(
                    userServers: $userServers,
                    operatorServersIndex: operatorServersIndex,
                    userOperatorServers: userOperatorServers,
                    serverOperatorToEdit: srvOperator,
                    useOperator: srvOperator.enabled
                )
                .navigationBarTitle("\(srvOperator.tradeName) servers")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
            } label: {
                HStack {
                    Image(srvOperator.logo(colorScheme))
                        .resizable()
                        .scaledToFit()
                        .grayscale(srvOperator.enabled ? 0.0 : 1.0)
                        .frame(width: 24, height: 24)
                    Text(srvOperator.tradeName)
                        .foregroundColor(srvOperator.enabled ? theme.colors.onBackground : theme.colors.secondary)
                }
            }
        }
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


    private var saveDisabled: Bool {
        userServers == currUserServers
    }

    func saveServers() {
        Task {
            do {
                // TODO validateServers
                // TODO Apply validation result to userServers state
                try await setUserServers(userServers: userServers)
                await MainActor.run {
                    currUserServers = userServers
                }
            } catch let error {
                let err = responseError(error)
                logger.error("saveServers setUserProtocolServers error: \(err)")
                await MainActor.run {
                    showAlert(
                        NSLocalizedString("Error saving servers", comment: "alert title"),
                        message: responseError(error)
                    )
                }
            }
        }
    }
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers()
    }
}
