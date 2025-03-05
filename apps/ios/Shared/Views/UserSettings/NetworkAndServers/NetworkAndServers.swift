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
    case showConditions

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
    @EnvironmentObject var ss: SaveableSettings
    @State private var sheetItem: NetworkAndServersSheet? = nil
    @State private var justOpened = true
    @State private var showSaveDialog = false

    var body: some View {
        VStack {
            List {
                let conditionsAction = m.conditions.conditionsAction
                let anyOperatorEnabled = ss.servers.userServers.contains(where: { $0.operator?.enabled ?? false })
                Section {
                    ForEach(ss.servers.userServers.enumerated().map { $0 }, id: \.element.id) { idx, userOperatorServers in
                        if let serverOperator = userOperatorServers.operator {
                            serverOperatorView(idx, serverOperator)
                        } else {
                            EmptyView()
                        }
                    }

                    if let conditionsAction = conditionsAction, anyOperatorEnabled {
                        conditionsButton(conditionsAction)
                    }
                } header: {
                    Text("Preset servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    switch conditionsAction {
                    case let .review(_, deadline, _):
                        if let deadline = deadline, anyOperatorEnabled {
                            Text("Conditions will be accepted on: \(conditionsTimestamp(deadline)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    default:
                        EmptyView()
                    }
                }

                Section {
                    if let idx = ss.servers.userServers.firstIndex(where: { $0.operator == nil }) {
                        NavigationLink {
                            YourServersView(
                                userServers: $ss.servers.userServers,
                                serverErrors: $ss.servers.serverErrors,
                                operatorIndex: idx
                            )
                            .navigationTitle("Your servers")
                            .modifier(ThemedBackground(grouped: true))
                        } label: {
                            HStack {
                                Text("Your servers")
                                
                                if ss.servers.userServers[idx] != ss.servers.currUserServers[idx] {
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
                    Button("Save servers", action: { saveServers($ss.servers.currUserServers, $ss.servers.userServers) })
                        .disabled(!serversCanBeSaved(ss.servers.currUserServers, ss.servers.userServers, ss.servers.serverErrors))
                } footer: {
                    if let errStr = globalServersError(ss.servers.serverErrors) {
                        ServersErrorView(errStr: errStr)
                    } else if !ss.servers.serverErrors.isEmpty {
                        ServersErrorView(errStr: NSLocalizedString("Errors in servers configuration.", comment: "servers error"))
                    }
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
                    ss.servers.currUserServers = try await getUserServers()
                    ss.servers.userServers = ss.servers.currUserServers
                    ss.servers.serverErrors = []
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
            if serversCanBeSaved(ss.servers.currUserServers, ss.servers.userServers, ss.servers.serverErrors) {
                showSaveDialog = true
            } else {
                dismiss()
            }
        })
        .confirmationDialog("Save servers?", isPresented: $showSaveDialog, titleVisibility: .visible) {
            Button("Save") {
                saveServers($ss.servers.currUserServers, $ss.servers.userServers)
                dismiss()
            }
            Button("Exit without saving") { dismiss() }
        }
        .sheet(item: $sheetItem) { item in
            switch item {
            case .showConditions:
                UsageConditionsView(
                    currUserServers: $ss.servers.currUserServers,
                    userServers: $ss.servers.userServers
                )
                .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    private func serverOperatorView(_ operatorIndex: Int, _ serverOperator: ServerOperator) -> some View {
        NavigationLink() {
            OperatorView(
                currUserServers: $ss.servers.currUserServers,
                userServers: $ss.servers.userServers,
                serverErrors: $ss.servers.serverErrors,
                operatorIndex: operatorIndex,
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

                if ss.servers.userServers[operatorIndex] != ss.servers.currUserServers[operatorIndex] {
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
            sheetItem = .showConditions
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
    @Binding var currUserServers: [UserOperatorServers]
    @Binding var userServers: [UserOperatorServers]

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            switch ChatModel.shared.conditions.conditionsAction {

            case .none:
                regularConditionsHeader()
                    .padding(.top)
                    .padding(.top)
                ConditionsTextView()
                    .padding(.bottom)
                    .padding(.bottom)

            case let .review(operators, deadline, _):
                HStack {
                    Text("Updated conditions").font(.largeTitle).bold()
                }
                .padding(.top)
                .padding(.top)

                Text("Conditions will be accepted for the operator(s): **\(operators.map { $0.legalName_ }.joined(separator: ", "))**.")
                ConditionsTextView()
                VStack(spacing: 8) {
                    acceptConditionsButton(operators.map { $0.operatorId })
                    if let deadline = deadline {
                        Text("Conditions will be automatically accepted for enabled operators on: \(conditionsTimestamp(deadline)).")
                            .foregroundColor(theme.colors.secondary)
                            .font(.footnote)
                            .multilineTextAlignment(.center)
                            .frame(maxWidth: .infinity, alignment: .center)
                            .padding(.horizontal, 32)
                        conditionsDiffButton(.footnote)
                    } else {
                        conditionsDiffButton()
                            .padding(.top)
                    }
                }
                .padding(.bottom)
                .padding(.bottom)
                

            case let .accepted(operators):
                regularConditionsHeader()
                    .padding(.top)
                    .padding(.top)
                Text("Conditions are accepted for the operator(s): **\(operators.map { $0.legalName_ }.joined(separator: ", "))**.")
                ConditionsTextView()
                    .padding(.bottom)
                    .padding(.bottom)
            }
        }
        .padding(.horizontal, 25)
        .frame(maxHeight: .infinity)
    }

    private func acceptConditionsButton(_ operatorIds: [Int64]) -> some View {
        Button {
            acceptForOperators(operatorIds)
        } label: {
            Text("Accept conditions")
        }
        .buttonStyle(OnboardingButtonStyle())
    }

    func acceptForOperators(_ operatorIds: [Int64]) {
        Task {
            do {
                let conditionsId = ChatModel.shared.conditions.currentConditions.conditionsId
                let r = try await acceptConditions(conditionsId: conditionsId, operatorIds: operatorIds)
                await MainActor.run {
                    ChatModel.shared.conditions = r
                    updateOperatorsConditionsAcceptance($currUserServers, r.serverOperators)
                    updateOperatorsConditionsAcceptance($userServers, r.serverOperators)
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
    }

    @ViewBuilder private func conditionsDiffButton(_ font: Font? = nil) -> some View {
        let commit = ChatModel.shared.conditions.currentConditions.conditionsCommit
        if let commitUrl = URL(string: "https://github.com/simplex-chat/simplex-chat/commit/\(commit)") {
            Link(destination: commitUrl) {
                HStack {
                    Text("Open changes")
                    Image(systemName: "arrow.up.right.circle")
                }
                .font(font)
            }
        }
    }
}

private func regularConditionsHeader() -> some View {
    HStack {
        Text("Conditions of use").font(.largeTitle).bold()
        Spacer()
        conditionsLinkButton()
    }
}

struct SimpleConditionsView: View {

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            regularConditionsHeader()
                .padding(.top)
                .padding(.top)
            ConditionsTextView()
                .padding(.bottom)
                .padding(.bottom)
        }
        .padding(.horizontal, 25)
        .frame(maxHeight: .infinity)
    }
}

func validateServers_(_ userServers: Binding<[UserOperatorServers]>, _ serverErrors: Binding<[UserServersError]>) {
    let userServersToValidate = userServers.wrappedValue
    Task {
        do {
            let errs = try await validateServers(userServers: userServersToValidate)
            await MainActor.run {
                serverErrors.wrappedValue = errs
            }
        } catch let error {
            logger.error("validateServers error: \(responseError(error))")
        }
    }
}

func serversCanBeSaved(
    _ currUserServers: [UserOperatorServers],
    _ userServers: [UserOperatorServers],
    _ serverErrors: [UserServersError]
) -> Bool {
    return userServers != currUserServers && serverErrors.isEmpty
}

struct ServersErrorView: View {
    @EnvironmentObject var theme: AppTheme
    var errStr: String

    var body: some View {
        HStack {
            Image(systemName: "exclamationmark.circle")
                .foregroundColor(.red)
            Text(errStr)
                .foregroundColor(theme.colors.secondary)
        }
    }
}

func globalServersError(_ serverErrors: [UserServersError]) -> String? {
    for err in serverErrors {
        if let errStr = err.globalError {
            return errStr
        }
    }
    return nil
}

func globalSMPServersError(_ serverErrors: [UserServersError]) -> String? {
    for err in serverErrors {
        if let errStr = err.globalSMPError {
            return errStr
        }
    }
    return nil
}

func globalXFTPServersError(_ serverErrors: [UserServersError]) -> String? {
    for err in serverErrors {
        if let errStr = err.globalXFTPError {
            return errStr
        }
    }
    return nil
}

func findDuplicateHosts(_ serverErrors: [UserServersError]) -> Set<String> {
    let duplicateHostsList = serverErrors.compactMap { err in
        if case let .duplicateServer(_, _, duplicateHost) = err {
            return duplicateHost
        } else {
            return nil
        }
    }
    return Set(duplicateHostsList)
}

func saveServers(_ currUserServers: Binding<[UserOperatorServers]>, _ userServers: Binding<[UserOperatorServers]>) {
    let userServersToSave = userServers.wrappedValue
    Task {
        do {
            try await setUserServers(userServers: userServersToSave)
            // Get updated servers to learn new server ids (otherwise it messes up delete of newly added and saved servers)
            do {
                let updatedServers = try await getUserServers()
                let updatedOperators = try await getServerOperators()
                await MainActor.run {
                    ChatModel.shared.conditions = updatedOperators
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

func updateOperatorsConditionsAcceptance(_ usvs: Binding<[UserOperatorServers]>, _ updatedOperators: [ServerOperator]) {
    for i in 0..<usvs.wrappedValue.count {
        if let updatedOperator = updatedOperators.first(where: { $0.operatorId == usvs.wrappedValue[i].operator?.operatorId }) {
            usvs.wrappedValue[i].operator?.conditionsAcceptance = updatedOperator.conditionsAcceptance
        }
    }
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers()
    }
}
