//
//  OperatorView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

import SwiftUI
import SimpleXChat

struct OperatorView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme
    @Environment(\.editMode) private var editMode
    @Binding var userServers: [UserOperatorServers]
    var operatorServersIndex: Int
    @State var serverOperatorToEdit: ServerOperator
    @State var useOperator: Bool
    @State private var useOperatorToggleReset: Bool = false
    @State private var showConditionsSheet: Bool = false
    @State private var selectedServer: String? = nil
    @State private var testing = false

    var body: some View {
        operatorView()
            .opacity(testing ? 0.4 : 1)
            .overlay {
                if testing {
                    ProgressView()
                        .scaleEffect(2)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                }
            }
            .allowsHitTesting(!testing)
    }

    private func operatorView() -> some View {
        VStack {
            List {
                Section {
                    infoViewLink()
                    useOperatorToggle()
                } header: {
                    Text("Operator")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    switch (serverOperatorToEdit.conditionsAcceptance) {
                    case let .accepted(acceptedAt):
                        if let acceptedAt = acceptedAt {
                            Text("Conditions accepted on: \(conditionsTimestamp(acceptedAt)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    case let .required(deadline):
                        if serverOperatorToEdit.enabled, let deadline = deadline {
                            Text("Conditions will be considered accepted after: \(conditionsTimestamp(deadline)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }
                }

                if serverOperatorToEdit.enabled {
                    if !userServers[operatorServersIndex].smpServers.filter({ !$0.deleted }).isEmpty {
                        Section(header: Text("Use for messages").foregroundColor(theme.colors.secondary)) {
                            Toggle("For receiving", isOn: $serverOperatorToEdit.smpRoles.storage)
                            Toggle("For private routing", isOn: $serverOperatorToEdit.smpRoles.proxy)
                        }
                    }

                    // Preset servers can't be deleted
                    if !userServers[operatorServersIndex].smpServers.filter({ $0.preset }).isEmpty {
                        Section {
                            ForEach($userServers[operatorServersIndex].smpServers) { srv in
                                if srv.wrappedValue.preset {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .smp,
                                        backLabel: "\(serverOperatorToEdit.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                        } header: {
                            Text("Message servers")
                                .foregroundColor(theme.colors.secondary)
                        } footer: {
                            Text("The servers for new connections of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                                .foregroundColor(theme.colors.secondary)
                                .lineLimit(10)
                        }
                    }

                    if !userServers[operatorServersIndex].smpServers.filter({ !$0.preset && !$0.deleted }).isEmpty {
                        Section {
                            ForEach($userServers[operatorServersIndex].smpServers) { srv in
                                if !srv.wrappedValue.preset && !srv.wrappedValue.deleted {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .smp,
                                        backLabel: "\(serverOperatorToEdit.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                            .onDelete { indexSet in deleteSMPServer($userServers, operatorServersIndex, indexSet) }
                        } header: {
                            Text("Added message servers")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }

                    if !userServers[operatorServersIndex].xftpServers.filter({ !$0.deleted }).isEmpty {
                        Section(header: Text("Use for files").foregroundColor(theme.colors.secondary)) {
                            Toggle("For sending", isOn: $serverOperatorToEdit.xftpRoles.storage)
                        }
                    }

                    // Preset servers can't be deleted
                    if !userServers[operatorServersIndex].xftpServers.filter({ $0.preset }).isEmpty {
                        Section {
                            ForEach($userServers[operatorServersIndex].xftpServers) { srv in
                                if srv.wrappedValue.preset {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .xftp,
                                        backLabel: "\(serverOperatorToEdit.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                        } header: {
                            Text("Media & file servers")
                                .foregroundColor(theme.colors.secondary)
                        } footer: {
                            Text("The servers for new files of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                                .foregroundColor(theme.colors.secondary)
                                .lineLimit(10)
                        }
                    }

                    if !userServers[operatorServersIndex].xftpServers.filter({ !$0.preset && !$0.deleted }).isEmpty {
                        Section {
                            ForEach($userServers[operatorServersIndex].xftpServers) { srv in
                                if !srv.wrappedValue.preset && !srv.wrappedValue.deleted {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .xftp,
                                        backLabel: "\(serverOperatorToEdit.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                            .onDelete { indexSet in deleteXFTPServer($userServers, operatorServersIndex, indexSet) }
                        } header: {
                            Text("Added media & file servers")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }

                    Section {
                        TestServersButton(
                            smpServers: $userServers[operatorServersIndex].smpServers,
                            xftpServers: $userServers[operatorServersIndex].xftpServers,
                            testing: $testing
                        )
                    }
                }
            }
        }
        .toolbar {
            if (
                !userServers[operatorServersIndex].smpServers.filter({ !$0.preset && !$0.deleted }).isEmpty ||
                !userServers[operatorServersIndex].xftpServers.filter({ !$0.preset && !$0.deleted }).isEmpty
            ) {
                EditButton()
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            userServers[operatorServersIndex].operator = serverOperatorToEdit
            ChatModel.shared.updateServerOperator(serverOperatorToEdit)
            dismiss()
        })
        .sheet(isPresented: $showConditionsSheet, onDismiss: onUseToggleSheetDismissed) {
            SingleOperatorUsageConditionsView(serverOperator: $serverOperatorToEdit, serverOperatorToEdit: serverOperatorToEdit)
                .modifier(ThemedBackground(grouped: true))
        }
    }

    private func infoViewLink() -> some View {
        NavigationLink() {
            OperatorInfoView(serverOperator: serverOperatorToEdit)
                .navigationBarTitle("Operator information")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Image(serverOperatorToEdit.logo(colorScheme))
                    .resizable()
                    .scaledToFit()
                    .grayscale(serverOperatorToEdit.enabled ? 0.0 : 1.0)
                    .frame(width: 24, height: 24)
                Text(serverOperatorToEdit.tradeName)
            }
        }
    }

    private func useOperatorToggle() -> some View {
        Toggle("Use operator", isOn: $useOperator)
            .onChange(of: useOperator) { useOperatorToggle in
                if useOperatorToggleReset {
                    useOperatorToggleReset = false
                } else if useOperatorToggle {
                    switch serverOperatorToEdit.conditionsAcceptance {
                    case .accepted:
                        serverOperatorToEdit.enabled = true
                        ChatModel.shared.updateServerOperator(serverOperatorToEdit)
                    case let .required(deadline):
                        if deadline == nil {
                            showConditionsSheet = true
                        } else {
                            serverOperatorToEdit.enabled = true
                            ChatModel.shared.updateServerOperator(serverOperatorToEdit)
                        }
                    }
                } else {
                    serverOperatorToEdit.enabled = false
                    ChatModel.shared.updateServerOperator(serverOperatorToEdit)
                }
            }
    }

    private func onUseToggleSheetDismissed() {
        if useOperator && !serverOperatorToEdit.enabled {
            if serverOperatorToEdit.conditionsAcceptance.usageAllowed {
                serverOperatorToEdit.enabled = true
                ChatModel.shared.updateServerOperator(serverOperatorToEdit)
            } else {
                useOperatorToggleReset = true
                useOperator = false
            }
        }
    }
}

func conditionsTimestamp(_ date: Date) -> String {
    let localDateFormatter = DateFormatter()
    localDateFormatter.dateStyle = .medium
    localDateFormatter.timeStyle = .none
    return localDateFormatter.string(from: date)
}

struct OperatorInfoView: View {
    @EnvironmentObject var theme: AppTheme
    var serverOperator: ServerOperator

    var body: some View {
        VStack {
            List {
                Section(header: Text("Description").foregroundColor(theme.colors.secondary)) {
                    Text(serverOperator.info.description)
                }
                Section(header: Text("Website").foregroundColor(theme.colors.secondary)) {
                    Link("\(serverOperator.info.website)", destination: URL(string: serverOperator.info.website)!)
                }
            }
        }
    }
}

struct ConditionsTextView: View {
    @State private var conditionsData: (UsageConditions, String?, UsageConditions?)?
    @State private var failedToLoad: Bool = false

    let defaultConditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md"

    var body: some View {
        viewBody()
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .task {
                do {
                    conditionsData = try await getUsageConditions()
                } catch let error {
                    logger.error("ConditionsTextView getUsageConditions error: \(responseError(error))")
                    failedToLoad = true
                }
            }
    }

    @ViewBuilder private func viewBody() -> some View {
        if let (usageConditions, conditionsText, acceptedConditions) = conditionsData {
            if let conditionsText = conditionsText {
                ScrollView {
                    Text(conditionsText.trimmingCharacters(in: .whitespacesAndNewlines))
                        .padding()
                }
                .background(
                    RoundedRectangle(cornerRadius: 12, style: .continuous)
                        .fill(Color(uiColor: .secondarySystemGroupedBackground))
                )
            } else {
                let conditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/\(usageConditions.conditionsCommit)/PRIVACY.md"
                conditionsLinkView(conditionsLink)
            }
        } else if failedToLoad {
            conditionsLinkView(defaultConditionsLink)
        } else {
            ProgressView()
                .scaleEffect(2)
        }
    }

    private func conditionsLinkView(_ conditionsLink: String) -> some View {
        VStack(alignment: .leading, spacing: 20) {
            Text("Current conditions text couldn't be loaded, you can review conditions via this link:")
            Link(destination: URL(string: conditionsLink)!) {
                Text(conditionsLink)
                    .multilineTextAlignment(.leading)
            }
        }
    }
}

struct SingleOperatorUsageConditionsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var serverOperator: ServerOperator
    @State var serverOperatorToEdit: ServerOperator
    @State private var usageConditionsNavLinkActive: Bool = false

    var body: some View {
        viewBody()
    }

    @ViewBuilder private func viewBody() -> some View {
        let operatorsWithConditionsAccepted = ChatModel.shared.conditions.serverOperators.filter { $0.conditionsAcceptance.conditionsAccepted }
        if case .accepted = serverOperator.conditionsAcceptance {

            // In current UI implementation this branch doesn't get shown - as conditions can't be opened from inside operator once accepted
            VStack(alignment: .leading, spacing: 20) {
                Group {
                    viewHeader()
                    ConditionsTextView()
                        .padding(.bottom)
                        .padding(.bottom)
                }
                .padding(.horizontal)
            }
            .frame(maxHeight: .infinity)

        } else if !operatorsWithConditionsAccepted.isEmpty {

            NavigationView {
                VStack(alignment: .leading, spacing: 20) {
                    Group {
                        viewHeader()
                        Text("Conditions are already accepted for following operator(s): **\(operatorsWithConditionsAccepted.map { $0.legalName_ }.joined(separator: ", "))**.")
                        Text("Same conditions will apply to operator **\(serverOperator.legalName_)**.")
                        conditionsAppliedToOtherOperatorsText()
                        usageConditionsNavLinkButton()

                        Spacer()

                        acceptConditionsButton()
                            .padding(.bottom)
                            .padding(.bottom)
                    }
                    .padding(.horizontal)
                }
                .frame(maxHeight: .infinity)
            }

        } else {

            VStack(alignment: .leading, spacing: 20) {
                Group {
                    viewHeader()
                    Text("In order to use operator **\(serverOperator.legalName_)**, accept conditions of use.")
                    conditionsAppliedToOtherOperatorsText()
                    ConditionsTextView()
                    acceptConditionsButton()
                        .padding(.bottom)
                        .padding(.bottom)
                }
                .padding(.horizontal)
            }
            .frame(maxHeight: .infinity)

        }
    }

    private func viewHeader() -> some View {
        Text("Use operator \(serverOperator.tradeName)")
            .font(.largeTitle)
            .bold()
            .padding(.top)
            .padding(.top)
    }

    @ViewBuilder private func conditionsAppliedToOtherOperatorsText() -> some View {
        let otherOperatorsToApply = ChatModel.shared.conditions.serverOperators.filter {
            $0.enabled &&
            !$0.conditionsAcceptance.conditionsAccepted &&
            $0.operatorId != serverOperator.operatorId
        }
        if !otherOperatorsToApply.isEmpty {
            Text("Conditions will also apply for following operator(s) you use: **\(otherOperatorsToApply.map { $0.legalName_ }.joined(separator: ", "))**.")
        }
    }

    private func acceptConditionsButton() -> some View {
        Button {
            // Should call api to save state here, not when saving all servers
            // (It's counterintuitive to lose to closed sheet or Reset)
            let acceptedAt = Date.now
            ChatModel.shared.acceptConditionsForEnabledOperators(acceptedAt)
            serverOperatorToEdit.conditionsAcceptance = .accepted(acceptedAt: acceptedAt)
            serverOperator = serverOperatorToEdit
            dismiss()
        } label: {
            Text("Accept conditions")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }

    private func usageConditionsNavLinkButton() -> some View {
        ZStack {
            Button {
                usageConditionsNavLinkActive = true
            } label: {
                Text("View conditions")
            }

            NavigationLink(isActive: $usageConditionsNavLinkActive) {
                usageConditionsDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func usageConditionsDestinationView() -> some View {
        VStack(spacing: 20) {
            ConditionsTextView()
                .padding(.top)

            acceptConditionsButton()
                .padding(.bottom)
                .padding(.bottom)
        }
        .padding(.horizontal)
        .navigationTitle("Conditions of use")
        .navigationBarTitleDisplayMode(.large)
        .modifier(ThemedBackground(grouped: true))
    }
}

struct UsageConditionsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    var onSheet: Bool
    var conditionsAction: UsageConditionsAction
    var onAcceptAction: (() -> Void)?

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            if onSheet {
                Text("Conditions of use")
                    .font(.largeTitle)
                    .bold()
                    .padding(.top)
                    .padding(.top)
            }

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
            acceptForOperators(operators.map { $0.operatorId })
            onAcceptAction?()
            if onSheet{
                dismiss()
            }
        } label: {
            Text("Accept conditions")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }
}

func acceptForOperators(_ operatorIds: [Int64]) {
    Task {
        do {
            let conditionsId = ChatModel.shared.conditions.currentConditions.conditionsId
            let r = try await acceptConditions(conditionsId: conditionsId, operatorIds: operatorIds)
            await MainActor.run {
                ChatModel.shared.conditions = r
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

#Preview {
    OperatorView(
        userServers: Binding.constant([UserOperatorServers.sampleData1]),
        operatorServersIndex: 1,
        serverOperatorToEdit: ServerOperator.sampleData1,
        useOperator: ServerOperator.sampleData1.enabled
    )
}
