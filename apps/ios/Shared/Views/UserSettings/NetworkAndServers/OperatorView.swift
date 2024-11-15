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
    @Binding var currUserServers: [UserOperatorServers]
    @Binding var userServers: [UserOperatorServers]
    var operatorIndex: Int
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
                    switch (userServers[operatorIndex].operator_.conditionsAcceptance) {
                    case let .accepted(acceptedAt):
                        if let acceptedAt = acceptedAt {
                            Text("Conditions accepted on: \(conditionsTimestamp(acceptedAt)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    case let .required(deadline):
                        if userServers[operatorIndex].operator_.enabled, let deadline = deadline {
                            Text("Conditions will be considered accepted after: \(conditionsTimestamp(deadline)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }
                }

                if userServers[operatorIndex].operator_.enabled {
                    if !userServers[operatorIndex].smpServers.filter({ !$0.deleted }).isEmpty {
                        Section(header: Text("Use for messages").foregroundColor(theme.colors.secondary)) {
                            Toggle("For receiving", isOn: $userServers[operatorIndex].operator_.smpRoles.storage)
                            Toggle("For private routing", isOn: $userServers[operatorIndex].operator_.smpRoles.proxy)
                        }
                    }

                    // Preset servers can't be deleted
                    if !userServers[operatorIndex].smpServers.filter({ $0.preset }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].smpServers) { srv in
                                if srv.wrappedValue.preset {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .smp,
                                        backLabel: "\(userServers[operatorIndex].operator_.tradeName) servers",
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

                    if !userServers[operatorIndex].smpServers.filter({ !$0.preset && !$0.deleted }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].smpServers) { srv in
                                if !srv.wrappedValue.preset && !srv.wrappedValue.deleted {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .smp,
                                        backLabel: "\(userServers[operatorIndex].operator_.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                            .onDelete { indexSet in deleteSMPServer($userServers, operatorIndex, indexSet) }
                        } header: {
                            Text("Added message servers")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }

                    if !userServers[operatorIndex].xftpServers.filter({ !$0.deleted }).isEmpty {
                        Section(header: Text("Use for files").foregroundColor(theme.colors.secondary)) {
                            Toggle("For sending", isOn: $userServers[operatorIndex].operator_.xftpRoles.storage)
                        }
                    }

                    // Preset servers can't be deleted
                    if !userServers[operatorIndex].xftpServers.filter({ $0.preset }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].xftpServers) { srv in
                                if srv.wrappedValue.preset {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .xftp,
                                        backLabel: "\(userServers[operatorIndex].operator_.tradeName) servers",
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

                    if !userServers[operatorIndex].xftpServers.filter({ !$0.preset && !$0.deleted }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].xftpServers) { srv in
                                if !srv.wrappedValue.preset && !srv.wrappedValue.deleted {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        server: srv,
                                        serverProtocol: .xftp,
                                        backLabel: "\(userServers[operatorIndex].operator_.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                            .onDelete { indexSet in deleteXFTPServer($userServers, operatorIndex, indexSet) }
                        } header: {
                            Text("Added media & file servers")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }

                    Section {
                        TestServersButton(
                            smpServers: $userServers[operatorIndex].smpServers,
                            xftpServers: $userServers[operatorIndex].xftpServers,
                            testing: $testing
                        )
                    }
                }
            }
        }
        .toolbar {
            if (
                !userServers[operatorIndex].smpServers.filter({ !$0.preset && !$0.deleted }).isEmpty ||
                !userServers[operatorIndex].xftpServers.filter({ !$0.preset && !$0.deleted }).isEmpty
            ) {
                EditButton()
            }
        }
        .sheet(isPresented: $showConditionsSheet, onDismiss: onUseToggleSheetDismissed) {
            SingleOperatorUsageConditionsView(
                currUserServers: $currUserServers,
                userServers: $userServers,
                operatorIndex: operatorIndex
            )
            .modifier(ThemedBackground(grouped: true))
        }
    }

    private func infoViewLink() -> some View {
        NavigationLink() {
            OperatorInfoView(serverOperator: userServers[operatorIndex].operator_)
                .navigationBarTitle("Operator information")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Image(userServers[operatorIndex].operator_.logo(colorScheme))
                    .resizable()
                    .scaledToFit()
                    .grayscale(userServers[operatorIndex].operator_.enabled ? 0.0 : 1.0)
                    .frame(width: 24, height: 24)
                Text(userServers[operatorIndex].operator_.tradeName)
            }
        }
    }

    private func useOperatorToggle() -> some View {
        Toggle("Use operator", isOn: $useOperator)
            .onChange(of: useOperator) { useOperatorToggle in
                if useOperatorToggleReset {
                    useOperatorToggleReset = false
                } else if useOperatorToggle {
                    switch userServers[operatorIndex].operator_.conditionsAcceptance {
                    case .accepted:
                        userServers[operatorIndex].operator_.enabled = true
                    case let .required(deadline):
                        if deadline == nil {
                            showConditionsSheet = true
                        } else {
                            userServers[operatorIndex].operator_.enabled = true
                        }
                    }
                } else {
                    userServers[operatorIndex].operator_.enabled = false
                }
            }
    }

    private func onUseToggleSheetDismissed() {
        if useOperator && !userServers[operatorIndex].operator_.conditionsAcceptance.usageAllowed {
            useOperatorToggleReset = true
            useOperator = false
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

    // TODO Markdown & diff rendering
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
    @Binding var currUserServers: [UserOperatorServers]
    @Binding var userServers: [UserOperatorServers]
    var operatorIndex: Int
    @State private var usageConditionsNavLinkActive: Bool = false

    var body: some View {
        viewBody()
    }

    @ViewBuilder private func viewBody() -> some View {
        let operatorsWithConditionsAccepted = ChatModel.shared.conditions.serverOperators.filter { $0.conditionsAcceptance.conditionsAccepted }
        if case .accepted = userServers[operatorIndex].operator_.conditionsAcceptance {

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
                        Text("Same conditions will apply to operator **\(userServers[operatorIndex].operator_.legalName_)**.")
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
                    Text("In order to use operator **\(userServers[operatorIndex].operator_.legalName_)**, accept conditions of use.")
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
        Text("Use operator \(userServers[operatorIndex].operator_.tradeName)")
            .font(.largeTitle)
            .bold()
            .padding(.top)
            .padding(.top)
    }

    @ViewBuilder private func conditionsAppliedToOtherOperatorsText() -> some View {
        let otherOperatorsToApply = ChatModel.shared.conditions.serverOperators.filter {
            $0.enabled &&
            !$0.conditionsAcceptance.conditionsAccepted &&
            $0.operatorId != userServers[operatorIndex].operator_.operatorId
        }
        if !otherOperatorsToApply.isEmpty {
            Text("Conditions will also apply for following operator(s) you use: **\(otherOperatorsToApply.map { $0.legalName_ }.joined(separator: ", "))**.")
        }
    }
    
    @ViewBuilder private func acceptConditionsButton() -> some View {
        let operatorIds = ChatModel.shared.conditions.serverOperators
            .filter {
                $0.operatorId == userServers[operatorIndex].operator_.operatorId || // Opened operator
                ($0.enabled && !$0.conditionsAcceptance.conditionsAccepted) // Other enabled operators with conditions not accepted
            }
            .map { $0.operatorId }
        Button {
            acceptForOperators($currUserServers, $userServers, operatorIndex, dismiss, operatorIds)
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

#Preview {
    OperatorView(
        currUserServers: Binding.constant([UserOperatorServers.sampleData1, UserOperatorServers.sampleDataNilOperator]),
        userServers: Binding.constant([UserOperatorServers.sampleData1, UserOperatorServers.sampleDataNilOperator]),
        operatorIndex: 1,
        useOperator: ServerOperator.sampleData1.enabled
    )
}
