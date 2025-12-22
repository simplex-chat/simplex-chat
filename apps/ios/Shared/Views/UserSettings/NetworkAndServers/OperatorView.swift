//
//  OperatorView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import Ink

struct OperatorView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @EnvironmentObject var theme: AppTheme
    @Environment(\.editMode) private var editMode
    @Binding var currUserServers: [UserOperatorServers]
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
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
        let duplicateHosts = findDuplicateHosts(serverErrors)
        return VStack {
            List {
                Section {
                    infoViewLink()
                    useOperatorToggle()
                } header: {
                    Text("Operator")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    if let errStr = globalServersError(serverErrors) {
                        ServersErrorView(errStr: errStr)
                    } else {
                        switch (userServers[operatorIndex].operator_.conditionsAcceptance) {
                        case let .accepted(acceptedAt, _):
                            if let acceptedAt = acceptedAt {
                                Text("Conditions accepted on: \(conditionsTimestamp(acceptedAt)).")
                                    .foregroundColor(theme.colors.secondary)
                            }
                        case let .required(deadline):
                            if userServers[operatorIndex].operator_.enabled, let deadline = deadline {
                                Text("Conditions will be accepted on: \(conditionsTimestamp(deadline)).")
                                    .foregroundColor(theme.colors.secondary)
                            }
                        }
                    }
                }

                if userServers[operatorIndex].operator_.enabled {
                    if !userServers[operatorIndex].smpServers.filter({ !$0.deleted }).isEmpty {
                        Section {
                            Toggle("To receive", isOn: $userServers[operatorIndex].operator_.smpRoles.storage)
                                .onChange(of: userServers[operatorIndex].operator_.smpRoles.storage) { _ in
                                    validateServers_($userServers, $serverErrors)
                                }
                            Toggle("For private routing", isOn: $userServers[operatorIndex].operator_.smpRoles.proxy)
                                .onChange(of: userServers[operatorIndex].operator_.smpRoles.proxy) { _ in
                                    validateServers_($userServers, $serverErrors)
                                }
                        } header: {
                            Text("Use for messages")
                                .foregroundColor(theme.colors.secondary)
                        } footer: {
                            if let errStr = globalSMPServersError(serverErrors) {
                                ServersErrorView(errStr: errStr)
                            }
                        }
                    }

                    // Preset servers can't be deleted
                    if !userServers[operatorIndex].smpServers.filter({ $0.preset }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].smpServers) { srv in
                                if srv.wrappedValue.preset {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        serverErrors: $serverErrors,
                                        duplicateHosts: duplicateHosts,
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
                            if let errStr = globalSMPServersError(serverErrors) {
                                ServersErrorView(errStr: errStr)
                            } else {
                                Text("The servers for new connections of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                                    .foregroundColor(theme.colors.secondary)
                                    .lineLimit(10)
                            }
                        }
                    }

                    if !userServers[operatorIndex].smpServers.filter({ !$0.preset && !$0.deleted }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].smpServers) { srv in
                                if !srv.wrappedValue.preset && !srv.wrappedValue.deleted {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        serverErrors: $serverErrors,
                                        duplicateHosts: duplicateHosts,
                                        server: srv,
                                        serverProtocol: .smp,
                                        backLabel: "\(userServers[operatorIndex].operator_.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                            .onDelete { indexSet in
                                deleteSMPServer($userServers, operatorIndex, indexSet)
                                validateServers_($userServers, $serverErrors)
                            }
                        } header: {
                            Text("Added message servers")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }

                    if !userServers[operatorIndex].xftpServers.filter({ !$0.deleted }).isEmpty {
                        Section {
                            Toggle("To send", isOn: $userServers[operatorIndex].operator_.xftpRoles.storage)
                                .onChange(of: userServers[operatorIndex].operator_.xftpRoles.storage) { _ in
                                    validateServers_($userServers, $serverErrors)
                                }
                        } header: {
                            Text("Use for files")
                                .foregroundColor(theme.colors.secondary)
                        } footer: {
                            if let errStr = globalXFTPServersError(serverErrors) {
                                ServersErrorView(errStr: errStr)
                            }
                        }
                    }

                    // Preset servers can't be deleted
                    if !userServers[operatorIndex].xftpServers.filter({ $0.preset }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].xftpServers) { srv in
                                if srv.wrappedValue.preset {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        serverErrors: $serverErrors,
                                        duplicateHosts: duplicateHosts,
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
                            if let errStr = globalXFTPServersError(serverErrors) {
                                ServersErrorView(errStr: errStr)
                            } else {
                                Text("The servers for new files of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                                    .foregroundColor(theme.colors.secondary)
                                    .lineLimit(10)
                            }
                        }
                    }

                    if !userServers[operatorIndex].xftpServers.filter({ !$0.preset && !$0.deleted }).isEmpty {
                        Section {
                            ForEach($userServers[operatorIndex].xftpServers) { srv in
                                if !srv.wrappedValue.preset && !srv.wrappedValue.deleted {
                                    ProtocolServerViewLink(
                                        userServers: $userServers,
                                        serverErrors: $serverErrors,
                                        duplicateHosts: duplicateHosts,
                                        server: srv,
                                        serverProtocol: .xftp,
                                        backLabel: "\(userServers[operatorIndex].operator_.tradeName) servers",
                                        selectedServer: $selectedServer
                                    )
                                } else {
                                    EmptyView()
                                }
                            }
                            .onDelete { indexSet in
                                deleteXFTPServer($userServers, operatorIndex, indexSet)
                                validateServers_($userServers, $serverErrors)
                            }
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
                serverErrors: $serverErrors,
                operatorIndex: operatorIndex
            )
            .modifier(ThemedBackground(grouped: true))
        }
    }

    private func infoViewLink() -> some View {
        NavigationLink() {
            OperatorInfoView(serverOperator: userServers[operatorIndex].operator_)
                .navigationBarTitle("Network operator")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
        } label: {
            Image(userServers[operatorIndex].operator_.largeLogo(colorScheme))
                .resizable()
                .scaledToFit()
                .grayscale(userServers[operatorIndex].operator_.enabled ? 0.0 : 1.0)
                .frame(height: 40)
        }
    }

    private func useOperatorToggle() -> some View {
        Toggle("Use servers", isOn: $useOperator)
            .onChange(of: useOperator) { useOperatorToggle in
                if useOperatorToggleReset {
                    useOperatorToggleReset = false
                } else if useOperatorToggle {
                    switch userServers[operatorIndex].operator_.conditionsAcceptance {
                    case .accepted:
                        userServers[operatorIndex].operator_.enabled = true
                        validateServers_($userServers, $serverErrors)
                    case let .required(deadline):
                        if deadline == nil {
                            showConditionsSheet = true
                        } else {
                            userServers[operatorIndex].operator_.enabled = true
                            validateServers_($userServers, $serverErrors)
                        }
                    }
                } else {
                    userServers[operatorIndex].operator_.enabled = false
                    validateServers_($userServers, $serverErrors)
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
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    var serverOperator: ServerOperator

    var body: some View {
        VStack {
            List {
                Section {
                    VStack(alignment: .leading) {
                        Image(serverOperator.largeLogo(colorScheme))
                            .resizable()
                            .scaledToFit()
                            .frame(height: 48)
                        if let legalName = serverOperator.legalName {
                            Text(legalName)
                        }
                    }
                }
                Section {
                    VStack(alignment: .leading, spacing: 12) {
                        ForEach(serverOperator.info.description, id: \.self) { d in
                            Text(d)
                        }
                    }
                    Link(serverOperator.info.website.absoluteString, destination: serverOperator.info.website)
                }
                if let selfhost = serverOperator.info.selfhost {
                    Section {
                        Link(selfhost.text, destination: selfhost.link)
                    }
                }
            }
        }
    }
}

struct ConditionsTextView: View {
    @State private var conditionsData: (UsageConditions, String?, UsageConditions?)?
    @State private var failedToLoad: Bool = false
    @State private var conditionsHTML: String? = nil

    let defaultConditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md"

    var body: some View {
        viewBody()
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .task {
                do {
                    let conditions = try await getUsageConditions()
                    let conditionsText = conditions.1
                    let parentLink =  "https://github.com/simplex-chat/simplex-chat/blob/\(conditions.0.conditionsCommit)"
                    let preparedText: String?
                    if let conditionsText {
                        let prepared = prepareMarkdown(conditionsText.trimmingCharacters(in: .whitespacesAndNewlines), parentLink)
                        conditionsHTML = MarkdownParser().html(from: prepared)
                        preparedText = prepared
                    } else {
                        preparedText = nil
                    }
                    conditionsData = (conditions.0, preparedText, conditions.2)
                } catch let error {
                    logger.error("ConditionsTextView getUsageConditions error: \(responseError(error))")
                    failedToLoad = true
                }
            }
    }

    // TODO Diff rendering
    @ViewBuilder private func viewBody() -> some View {
        if let (usageConditions, _, _) = conditionsData {
            if let conditionsHTML {
                ConditionsWebView(html: conditionsHTML)
                    .padding(6)
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

    private func prepareMarkdown(_ text: String, _ parentLink: String) -> String {
        let localLinkRegex = try! NSRegularExpression(pattern: "\\[([^\\(]*)\\]\\(#.*\\)")
        let h1Regex = try! NSRegularExpression(pattern: "^# ")
        var text = localLinkRegex.stringByReplacingMatches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count), withTemplate: "$1")
        text = h1Regex.stringByReplacingMatches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count), withTemplate: "")
        return text
            .replacingOccurrences(of: "](/", with: "](\(parentLink)/")
            .replacingOccurrences(of: "](./", with: "](\(parentLink)/")
    }
}

struct SingleOperatorUsageConditionsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var currUserServers: [UserOperatorServers]
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    var operatorIndex: Int

    var body: some View {
        viewBody()
    }

    @ViewBuilder private func viewBody() -> some View {
        let operatorsWithConditionsAccepted = ChatModel.shared.conditions.serverOperators.filter { $0.conditionsAcceptance.conditionsAccepted }
        if case .accepted = userServers[operatorIndex].operator_.conditionsAcceptance {

            // In current UI implementation this branch doesn't get shown - as conditions can't be opened from inside operator once accepted
            VStack(alignment: .leading, spacing: 20) {
                viewHeader()
                ConditionsTextView()
            }
            .padding(.bottom)
            .padding(.bottom)
            .padding(.horizontal)
            .frame(maxHeight: .infinity)

        } else if !operatorsWithConditionsAccepted.isEmpty {

            NavigationView {
                VStack(alignment: .leading, spacing: 20) {
                    viewHeader()
                    Text("Conditions are already accepted for these operator(s): **\(operatorsWithConditionsAccepted.map { $0.legalName_ }.joined(separator: ", "))**.")
                    Text("The same conditions will apply to operator **\(userServers[operatorIndex].operator_.legalName_)**.")
                    conditionsAppliedToOtherOperatorsText()
                    Spacer()

                    acceptConditionsButton()
                    usageConditionsNavLinkButton()
                }
                .padding(.bottom)
                .padding(.bottom)
                .padding(.horizontal)
                .frame(maxHeight: .infinity)
            }

        } else {

            VStack(alignment: .leading, spacing: 20) {
                viewHeader()
                Text("To use the servers of **\(userServers[operatorIndex].operator_.legalName_)**, accept conditions of use.")
                conditionsAppliedToOtherOperatorsText()
                ConditionsTextView()
                acceptConditionsButton()
                    .padding(.bottom)
                    .padding(.bottom)
            }
            .padding(.horizontal)
            .frame(maxHeight: .infinity)

        }
    }

    private func viewHeader() -> some View {
        HStack {
            Text("Use \(userServers[operatorIndex].operator_.tradeName)").font(.largeTitle).bold()
            Spacer()
            conditionsLinkButton()
        }
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
            Text("These conditions will also apply for: **\(otherOperatorsToApply.map { $0.legalName_ }.joined(separator: ", "))**.")
        }
    }
    
    private func acceptConditionsButton() -> some View {
        let operatorIds = ChatModel.shared.conditions.serverOperators
            .filter {
                $0.operatorId == userServers[operatorIndex].operator_.operatorId || // Opened operator
                ($0.enabled && !$0.conditionsAcceptance.conditionsAccepted) // Other enabled operators with conditions not accepted
            }
            .map { $0.operatorId }
        return Button {
            acceptForOperators(operatorIds, operatorIndex)
        } label: {
            Text("Accept conditions")
        }
        .buttonStyle(OnboardingButtonStyle())
    }

    func acceptForOperators(_ operatorIds: [Int64], _ operatorIndexToEnable: Int) {
        Task {
            do {
                let conditionsId = ChatModel.shared.conditions.currentConditions.conditionsId
                let r = try await acceptConditions(conditionsId: conditionsId, operatorIds: operatorIds)
                await MainActor.run {
                    ChatModel.shared.conditions = r
                    updateOperatorsConditionsAcceptance($currUserServers, r.serverOperators)
                    updateOperatorsConditionsAcceptance($userServers, r.serverOperators)
                    userServers[operatorIndexToEnable].operator?.enabled = true
                    validateServers_($userServers, $serverErrors)
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

    private func usageConditionsNavLinkButton() -> some View {
        NavigationLink("View conditions") {
            ConditionsTextView()
                .padding()
                .navigationTitle("Conditions of use")
                .navigationBarTitleDisplayMode(.large)
                .toolbar { ToolbarItem(placement: .navigationBarTrailing, content: conditionsLinkButton) }
                .modifier(ThemedBackground(grouped: true))
        }
        .font(.callout)
        .frame(maxWidth: .infinity, alignment: .center)
    }
}

func conditionsLinkButton() -> some View {
    let commit = ChatModel.shared.conditions.currentConditions.conditionsCommit
    let mdUrl = URL(string: "https://github.com/simplex-chat/simplex-chat/blob/\(commit)/PRIVACY.md") ?? conditionsURL
    return Menu {
        Link(destination: mdUrl) {
            Label("Open conditions", systemImage: "doc")
        }
        if let commitUrl = URL(string: "https://github.com/simplex-chat/simplex-chat/commit/\(commit)") {
            Link(destination: commitUrl) {
                Label("Open changes", systemImage: "ellipsis")
            }
        }
    } label: {
        Image(systemName: "arrow.up.right.circle")
            .resizable()
            .scaledToFit()
            .frame(width: 20)
            .padding(2)
            .contentShape(Circle())
    }
}

#Preview {
    OperatorView(
        currUserServers: Binding.constant([UserOperatorServers.sampleData1, UserOperatorServers.sampleDataNilOperator]),
        userServers: Binding.constant([UserOperatorServers.sampleData1, UserOperatorServers.sampleDataNilOperator]),
        serverErrors: Binding.constant([]),
        operatorIndex: 1,
        useOperator: ServerOperator.sampleData1.enabled
    )
}
