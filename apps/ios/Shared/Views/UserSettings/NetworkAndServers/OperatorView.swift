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
    @Binding var serverOperator: ServerOperator
    @State var serverOperatorToEdit: ServerOperator
    @State var useOperator: Bool
    @State private var useOperatorToggleReset: Bool = false
    @State private var showConditionsSheet: Bool = false
    @Binding var currSMPServers: [UserServer]
    @Binding var smpServers: [UserServer]
    @Binding var currXFTPServers: [UserServer]
    @Binding var xftpServers: [UserServer]
    @State private var selectedServer: String? = nil
    @State private var testing = false

    let smpStr: String = ServerProtocol.smp.rawValue.uppercased()
    let xftpStr: String = ServerProtocol.xftp.rawValue.uppercased()

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
                        } else {
                            Text("Conditions accepted.")
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
                    Section(header: Text("Use operator").foregroundColor(theme.colors.secondary)) {
                        Toggle("For storage", isOn: $serverOperatorToEdit.roles.storage)
                        Toggle("As proxy", isOn: $serverOperatorToEdit.roles.proxy)
                    }

                    Section {
                        ForEach($smpServers) { srv in
                            ProtocolServerViewLink(
                                server: srv,
                                serverProtocol: .smp,
                                preset: true,
                                backLabel: "\(serverOperator.tradeName) servers",
                                selectedServer: $selectedServer
                            )
                        }
                    } header: {
                        Text("\(smpStr) servers")
                            .foregroundColor(theme.colors.secondary)
                    } footer: {
                        Text("The servers for new connections of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(10)
                    }

                    Section {
                        ForEach($xftpServers) { srv in
                            ProtocolServerViewLink(
                                server: srv,
                                serverProtocol: .xftp,
                                preset: true,
                                backLabel: "\(serverOperator.tradeName) servers",
                                selectedServer: $selectedServer
                            )
                        }
                    } header: {
                        Text("\(xftpStr) servers")
                            .foregroundColor(theme.colors.secondary)
                    } footer: {
                        Text("The servers for new files of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(10)
                    }

                    Section {
                        TestServersButton(
                            smpServers: $smpServers,
                            xftpServers: $xftpServers,
                            testing: $testing
                        )
                    }
                }
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            serverOperator = serverOperatorToEdit
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
            OperatorInfoView(serverOperator: serverOperator)
                .navigationBarTitle("Operator information")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Image(serverOperator.logo(colorScheme))
                    .resizable()
                    .scaledToFit()
                    .grayscale(serverOperatorToEdit.enabled ? 0.0 : 1.0)
                    .frame(width: 24, height: 24)
                Text(serverOperator.tradeName)
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

let conditionsText = """
Lorem ipsum odor amet, consectetuer adipiscing elit. Blandit mauris massa tempor ac; maximus accumsan magnis. Sollicitudin maximus tempor luctus sociosqu turpis dictum per imperdiet porttitor. Efficitur mattis fusce curae id efficitur. Non bibendum elementum faucibus vehicula morbi pulvinar. Accumsan habitant tincidunt sollicitudin taciti ad urna potenti velit. Primis laoreet pharetra magnis est dolor proin viverra.

Laoreet auctor morbi a varius rutrum diam porta? In ad erat condimentum erat leo ornare. Eu venenatis inceptos rhoncus urna fringilla dis proin ante. Cras dignissim rutrum et faucibus feugiat neque curae tempus. Tellus ligula id dapibus, diam sollicitudin velit odio aliquam lectus. Maecenas ullamcorper arcu interdum cubilia donec iaculis. Maximus penatibus turpis a; vel fermentum ridiculus magna phasellus pellentesque. Eros tellus libero varius potenti; lobortis iaculis.

Mollis condimentum potenti velit at rutrum tellus maximus suscipit nec. Vehicula aenean dui netus enim aliquam. Aliquam libero rhoncus per pharetra accumsan eros. Urna non eu sem varius vivamus mus tellus aptent quam. Tristique mi natoque lectus volutpat facilisi commodo ac consequat. Proin parturient facilisi senectus egestas ultrices. Fringilla nisi urna convallis molestie lorem varius phasellus a ornare. Ullamcorper varius praesent facilisi habitasse massa.

Potenti dolor ridiculus est faucibus leo. Euismod consequat ultricies fringilla sociosqu duis sollicitudin. Eget convallis lacinia lacus justo per habitasse parturient. Donec nunc himenaeos pretium donec cursus pharetra ac phasellus? Fringilla sodales egestas orci ligula per ligula semper pellentesque. Potenti non dignissim tempor; orci rutrum elit.

Habitasse eu sapien eleifend gravida tortor potenti senectus euismod. Lectus enim fames turpis lectus facilisi efficitur elit porttitor facilisi. Nisl quam senectus quam augue integer leo. In aliquam tempor nibh proin felis tortor elementum sodales lacinia. Ut per placerat bibendum magna dapibus fermentum bibendum amet congue. Curae bibendum enim platea per faucibus imperdiet morbi hac varius. Conubia feugiat justo hac faucibus dis.
"""

struct ConditionsTextView: View {
    @State private var conditionsData: (UsageConditions, String?, UsageConditions?)?
    @State private var failedToLoad: Bool = false

    let defaultConditionsLink = "https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md"

    var body: some View {
        viewBody()
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .task {
                do {
                    // conditionsData = try await getUsageConditions()
                    conditionsData = (UsageConditions.sampleData, conditionsText, nil)
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
                    Text(conditionsText)
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
    @State private var conditionsExpanded: Bool = false

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            Group {
                Text("Use operator \(serverOperator.tradeName)")
                    .font(.largeTitle)
                    .bold()
                    .padding(.top)
                    .padding(.top)
                
                let operatorsWithConditionsAccepted = ChatModel.shared.serverOperators.filter { $0.conditionsAcceptance.conditionsAccepted }

                if case .accepted = serverOperator.conditionsAcceptance {
                    // In current UI this branch doesn't get shown - as conditions can't be opened from inside operator once accepted

                    ConditionsTextView()
                        .padding(.bottom)
                        .padding(.bottom)

                } else if !operatorsWithConditionsAccepted.isEmpty {
                    
                    Text("You already accepted conditions of use for following operator(s): **\(operatorsWithConditionsAccepted.map { $0.conditionsName }.joined(separator: ", "))**.")

                    Text("Same conditions will apply to operator **\(serverOperator.conditionsName)**.")

                    conditionsAppliedToOtherOperatorsText()
                    
                    if !conditionsExpanded {
                        Button {
                            conditionsExpanded = true
                        } label: {
                            Text("View conditions")
                        }

                        Spacer()
                    } else {
                        ConditionsTextView()
                    }
                    
                    acceptConditionsButton()
                        .padding(.bottom)
                        .padding(.bottom)

                } else {
                    
                    Text("In order to use operator **\(serverOperator.conditionsName)**, accept conditions of use.")

                    conditionsAppliedToOtherOperatorsText()
                    
                    ConditionsTextView()

                    acceptConditionsButton()
                        .padding(.bottom)
                        .padding(.bottom)

                }
            }
            .padding(.horizontal)
        }
        .frame(maxHeight: .infinity)
    }

    @ViewBuilder private func conditionsAppliedToOtherOperatorsText() -> some View {
        let otherEnabledOperators = ChatModel.shared.enabledOperatorsWithConditionsNotAccepted.filter { $0.operatorId != serverOperator.operatorId }
        if !otherEnabledOperators.isEmpty {
            Text("Conditions will also apply for following operator(s) you use: **\(otherEnabledOperators.map { $0.conditionsName }.joined(separator: ", "))**.")
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
        .buttonStyle(OnboardingButtonStyle())
    }
}

struct UsageConditionsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    var showTitle: Bool // When shown on sheet
    var dismissOnAccept: Bool // When shown on sheet
    var conditionsAction: UsageConditionsAction
    var onAcceptAction: ((Date) -> Void)

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            if showTitle {
                Text("Conditions of use")
                    .font(.largeTitle)
                    .bold()
                    .padding(.top)
                    .padding(.top)
            }
            
            switch conditionsAction {
            case let .review(operators, _, _):

                Text("Conditions will be accepted for following operator(s): **\(operators.map { $0.conditionsName }.joined(separator: ", "))**.")

                ConditionsTextView()

                acceptConditionsButton(operators)
                    .padding(.bottom)
                    .padding(.bottom)
                
            case let .accepted(operators):

                Text("Conditions are accepted for following operator(s): **\(operators.map { $0.conditionsName }.joined(separator: ", "))**.")

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
            // Should call api to save state here, not when saving all servers
            // (It's counterintuitive to lose to closed sheet or Reset)
            onAcceptAction(Date.now)
            if dismissOnAccept{
                dismiss()
            }
        } label: {
            Text("Accept conditions")
        }
        .buttonStyle(OnboardingButtonStyle())
    }
}

#Preview {
    OperatorView(
        serverOperator: Binding.constant(ServerOperator.sampleData1),
        serverOperatorToEdit: ServerOperator.sampleData1,
        useOperator: ServerOperator.sampleData1.enabled,
        currSMPServers: Binding.constant([UserServer.sampleData.preset]),
        smpServers: Binding.constant([UserServer.sampleData.preset]),
        currXFTPServers: Binding.constant([UserServer.sampleData.xftpPreset]),
        xftpServers: Binding.constant([UserServer.sampleData.xftpPreset])
    )
}
