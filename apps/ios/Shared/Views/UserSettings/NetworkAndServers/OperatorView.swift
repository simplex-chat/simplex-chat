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
    @State var currSMPServers: [UserServer]
    @State var smpServers: [UserServer] = []
    @State var currXFTPServers: [UserServer]
    @State var xftpServers: [UserServer] = []
    @State private var selectedServer: String? = nil
    @State private var justOpened = true

    var body: some View {
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
                    case let .accepted(date):
                        Text("Conditions accepted on: \(conditionsTimestamp(date)).")
                            .foregroundColor(theme.colors.secondary)
                    case let .reviewAvailable(deadline):
                        if serverOperatorToEdit.enabled {
                            Text("Review conditions until: \(conditionsTimestamp(deadline)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    case .reviewRequired:
                        EmptyView()
                    }
                }

                if serverOperatorToEdit.enabled {
                    usageRolesSection()
                    serversSection($smpServers, .smp)
                    serversSection($xftpServers, .xftp)
                }

                Section {
                    if serverOperatorToEdit.enabled {
                        Button("Test servers") {}
                    }
                    Button("Save") {}
                        .disabled(true)
                }
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            serverOperator = serverOperatorToEdit
            ChatModel.shared.updateServerOperator(serverOperatorToEdit)
            dismiss()
        })
        .onAppear {
            // this condition is needed to prevent re-setting the servers when exiting single server view
            if justOpened {
                smpServers = currSMPServers
                xftpServers = currXFTPServers
                justOpened = false
            }
        }
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
                Text(serverOperator.name)
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
                    case .reviewAvailable:
                        if !ChatModel.shared.operatorsWithConditionsAccepted.isEmpty {
                            showConditionsSheet = true
                        } else {
                            serverOperatorToEdit.enabled = true
                            ChatModel.shared.updateServerOperator(serverOperatorToEdit)
                        }
                    case .reviewRequired:
                        showConditionsSheet = true
                    }
                } else {
                    serverOperatorToEdit.enabled = false
                    ChatModel.shared.updateServerOperator(serverOperatorToEdit)
                }
            }
    }

    private func onUseToggleSheetDismissed() {
        if useOperator && !serverOperatorToEdit.enabled {
            if case .reviewRequired = serverOperatorToEdit.conditionsAcceptance {
                useOperatorToggleReset = true
                useOperator = false
            } else if serverOperatorToEdit.conditionsAcceptance.conditionsAccepted {
                serverOperatorToEdit.enabled = true
                ChatModel.shared.updateServerOperator(serverOperatorToEdit)
            }
        }
    }

    private func usageRolesSection() -> some View {
        Section(header: Text("Use operator").foregroundColor(theme.colors.secondary)) {
            Toggle("For storage", isOn: $serverOperatorToEdit.roles.storage)
            Toggle("As proxy", isOn: $serverOperatorToEdit.roles.proxy)
        }
    }

    @ViewBuilder private func serversSection(_ servers: Binding<[UserServer]>, _ serverProtocol: ServerProtocol) -> some View {
        let proto = serverProtocol.rawValue.uppercased()
        Section {
            ForEach(servers) { srv in
                protocolServerView(srv, serverProtocol)
            }
        } header: {
            Text("\(proto) servers")
                .foregroundColor(theme.colors.secondary)
        } footer: {
            Text("The servers for new connections of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                .foregroundColor(theme.colors.secondary)
                .lineLimit(10)
        }
    }

    // TODO Refactor (similar function in ProtocolServersView) / Keep modified for operator servers? (some things are not applicable)
    // TODO Check all servers across all operators (uniqueAddress in ProtocolServersView) / Validate via api (per server?)
    private func protocolServerView(_ server: Binding<UserServer>, _ serverProtocol: ServerProtocol) -> some View {
        let proto = serverProtocol.rawValue.uppercased()
        let srv = server.wrappedValue
        return NavigationLink(tag: srv.id, selection: $selectedServer) {
            ProtocolServerView(
                serverProtocol: serverProtocol,
                server: server,
                serverToEdit: srv,
                preset: true,
                backLabel: "\(serverOperator.name) servers"
            )
            .navigationBarTitle("\(proto) server")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            let address = parseServerAddress(srv.server)
            HStack {
                Group {
                    if !srv.enabled {
                        Image(systemName: "slash.circle").foregroundColor(theme.colors.secondary)
                    } else {
                        showTestStatus(server: srv)
                    }
                }
            }
            .frame(width: 16, alignment: .center)
            .padding(.trailing, 4)

            let v = Text(address?.hostnames.first ?? srv.server).lineLimit(1)
            if srv.enabled {
                v
            } else {
                v.foregroundColor(theme.colors.secondary)
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

func conditionsTextView() -> some View {
    ScrollView {
        Text(conditionsText)
            .padding()
    }
    .background(
        RoundedRectangle(cornerRadius: 12, style: .continuous)
            .fill(Color(uiColor: .secondarySystemGroupedBackground))
    )
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
                Text("Use operator \(serverOperator.name)")
                    .font(.largeTitle)
                    .bold()
                    .padding(.top)
                    .padding(.top)
                
                let operatorsWithConditionsAccepted = ChatModel.shared.operatorsWithConditionsAccepted
                
                if case let .accepted(date) = serverOperator.conditionsAcceptance {
                    
                    conditionsTextView()
                    
                    Text("Conditions accepted on: \(conditionsTimestamp(date)).")
                        .foregroundColor(theme.colors.secondary)
                        .padding(.bottom)
                        .padding(.bottom)

                } else if !operatorsWithConditionsAccepted.isEmpty {
                    
                    Text("You already accepted conditions of use for following operator(s): **\(operatorsWithConditionsAccepted.map { $0.name }.joined(separator: ", "))**.")

                    Text("Same conditions will apply to operator **\(serverOperator.name)**.")

                    conditionsAppliedToOtherOperatorsText()
                    
                    if !conditionsExpanded {
                        Button {
                            conditionsExpanded = true
                        } label: {
                            Text("View conditions")
                        }

                        Spacer()
                    } else {
                        conditionsTextView()
                    }
                    
                    acceptConditionsButton()
                        .padding(.bottom)
                        .padding(.bottom)

                } else {
                    
                    Text("In order to use operator **\(serverOperator.name)**, accept conditions of use.")

                    conditionsAppliedToOtherOperatorsText()
                    
                    conditionsTextView()
                    
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
            Text("Conditions will also apply for following operator(s) you use: **\(otherEnabledOperators.map { $0.name }.joined(separator: ", "))**.")
        }
    }

    private func acceptConditionsButton() -> some View {
        Button {
            // Should call api to save state here, not when saving all servers
            // (It's counterintuitive to lose to closed sheet or Reset)
            let date = Date.now
            ChatModel.shared.acceptConditionsForEnabledOperators(date)
            serverOperatorToEdit.conditionsAcceptance = .accepted(date: date)
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
            case let .reviewUpdatedConditions(acceptForOperators, _):
                
                Text("Conditions will be accepted for following operator(s): **\(acceptForOperators.map { $0.name }.joined(separator: ", "))**.")

                conditionsTextView()
                
                acceptConditionsButton(acceptForOperators)
                    .padding(.bottom)
                    .padding(.bottom)
                
            case let .viewAcceptedConditions(acceptedForOperators):
                
                Text("Conditions are accepted for following operator(s): **\(acceptedForOperators.map { $0.name }.joined(separator: ", "))**.")

                conditionsTextView()
                    .padding(.bottom)
                    .padding(.bottom)
                
            }
        }
        .padding(.horizontal)
        .frame(maxHeight: .infinity)
    }

    private func acceptConditionsButton(_ acceptForOperators: [ServerOperator]) -> some View {
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
        currSMPServers: [UserServer.sampleData.preset],
        currXFTPServers: [UserServer.sampleData.xftpPreset]
    )
}
