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
    @EnvironmentObject var theme: AppTheme
    let serverProtocol: ServerProtocol
    @Binding var serverOperator: ServerOperator
    @State var serverOperatorToEdit: ServerOperator
    @State var useOperator: Bool
    @State private var useOperatorToggleReset: Bool = false
    @State private var showConditionsSheet: Bool = false
    @State var currServers: [ServerCfg]
    @State var servers: [ServerCfg] = []
    @State private var selectedServer: String? = nil
    @State private var justOpened = true

    var proto: String { serverProtocol.rawValue.uppercased() }

    var body: some View {
        VStack {
            List {
                Section {
                    infoViewLink()
                    useOperatorToggle()
                    if serverOperatorToEdit.enabled || serverOperatorToEdit.latestConditionsAcceptance.conditionsAccepted {
                        viewConditionsButton()
                    }
                } header: {
                    Text("Operator")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    switch (serverOperatorToEdit.latestConditionsAcceptance) {
                    case let .accepted(date):
                        Text("Conditions accepted on: \(conditionsTimestamp(date)).")
                    case let .reviewAvailable(deadline):
                        if serverOperatorToEdit.enabled {
                            Text("Review conditions until: \(conditionsTimestamp(deadline)).")
                        }
                    case .reviewRequired:
                        EmptyView()
                    }
                }

                if serverOperatorToEdit.enabled {
                    usageRolesSection()
                    serversSection()
                }

                Section {
                    Button("Reset") { servers = currServers }
                        .disabled(Set(servers) == Set(currServers))
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
            dismiss()
        })
        .onAppear {
            // this condition is needed to prevent re-setting the servers when exiting single server view
            if justOpened {
                servers = currServers
                justOpened = false
            }
        }
        .sheet(isPresented: $showConditionsSheet, onDismiss: onUseToggleSheetDismissed) {
            UsageConditionsView(serverOperator: $serverOperatorToEdit, serverOperatorToEdit: serverOperatorToEdit)
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
                Image(serverOperator.info.logo)
                    .resizable()
                    .scaledToFit()
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
                    if serverOperatorToEdit.latestConditionsAcceptance.usageAllowed {
                        serverOperatorToEdit.enabled = true
                    } else {
                        showConditionsSheet = true
                    }
                } else {
                    serverOperatorToEdit.enabled = false
                }
            }
    }

    private func onUseToggleSheetDismissed() {
        if useOperator {
            if case .reviewRequired = serverOperatorToEdit.latestConditionsAcceptance {
                useOperatorToggleReset = true
                useOperator = false
            } else if serverOperatorToEdit.latestConditionsAcceptance.conditionsAccepted {
                serverOperatorToEdit.enabled = true
            }
        }
    }

    private func viewConditionsButton() -> some View {
        Button {
            showConditionsSheet = true
        } label: {
            if case .accepted = serverOperatorToEdit.latestConditionsAcceptance {
                Text("Conditions accepted")
            } else {
                Text("Review conditions")
            }
        }
    }

    private func usageRolesSection() -> some View {
        Section(header: Text("Use operator").foregroundColor(theme.colors.secondary)) {
            Toggle("For storage", isOn: $serverOperatorToEdit.roles.storage)
            Toggle("As proxy", isOn: $serverOperatorToEdit.roles.proxy)
        }
    }

    private func serversSection() -> some View {
        Section {
            ForEach($servers) { srv in
                protocolServerView(srv)
            }
        } header: {
            Text("\(serverOperator.name) \(proto) servers")
                .foregroundColor(theme.colors.secondary)
        } footer: {
            Text("The servers for new connections of your current chat profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                .foregroundColor(theme.colors.secondary)
                .lineLimit(10)
        }
    }

    private func protocolServerView(_ server: Binding<ServerCfg>) -> some View {
        let srv = server.wrappedValue
        return NavigationLink(tag: srv.id, selection: $selectedServer) {
            ProtocolServerView(
                serverProtocol: serverProtocol,
                server: server,
                serverToEdit: srv
            )
            .navigationBarTitle("\(serverOperator.name) server")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            let address = parseServerAddress(srv.server)
            HStack {
                Group {
                    if let address = address {
                        if !address.valid || address.serverProtocol != serverProtocol {
                            invalidServer()
                        } else if !uniqueAddress(srv, address) {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        } else if !srv.enabled {
                            Image(systemName: "slash.circle").foregroundColor(theme.colors.secondary)
                        } else {
                            showTestStatus(server: srv)
                        }
                    } else {
                        invalidServer()
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

    private func invalidServer() -> some View {
        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
    }

    // TODO all servers across all operators
    private func uniqueAddress(_ s: ServerCfg, _ address: ServerAddress) -> Bool {
        servers.allSatisfy { srv in
            address.hostnames.allSatisfy { host in
                srv.id == s.id || !srv.server.contains(host)
            }
        }
    }
}

private func conditionsTimestamp(_ date: Date) -> String {
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

struct UsageConditionsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var serverOperator: ServerOperator
    @State var serverOperatorToEdit: ServerOperator

    let conditionsText = """
    Lorem ipsum odor amet, consectetuer adipiscing elit. Blandit mauris massa tempor ac; maximus accumsan magnis. Sollicitudin maximus tempor luctus sociosqu turpis dictum per imperdiet porttitor. Efficitur mattis fusce curae id efficitur. Non bibendum elementum faucibus vehicula morbi pulvinar. Accumsan habitant tincidunt sollicitudin taciti ad urna potenti velit. Primis laoreet pharetra magnis est dolor proin viverra.

    Laoreet auctor morbi a varius rutrum diam porta? In ad erat condimentum erat leo ornare. Eu venenatis inceptos rhoncus urna fringilla dis proin ante. Cras dignissim rutrum et faucibus feugiat neque curae tempus. Tellus ligula id dapibus, diam sollicitudin velit odio aliquam lectus. Maecenas ullamcorper arcu interdum cubilia donec iaculis. Maximus penatibus turpis a; vel fermentum ridiculus magna phasellus pellentesque. Eros tellus libero varius potenti; lobortis iaculis.

    Mollis condimentum potenti velit at rutrum tellus maximus suscipit nec. Vehicula aenean dui netus enim aliquam. Aliquam libero rhoncus per pharetra accumsan eros. Urna non eu sem varius vivamus mus tellus aptent quam. Tristique mi natoque lectus volutpat facilisi commodo ac consequat. Proin parturient facilisi senectus egestas ultrices. Fringilla nisi urna convallis molestie lorem varius phasellus a ornare. Ullamcorper varius praesent facilisi habitasse massa.

    Potenti dolor ridiculus est faucibus leo. Euismod consequat ultricies fringilla sociosqu duis sollicitudin. Eget convallis lacinia lacus justo per habitasse parturient. Donec nunc himenaeos pretium donec cursus pharetra ac phasellus? Fringilla sodales egestas orci ligula per ligula semper pellentesque. Potenti non dignissim tempor; orci rutrum elit.

    Habitasse eu sapien eleifend gravida tortor potenti senectus euismod. Lectus enim fames turpis lectus facilisi efficitur elit porttitor facilisi. Nisl quam senectus quam augue integer leo. In aliquam tempor nibh proin felis tortor elementum sodales lacinia. Ut per placerat bibendum magna dapibus fermentum bibendum amet congue. Curae bibendum enim platea per faucibus imperdiet morbi hac varius. Conubia feugiat justo hac faucibus dis.
    """

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            Text("Use operator \(serverOperator.name)")
                .font(.largeTitle)
                .bold()
                .padding(.horizontal)
                .padding(.top)
                .padding(.top)

            if !serverOperator.latestConditionsAcceptance.conditionsAccepted {
                Text("In order to use operator \(serverOperator.name), accept conditions of use.")
                    .foregroundColor(theme.colors.secondary)
                    .padding(.horizontal)
            }

            ScrollView {
                Text(conditionsText)
                    .padding()
            }
            .background(
                RoundedRectangle(cornerRadius: 12, style: .continuous)
                    .fill(Color(uiColor: .secondarySystemGroupedBackground))
            )
            .padding(.horizontal)

            Group {
                if case let .accepted(date) = serverOperator.latestConditionsAcceptance {
                    Text("Conditions accepted on: \(conditionsTimestamp(date)).")
                        .foregroundColor(theme.colors.secondary)
                } else {
                    HStack {
                        Spacer()

                        Button {
                            // Should call api to save state here, not when saving all servers
                            // (It's counterintuitive to lose to closed sheet or Reset)
                            serverOperatorToEdit.latestConditionsAcceptance = .accepted(date: Date.now)
                            serverOperator = serverOperatorToEdit
                            dismiss()
                        } label: {
                            Text("Accept conditions")
                        }
                        .buttonStyle(.borderedProminent)

                        Spacer()
                    }
                }
            }
            .padding(.horizontal)
            .padding(.bottom)
        }
    }
}

#Preview {
    OperatorView(
        serverProtocol: .smp,
        serverOperator: Binding.constant(ServerOperator.sampleData1),
        serverOperatorToEdit: ServerOperator.sampleData1,
        useOperator: ServerOperator.sampleData1.enabled,
        currServers: [ServerCfg.sampleData.preset]
    )
}
