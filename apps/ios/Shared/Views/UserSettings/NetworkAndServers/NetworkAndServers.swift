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
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State private var serverOperators: [ServerOperator] = []
    @State private var sheetItem: NetworkAndServersSheet? = nil

    var body: some View {
        VStack {
            List {
                let conditionsAction = m.usageConditionsAction
                let smpServers = [ServerCfg.sampleData.preset, ServerCfg.sampleData.preset]
                let xftpServers = [ServerCfg.sampleData.xftpPreset, ServerCfg.sampleData.xftpPreset]
                Section {
                    ForEach($serverOperators) { srvOperator in
                        serverOperatorView(srvOperator, smpServers, xftpServers)
                    }

                    if let conditionsAction = conditionsAction {
                        conditionsButton(conditionsAction)
                    }
                } header: {
                    Text("Preset servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    switch conditionsAction {
                    case let .reviewUpdatedConditions(_, deadline):
                        if let deadline = deadline {
                            Text("Review conditions until: \(conditionsTimestamp(deadline)).")
                                .foregroundColor(theme.colors.secondary)
                        }
                    default:
                        EmptyView()
                    }
                }

                Section {
                    NavigationLink {
                        YourServersView()
                            .navigationTitle("Your servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Your servers")
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
                    Button("Save servers") {}
                        .disabled(true)
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
        .onAppear {
            serverOperators = ChatModel.shared.serverOperators
        }
        .sheet(item: $sheetItem, onDismiss: { serverOperators = ChatModel.shared.serverOperators }) { item in
            switch item {
            case let .showConditions(conditionsAction):
                UsageConditionsView(
                    showTitle: true,
                    dismissOnAccept: true,
                    conditionsAction: conditionsAction,
                    onAcceptAction: { date in
                        switch conditionsAction {
                        case let .reviewUpdatedConditions(acceptForOperators, _): ChatModel.shared.acceptConditionsForOperators(acceptForOperators, date)
                        default: break
                        }
                    }
                )
                .modifier(ThemedBackground(grouped: true))
            }
        }
    }

    @ViewBuilder private func serverOperatorView(
        _ serverOperator: Binding<ServerOperator>,
        _ smpServers: [ServerCfg],
        _ xftpServers: [ServerCfg]
    ) -> some View {
        let srvOperator = serverOperator.wrappedValue
        NavigationLink() {
            OperatorView(
                serverOperator: serverOperator,
                serverOperatorToEdit: srvOperator,
                useOperator: srvOperator.enabled,
                currSMPServers: smpServers,
                currXFTPServers: xftpServers
            )
            .navigationBarTitle("\(srvOperator.name) servers")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Image(srvOperator.info.logo)
                    .resizable()
                    .scaledToFit()
                    .grayscale(srvOperator.enabled ? 0.0 : 1.0)
                    .frame(width: 24, height: 24)
                Text(srvOperator.name)
                    .foregroundColor(srvOperator.enabled ? theme.colors.onBackground : theme.colors.secondary)
            }
        }
    }

    private func conditionsButton(_ conditionsAction: UsageConditionsAction) -> some View {
        Button {
            sheetItem = .showConditions(conditionsAction: conditionsAction)
        } label: {
            switch conditionsAction {
            case .reviewUpdatedConditions:
                Text("Review conditions")
            case .viewAcceptedConditions:
                Text("Accepted conditions")
            }
        }
    }
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers()
    }
}
