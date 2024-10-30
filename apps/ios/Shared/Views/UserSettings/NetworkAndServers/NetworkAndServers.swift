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

struct NetworkAndServers: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State private var serverOperators: [ServerOperator] = []

    var body: some View {
        VStack {
            List {
                presetServersSection()

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
    }

    @ViewBuilder private func presetServersSection() -> some View {
        let smpServers = [ServerCfg.sampleData.preset, ServerCfg.sampleData.preset]
        let xftpServers = [ServerCfg.sampleData.xftpPreset, ServerCfg.sampleData.xftpPreset]
        Section {
            ForEach($serverOperators) { srvOperator in
                serverOperatorView(srvOperator, smpServers, xftpServers)
            }
        } header: {
            Text("Preset servers")
                .foregroundColor(theme.colors.secondary)
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
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers()
    }
}
