//
//  AllServersView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 30.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct AllServersView: View {
    @EnvironmentObject var theme: AppTheme
    @State private var serverOperators: [ServerOperator] = []

    var body: some View {
        VStack {
            List {
                operatorsSection()
                
                Section {
                    NavigationLink {
                        ProtocolServersView(serverProtocol: .smp)
                            .navigationTitle("SMP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Message servers")
                    }

                    NavigationLink {
                        ProtocolServersView(serverProtocol: .xftp)
                            .navigationTitle("XFTP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Media & file servers")
                    }
                } header: {
                    Text("Other servers")
                        .foregroundColor(theme.colors.secondary)
                }

                Section {
                    Button("Reset") {}
                        .disabled(true)
                    Button("Save") {}
                        .disabled(true)
                }
            }
        }
        .onAppear {
            serverOperators = ChatModel.shared.serverOperators
        }
    }

    @ViewBuilder private func operatorsSection() -> some View {
        let smpServers = [ServerCfg.sampleData.preset, ServerCfg.sampleData.preset]
        let xftpServers = [ServerCfg.sampleData.xftpPreset, ServerCfg.sampleData.xftpPreset]
        Section {
            ForEach($serverOperators) { srvOperator in
                serverOperatorView(srvOperator, smpServers, xftpServers)
            }
        } header: {
            Text("Operators")
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
                    .frame(width: 24, height: 24)
                Text(srvOperator.name)
            }
        }
    }
}

#Preview {
    AllServersView()
}
