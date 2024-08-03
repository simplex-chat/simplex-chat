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

    var body: some View {
        VStack {
            List {
                Section {
                    NavigationLink {
                        ProtocolServersView(serverProtocol: .smp)
                            .navigationTitle("Your SMP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("SMP servers")
                    }

                    NavigationLink {
                        ProtocolServersView(serverProtocol: .xftp)
                            .navigationTitle("Your XFTP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("XFTP servers")
                    }

                    NavigationLink {
                        AdvancedNetworkSettings()
                            .navigationTitle("Advanced settings")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Advanced settings")
                    }
                } header: {
                    Text("Messages & files")
                        .foregroundColor(theme.colors.secondary)
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
    }
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers()
    }
}
