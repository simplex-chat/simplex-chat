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
    case updateOnionHosts(hosts: OnionHosts)
    case updateSessionMode(mode: TransportSessionMode)
    case error(err: String)

    var id: String {
        switch self {
        case let .updateOnionHosts(hosts): return "updateOnionHosts \(hosts)"
        case let .updateSessionMode(mode): return "updateSessionMode \(mode)"
        case let .error(err): return "error \(err)"
        }
    }
}

struct NetworkAndServers: View {
    @EnvironmentObject var m: ChatModel
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var cfgLoaded = false
    @State private var currentNetCfg = NetCfg.defaults
    @State private var netCfg = NetCfg.defaults
    @State private var onionHosts: OnionHosts = .no
    @State private var sessionMode: TransportSessionMode = .user
    @State private var alert: NetworkAlert?

    var body: some View {
        VStack {
            List {
                Section {
                    NavigationLink {
                        ProtocolServersView(serverProtocol: .smp)
                            .navigationTitle("Your SMP servers")
                    } label: {
                        Text("SMP servers")
                    }

                    NavigationLink {
                        ProtocolServersView(serverProtocol: .xftp)
                            .navigationTitle("Your XFTP servers")
                    } label: {
                        Text("XFTP servers")
                    }

                    Picker("Use .onion hosts", selection: $onionHosts) {
                        ForEach(OnionHosts.values, id: \.self) { Text($0.text) }
                    }
                    .frame(height: 36)

                    if developerTools {
                        Picker("Transport isolation", selection: $sessionMode) {
                            ForEach(TransportSessionMode.values, id: \.self) { Text($0.text) }
                        }
                        .frame(height: 36)
                    }

                    NavigationLink {
                        AdvancedNetworkSettings()
                            .navigationTitle("Network settings")
                    } label: {
                        Text("Advanced network settings")
                    }
                } header: {
                    Text("Messages & files")
                } footer: {
                    Text("Using .onion hosts requires compatible VPN provider.")
                }

                Section("Calls") {
                    NavigationLink {
                        RTCServers()
                            .navigationTitle("Your ICE servers")
                    } label: {
                        Text("WebRTC ICE servers")
                    }
                }

                Section("Network connection") {
                    HStack {
                        Text(m.networkInfo.networkType.text)
                        Spacer()
                        Image(systemName: "circle.fill").foregroundColor(m.networkInfo.online ? .green : .red)
                    }
                }
            }
        }
        .onAppear {
            if cfgLoaded { return }
            cfgLoaded = true
            currentNetCfg = getNetCfg()
            resetNetCfgView()
        }
        .onChange(of: onionHosts) { _ in
            if onionHosts != OnionHosts(netCfg: currentNetCfg) {
                alert = .updateOnionHosts(hosts: onionHosts)
            }
        }
        .onChange(of: sessionMode) { _ in
            if sessionMode != netCfg.sessionMode {
                alert = .updateSessionMode(mode: sessionMode)
            }
        }
        .alert(item: $alert) { a in
            switch a {
            case let .updateOnionHosts(hosts):
                return Alert(
                    title: Text("Update .onion hosts setting?"),
                    message: Text(onionHostsInfo(hosts)) + Text("\n") + Text("Updating this setting will re-connect the client to all servers."),
                    primaryButton: .default(Text("Ok")) {
                        let (hostMode, requiredHostMode) = hosts.hostMode
                        netCfg.hostMode = hostMode
                        netCfg.requiredHostMode = requiredHostMode
                        saveNetCfg()
                    },
                    secondaryButton: .cancel() {
                        resetNetCfgView()
                    }
                )
            case let .updateSessionMode(mode):
                return Alert(
                    title: Text("Update transport isolation mode?"),
                    message: Text(sessionModeInfo(mode)) + Text("\n") + Text("Updating this setting will re-connect the client to all servers."),
                    primaryButton: .default(Text("Ok")) {
                        netCfg.sessionMode = mode
                        saveNetCfg()
                    },
                    secondaryButton: .cancel() {
                        resetNetCfgView()
                    }
                )
            case let .error(err):
                return Alert(
                    title: Text("Error updating settings"),
                    message: Text(err)
                )
            }
        }
    }

    private func saveNetCfg() {
        do {
            let def = netCfg.hostMode == .onionHost ? NetCfg.proxyDefaults : NetCfg.defaults
            netCfg.tcpConnectTimeout = def.tcpConnectTimeout
            netCfg.tcpTimeout = def.tcpTimeout
            try setNetworkConfig(netCfg)
            currentNetCfg = netCfg
            setNetCfg(netCfg)
        } catch let error {
            let err = responseError(error)
            resetNetCfgView()
            alert = .error(err: err)
            logger.error("\(err)")
        }
    }

    private func resetNetCfgView() {
        netCfg = currentNetCfg
        onionHosts = OnionHosts(netCfg: netCfg)
        sessionMode = netCfg.sessionMode
    }

    private func onionHostsInfo(_ hosts: OnionHosts) -> LocalizedStringKey {
        switch hosts {
        case .no: return "Onion hosts will not be used."
        case .prefer: return "Onion hosts will be used when available. Requires enabling VPN."
        case .require: return "Onion hosts will be required for connection. Requires enabling VPN."
        }
    }

    private func sessionModeInfo(_ mode: TransportSessionMode) -> LocalizedStringKey {
        switch mode {
        case .user: return "A separate TCP connection will be used **for each chat profile you have in the app**."
        case .entity: return "A separate TCP connection will be used **for each contact and group member**.\n**Please note**: if you have many connections, your battery and traffic consumption can be substantially higher and some connections may fail."
        }
    }
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers()
    }
}
