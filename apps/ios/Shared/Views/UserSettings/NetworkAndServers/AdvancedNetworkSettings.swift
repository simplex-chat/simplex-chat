//
//  AdvancedNetworkSettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 02/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let secondsLabel =  NSLocalizedString("sec", comment: "network option")

enum NetworkSettingsAlert: Identifiable {
    case update
    case error(err: String)

    var id: String {
        switch self {
        case .update: return "update"
        case let .error(err): return "error \(err)"
        }
    }
}

struct AdvancedNetworkSettings: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false
    @State private var netCfg = NetCfg.defaults
    @State private var currentNetCfg = NetCfg.defaults
    @State private var cfgLoaded = false
    @State private var enableKeepAlive = true
    @State private var keepAliveOpts = KeepAliveOpts.defaults
    @State private var showSettingsAlert: NetworkSettingsAlert?
    @State private var onionHosts: OnionHosts = .no
    @State private var showSaveDialog = false
    @State private var netProxy = networkProxyDefault.get()
    @State private var currentNetProxy = networkProxyDefault.get()
    @State private var useNetProxy = false
    @State private var netProxyAuth = false

    var body: some View {
        VStack {
            List {
                Section {
                    NavigationLink {
                        List {
                            Section {
                                SelectionListView(list: SMPProxyMode.values, selection: $netCfg.smpProxyMode) { mode in
                                    netCfg.smpProxyMode = mode
                                }
                            } footer: {
                                Text(proxyModeInfo(netCfg.smpProxyMode))
                                    .font(.callout)
                                    .foregroundColor(theme.colors.secondary)
                            }
                        }
                        .navigationTitle("Private routing")
                        .modifier(ThemedBackground(grouped: true))
                        .navigationBarTitleDisplayMode(.inline)
                    } label: {
                        HStack {
                            Text("Private routing")
                            Spacer()
                            Text(netCfg.smpProxyMode.label)
                        }
                    }
                    
                    NavigationLink {
                        List {
                            Section {
                                SelectionListView(list: SMPProxyFallback.values, selection: $netCfg.smpProxyFallback) { mode in
                                    netCfg.smpProxyFallback = mode
                                }
                                .disabled(netCfg.smpProxyMode == .never)
                            } footer: {
                                Text(proxyFallbackInfo(netCfg.smpProxyFallback))
                                    .font(.callout)
                                    .foregroundColor(theme.colors.secondary)
                            }
                        }
                        .navigationTitle("Allow downgrade")
                        .modifier(ThemedBackground(grouped: true))
                        .navigationBarTitleDisplayMode(.inline)
                    } label: {
                        HStack {
                            Text("Allow downgrade")
                            Spacer()
                            Text(netCfg.smpProxyFallback.label)
                        }
                    }

                    Toggle("Show message status", isOn: $showSentViaProxy)
                } header: {
                    Text("Private message routing")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    VStack(alignment: .leading) {
                        Text("To protect your IP address, private routing uses your SMP servers to deliver messages.")
                        if showSentViaProxy {
                            Text("Show → on messages sent via private routing.")
                        }
                    }
                    .foregroundColor(theme.colors.secondary)
                }

                Section {
                    Toggle("Use SOCKS proxy", isOn: $useNetProxy)
                    Group {
                        TextField("IP address", text: $netProxy.host)
                        TextField(
                            "Port",
                            text: Binding(
                                get: { netProxy.port > 0 ? "\(netProxy.port)" : "" },
                                set: { s in
                                    netProxy.port = if let port = Int(s), port > 0 {
                                        port
                                    } else {
                                        0
                                    }
                                }
                            )
                        )
                        Toggle("Proxy requires password", isOn: $netProxyAuth)
                        if netProxyAuth {
                            TextField("Username", text: $netProxy.username)
                            PassphraseField(
                                key: $netProxy.password,
                                placeholder: "Password",
                                valid: NetworkProxy.validCredential(netProxy.password)
                            )
                        }
                    }
                    .if(!useNetProxy) { $0.foregroundColor(theme.colors.secondary) }
                    .disabled(!useNetProxy)
                } header: {
                    HStack {
                        Text("SOCKS proxy").foregroundColor(theme.colors.secondary)
                        if useNetProxy && !netProxy.valid {
                            Spacer()
                            Image(systemName: "exclamationmark.circle.fill").foregroundColor(.red)
                        }
                    }
                } footer: {
                    if netProxyAuth {
                        Text("Your credentials may be sent unencrypted.")
                            .foregroundColor(theme.colors.secondary)
                    } else {
                        Text("Do not use credentials with proxy.")
                            .foregroundColor(theme.colors.secondary)
                    }
                }
                .onChange(of: useNetProxy) { useNetProxy in
                    netCfg.socksProxy = useNetProxy && currentNetProxy.valid
                        ? currentNetProxy.toProxyString()
                        : nil
                    netProxy = currentNetProxy
                    netProxyAuth = netProxy.username != "" || netProxy.password != ""
                }
                .onChange(of: netProxyAuth) { netProxyAuth in
                    if netProxyAuth {
                        netProxy.auth = currentNetProxy.auth
                        netProxy.username = currentNetProxy.username
                        netProxy.password = currentNetProxy.password
                    } else {
                        netProxy.auth = .username
                        netProxy.username = ""
                        netProxy.password = ""
                    }
                }
                .onChange(of: netProxy) { netProxy in
                    netCfg.socksProxy = useNetProxy && netProxy.valid
                        ? netProxy.toProxyString()
                        : nil
                }

                Section {
                    Picker("Use .onion hosts", selection: $onionHosts) {
                        ForEach(OnionHosts.values, id: \.self) { Text($0.text) }
                    }
                    .frame(height: 36)
                } footer: {
                    Text(onionHostsInfo(onionHosts))
                        .foregroundColor(theme.colors.secondary)
                }
                .onChange(of: onionHosts) { hosts in
                    if hosts != OnionHosts(netCfg: currentNetCfg) {
                        let (hostMode, requiredHostMode) = hosts.hostMode
                        netCfg.hostMode = hostMode
                        netCfg.requiredHostMode = requiredHostMode
                    }
                }
                
                if developerTools {
                    Section {
                        Picker("Transport isolation", selection: $netCfg.sessionMode) {
                            let modes = TransportSessionMode.values.contains(netCfg.sessionMode)
                                ? TransportSessionMode.values
                                : TransportSessionMode.values + [netCfg.sessionMode]
                            ForEach(modes, id: \.self) { Text($0.text) }
                        }
                        .frame(height: 36)
                    } footer: {
                        sessionModeInfo(netCfg.sessionMode)
                            .foregroundColor(theme.colors.secondary)
                    }
                }

                Section("TCP connection") {
                    timeoutSettingPicker("TCP connection timeout", selection: $netCfg.tcpConnectTimeout, values: [10_000000, 15_000000, 20_000000, 30_000000, 45_000000, 60_000000, 90_000000], label: secondsLabel)
                    timeoutSettingPicker("Protocol timeout", selection: $netCfg.tcpTimeout, values: [5_000000, 7_000000, 10_000000, 15_000000, 20_000000, 30_000000], label: secondsLabel)
                    timeoutSettingPicker("Protocol timeout per KB", selection: $netCfg.tcpTimeoutPerKb, values: [2_500, 5_000, 10_000, 15_000, 20_000, 30_000], label: secondsLabel)
                    // intSettingPicker("Receiving concurrency", selection: $netCfg.rcvConcurrency, values: [1, 2, 4, 8, 12, 16, 24], label: "")
                    timeoutSettingPicker("PING interval", selection: $netCfg.smpPingInterval, values: [120_000000, 300_000000, 600_000000, 1200_000000, 2400_000000, 3600_000000], label: secondsLabel)
                    intSettingPicker("PING count", selection: $netCfg.smpPingCount, values: [1, 2, 3, 5, 8], label: "")
                    Toggle("Enable TCP keep-alive", isOn: $enableKeepAlive)

                    if enableKeepAlive {
                        intSettingPicker("TCP_KEEPIDLE", selection: $keepAliveOpts.keepIdle, values: [15, 30, 60, 120, 180], label: secondsLabel)
                        intSettingPicker("TCP_KEEPINTVL", selection: $keepAliveOpts.keepIntvl, values: [5, 10, 15, 30, 60], label: secondsLabel)
                        intSettingPicker("TCP_KEEPCNT", selection: $keepAliveOpts.keepCnt, values: [1, 2, 4, 6, 8], label: "")
                    } else {
                        Group {
                            Text("TCP_KEEPIDLE")
                            Text("TCP_KEEPINTVL")
                            Text("TCP_KEEPCNT")
                        }
                        .foregroundColor(theme.colors.secondary)
                    }
                }
                
                Section {
                    Button("Reset to defaults") {
                        updateNetCfgView(NetCfg.defaults, NetworkProxy.def)
                    }
                    .disabled(netCfg == NetCfg.defaults)

                    Button("Set timeouts for proxy/VPN") {
                        updateNetCfgView(netCfg.withProxyTimeouts, netProxy)
                    }
                    .disabled(netCfg.hasProxyTimeouts)
                    
                    Button("Save and reconnect") {
                        showSettingsAlert = .update
                    }
                    .disabled(netCfg == currentNetCfg || (useNetProxy && !netProxy.valid))
                }
            }
        }
        .onChange(of: keepAliveOpts) { opts in
            netCfg.tcpKeepAlive = keepAliveOpts
        }
        .onChange(of: enableKeepAlive) { on in
            netCfg.tcpKeepAlive = on ? (currentNetCfg.tcpKeepAlive ?? KeepAliveOpts.defaults) : nil
        }
        .onAppear {
            if cfgLoaded { return }
            cfgLoaded = true
            currentNetCfg = getNetCfg()
            currentNetProxy = networkProxyDefault.get()
            updateNetCfgView(currentNetCfg, currentNetProxy)
        }
        .alert(item: $showSettingsAlert) { a in
            switch a {
            case .update:
                return Alert(
                    title: Text("Update settings?"),
                    message: Text("Updating settings will re-connect the client to all servers."),
                    primaryButton: .default(Text("Ok")) {
                        _ = saveNetCfg()
                    },
                    secondaryButton: .cancel()
                )
            case let .error(err):
                return Alert(
                    title: Text("Error updating settings"),
                    message: Text(err)
                )
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if netCfg == currentNetCfg {
                dismiss()
                cfgLoaded = false
            } else if !useNetProxy || netProxy.valid {
                showSaveDialog = true
            }
        })
        .confirmationDialog("Update network settings?", isPresented: $showSaveDialog, titleVisibility: .visible) {
            Button("Save and reconnect") {
                if saveNetCfg() {
                    dismiss()
                    cfgLoaded = false
                }
            }
            Button("Exit without saving") { dismiss() }
        }
    }

    private func updateNetCfgView(_ cfg: NetCfg, _ proxy: NetworkProxy) {
        netCfg = cfg
        netProxy = proxy
        onionHosts = OnionHosts(netCfg: netCfg)
        enableKeepAlive = netCfg.enableKeepAlive
        keepAliveOpts = netCfg.tcpKeepAlive ?? KeepAliveOpts.defaults
        useNetProxy = netCfg.socksProxy != nil
        netProxyAuth = switch netProxy.auth {
        case .username: netProxy.username != "" || netProxy.password != ""
        case .isolate: false
        }
    }

    private func saveNetCfg() -> Bool {
        do {
            try setNetworkConfig(netCfg)
            currentNetCfg = netCfg
            setNetCfg(netCfg, networkProxy: useNetProxy ? netProxy : nil)
            currentNetProxy = netProxy
            networkProxyDefault.set(netProxy)
            return true
        } catch let error {
            let err = responseError(error)
            showSettingsAlert = .error(err: err)
            logger.error("\(err)")
            return false
        }
    }

    private func intSettingPicker(_ title: LocalizedStringKey, selection: Binding<Int>, values: [Int], label: String) -> some View {
        Picker(title, selection: selection) {
            ForEach(values, id: \.self) { value in
                Text("\(value) \(label)")
            }
        }
        .frame(height: 36)
    }

    private func timeoutSettingPicker(_ title: LocalizedStringKey, selection: Binding<Int>, values: [Int], label: String) -> some View {
        Picker(title, selection: selection) {
            let v = selection.wrappedValue
            let vs = values.contains(v) ? values : values + [v]
            ForEach(vs, id: \.self) { value in
                Text("\(String(format: "%g", (Double(value) / 1000000))) \(secondsLabel)")
            }
        }
        .frame(height: 36)
    }
    
    private func onionHostsInfo(_ hosts: OnionHosts) -> LocalizedStringKey {
        switch hosts {
        case .no: return "Onion hosts will not be used."
        case .prefer: return "Onion hosts will be used when available.\nRequires compatible VPN."
        case .require: return "Onion hosts will be **required** for connection.\nRequires compatible VPN."
        }
    }

    private func sessionModeInfo(_ mode: TransportSessionMode) -> Text {
        let userMode = Text("A separate TCP connection will be used **for each chat profile you have in the app**.")
        return switch mode {
        case .user: userMode
        case .session: userMode + Text("\n") + Text("New SOCKS credentials will be used every time you start the app.")
        case .server: userMode + Text("\n") + Text("New SOCKS credentials will be used for each server.")
        case .entity: Text("A separate TCP connection will be used **for each contact and group member**.\n**Please note**: if you have many connections, your battery and traffic consumption can be substantially higher and some connections may fail.")
        }
    }
    
    private func proxyModeInfo(_ mode: SMPProxyMode) -> LocalizedStringKey {
        switch mode {
        case .always: return "Always use private routing."
        case .unknown: return "Use private routing with unknown servers."
        case .unprotected: return "Use private routing with unknown servers when IP address is not protected."
        case .never: return "Do NOT use private routing."
        }
    }
    
    private func proxyFallbackInfo(_ proxyFallback: SMPProxyFallback) -> LocalizedStringKey {
        switch proxyFallback {
        case .allow: return "Send messages directly when your or destination server does not support private routing."
        case .allowProtected: return "Send messages directly when IP address is protected and your or destination server does not support private routing."
        case .prohibit: return "Do NOT send messages directly, even if your or destination server does not support private routing."
        }
    }
}

struct AdvancedNetworkSettings_Previews: PreviewProvider {
    static var previews: some View {
        AdvancedNetworkSettings()
    }
}
