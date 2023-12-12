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
    @State private var netCfg = NetCfg.defaults
    @State private var currentNetCfg = NetCfg.defaults
    @State private var cfgLoaded = false
    @State private var enableKeepAlive = true
    @State private var keepAliveOpts = KeepAliveOpts.defaults
    @State private var showSettingsAlert: NetworkSettingsAlert?

    var body: some View {
        VStack {
            List {
                Section {
                    Button {
                        updateNetCfgView(NetCfg.defaults)
                        showSettingsAlert = .update
                    } label: {
                        Text("Reset to defaults")
                    }
                    .disabled(currentNetCfg == NetCfg.defaults)

                    Button {
                        updateNetCfgView(NetCfg.proxyDefaults)
                        showSettingsAlert = .update
                    } label: {
                        Text("Set timeouts for proxy/VPN")
                    }
                    .disabled(currentNetCfg == NetCfg.proxyDefaults)

                    timeoutSettingPicker("TCP connection timeout", selection: $netCfg.tcpConnectTimeout, values: [7_500000, 10_000000, 15_000000, 20_000000, 30_000000, 45_000000], label: secondsLabel)
                    timeoutSettingPicker("Protocol timeout", selection: $netCfg.tcpTimeout, values: [5_000000, 7_000000, 10_000000, 15_000000, 20_000000, 30_000000], label: secondsLabel)
                    timeoutSettingPicker("Protocol timeout per KB", selection: $netCfg.tcpTimeoutPerKb, values: [15_000, 30_000, 45_000, 60_000, 90_000, 120_000], label: secondsLabel)
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
                        .foregroundColor(.secondary)
                    }
                } header: {
                        Text("")
                } footer: {
                    HStack {
                        Button {
                            updateNetCfgView(currentNetCfg)
                        } label: {
                            Label("Revert", systemImage: "arrow.counterclockwise").font(.callout)
                        }

                        Spacer()

                        Button {
                            showSettingsAlert = .update
                        } label: {
                            Label("Save", systemImage: "checkmark").font(.callout)
                        }
                    }
                    .disabled(netCfg == currentNetCfg)
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
            updateNetCfgView(currentNetCfg)
        }
        .alert(item: $showSettingsAlert) { a in
            switch a {
            case .update:
                return Alert(
                    title: Text("Update network settings?"),
                    message: Text("Updating settings will re-connect the client to all servers."),
                    primaryButton: .default(Text("Ok")) {
                        saveNetCfg()
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
    }

    private func updateNetCfgView(_ cfg: NetCfg) {
        netCfg = cfg
        enableKeepAlive = netCfg.enableKeepAlive
        keepAliveOpts = netCfg.tcpKeepAlive ?? KeepAliveOpts.defaults
    }

    private func saveNetCfg() {
        do {
            try setNetworkConfig(netCfg)
            currentNetCfg = netCfg
            setNetCfg(netCfg)
        } catch let error {
            let err = responseError(error)
            showSettingsAlert = .error(err: err)
            logger.error("\(err)")
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
}

struct AdvancedNetworkSettings_Previews: PreviewProvider {
    static var previews: some View {
        AdvancedNetworkSettings()
    }
}
