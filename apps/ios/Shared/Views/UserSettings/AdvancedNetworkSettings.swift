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

struct AdvancedNetworkSettings: View {
    @State private var netCfg = NetCfg.defaults
    @State private var currentNetCfg = NetCfg.defaults
    @State private var cfgLoaded = false
    @State private var enableKeepAlive = true
    @State private var keepAliveOpts = KeepAliveOpts.defaults
    @State private var showSaveErrorAlert = false
    @State private var saveConfigError: String?

    var body: some View {
        VStack {
            List {
                Section {
                    Button {
                        currentNetCfg = NetCfg.defaults
                        resetNetCfg()
                        saveNetCfg()
                    } label: {
                        Text("Reset to defaults")
                    }
                    .disabled(currentNetCfg == NetCfg.defaults)

                    Button {
                        currentNetCfg = NetCfg.proxyDefaults
                        resetNetCfg()
                        saveNetCfg()
                    } label: {
                        Text("Set timeouts for proxy/VPN")
                    }
                    .disabled(currentNetCfg == NetCfg.proxyDefaults)

                    timeoutSettingPicker("TCP connection timeout", selection: $netCfg.tcpConnectTimeout, values: [2_500000, 5_000000, 7_500000, 10_000000, 15_000000, 20_000000], label: secondsLabel)
                    timeoutSettingPicker("Protocol timeout", selection: $netCfg.tcpTimeout, values: [1_500000, 3_000000, 5_000000, 7_000000, 10_000000, 15_000000], label: secondsLabel)
                    timeoutSettingPicker("PING interval", selection: $netCfg.smpPingInterval, values: [120_000000, 300_000000, 600_000000, 1200_000000, 2400_000000], label: secondsLabel)
                    Toggle("Enable TCP keep-alive", isOn: $enableKeepAlive)
                    if enableKeepAlive {
                        intSettingPicker("TCP_KEEPIDLE", selection: $keepAliveOpts.keepIdle, values: [15, 30, 60, 120, 180], label: secondsLabel)
                        intSettingPicker("TCP_KEEPINTVL", selection: $keepAliveOpts.keepIntvl, values: [5, 10, 15, 30, 60], label: secondsLabel)
                        intSettingPicker("TCP_KEEPCNT", selection: $keepAliveOpts.keepCnt, values: [1, 2, 4, 6, 8], label: "")
                    }
                } header: {
                        Text("")
                } footer: {
                    HStack {
                        Button {
                            resetNetCfg()
                        } label: {
                            Label("Revert", systemImage: "arrow.counterclockwise").font(.callout)
                        }

                        Spacer()

                        Button {
                            saveNetCfg()
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
            resetNetCfg()
        }
        .alert(isPresented: $showSaveErrorAlert) {
            Alert(
                title: Text("Error updating settings"),
                message: Text(saveConfigError ?? "Unexpected error")
            )
        }
    }

    private func resetNetCfg() {
        netCfg = currentNetCfg
        enableKeepAlive = currentNetCfg.enableKeepAlive
        keepAliveOpts = currentNetCfg.tcpKeepAlive ?? KeepAliveOpts.defaults
    }

    private func saveNetCfg() {
        do {
            try setNetworkConfig(netCfg)
            currentNetCfg = netCfg
            setNetCfg(netCfg)
        } catch let error {
            saveConfigError = responseError(error)
            showSaveErrorAlert = true
        }
    }

    private func intSettingPicker(_ title: LocalizedStringKey, selection: Binding<Int>, values: [Int], label: String) -> some View {
        Picker(title, selection: selection) {
            ForEach(values, id: \.self) { value in
                Text("\(value) \(label)")
            }
        }
    }

    private func timeoutSettingPicker(_ title: LocalizedStringKey, selection: Binding<Int>, values: [Int], label: String) -> some View {
        Picker(title, selection: selection) {
            ForEach(values, id: \.self) { value in
                Text("\(String(format: "%g", (Double(value) / 1000000))) \(secondsLabel)")
            }
        }
    }
}

struct AdvancedNetworkSettings_Previews: PreviewProvider {
    static var previews: some View {
        AdvancedNetworkSettings()
    }
}
