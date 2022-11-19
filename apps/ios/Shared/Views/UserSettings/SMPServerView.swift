//
//  SMPServerView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SMPServerView: View {
    @Binding var server: ServerCfg
    @State var serverToEdit: ServerCfg

    var body: some View {
        if server.preset {
            presetServer()
        } else {
            customServer()
        }
    }

    private func presetServer() -> some View {
        return VStack {
            List {
                Section("Preset server address") {
                    Text(server.server)
                        .textSelection(.enabled)
                }
                useServerSection()
            }
        }
    }

    private func customServer() -> some View {
        VStack {
            List {
                Section("Your server address") {
                    TextEditor(text: $serverToEdit.server)
                        .multilineTextAlignment(.leading)
                        .autocorrectionDisabled(true)
                        .autocapitalization(.none)
                        .lineLimit(10)
                        .frame(height: 108)
                        .padding(-6)
                        .onDisappear {
                            server.server = serverToEdit.server
                        }
                }
                useServerSection()
                Section("Add to another device") {
                    QRCode(uri: server.server)
                        .listRowInsets(EdgeInsets(top: 12, leading: 12, bottom: 12, trailing: 12))
                }
            }
        }
    }

    private func useServerSection() -> some View {
        Section("Use server") {
            HStack {
                Button("Test server") {
                    Task { await testServerConnection(server: $server) }
                }
                Spacer()
                showTestStatus(server: server)
            }
            Toggle("Use for new connections", isOn: $server.enabled)
            Button("Remove server", role: .destructive) {

            }
        }
    }
}

@ViewBuilder func showTestStatus(server: ServerCfg) -> some View {
    switch server.tested {
    case .some(true):
        Image(systemName: "checkmark")
            .foregroundColor(.green)
    case .some(false):
        Image(systemName: "multiply")
            .foregroundColor(.red)
    case .none:
        Color.clear
    }
}

func testServerConnection(server: Binding<ServerCfg>) async {
    do {
        let r = try await testSMPServer(smpServer: server.wrappedValue.server)
        await MainActor.run {
            switch r {
            case .success: server.wrappedValue.tested = true
            case .failure: server.wrappedValue.tested = false
            }
        }
    } catch let error {
        await MainActor.run {
            server.wrappedValue.tested = false
        }
    }
}

struct SMPServerView_Previews: PreviewProvider {
    static var previews: some View {
        SMPServerView(server: Binding.constant(ServerCfg.sampleData.custom), serverToEdit: ServerCfg.sampleData.custom)
    }
}
