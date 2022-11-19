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
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var server: ServerCfg
    @State var serverToEdit: ServerCfg
    @State var showTestFailure = false
    @State var testFailure: SMPTestFailure?

    var body: some View {
        smpServerView()
            .navigationBarBackButtonHidden(true)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    Button {
                        if serverToEdit.server != server.server {
                            print("serverToEdit.tested = nil")
                            serverToEdit.tested = nil
                        }
                        server = serverToEdit
                        dismiss()
                    } label: {
                        HStack {
                            Image(systemName: "chevron.left")
                            Text("Your SMP Servers")
                        }
                    }
                }
            }
            .alert(isPresented: $showTestFailure) {
                Alert(
                    title: Text("Server test failed!"),
                    message: Text(testFailure?.localizedDescription ?? "")
                )
            }
    }

    @ViewBuilder private func smpServerView() -> some View {
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
                    Text(serverToEdit.server)
                        .textSelection(.enabled)
                }
                useServerSection(true)
            }
        }
    }

    private func customServer() -> some View {
        VStack {
            let valid = parseServerAddress(serverToEdit.server)?.valid == true
            List {
                Section {
                    TextEditor(text: $serverToEdit.server)
                        .multilineTextAlignment(.leading)
                        .autocorrectionDisabled(true)
                        .autocapitalization(.none)
                        .allowsTightening(true)
                        .lineLimit(10)
                        .frame(height: 144)
                        .padding(-6)
                } header: {
                    HStack {
                        Text("Your server address")
                        if !valid {
                            Spacer()
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    }
                }
                useServerSection(valid)
                Section("Add to another device") {
                    MutableQRCode(uri: $serverToEdit.server)
                        .listRowInsets(EdgeInsets(top: 12, leading: 12, bottom: 12, trailing: 12))
                }
            }
        }
    }

    private func useServerSection(_ valid: Bool) -> some View {
        Section("Use server") {
            HStack {
                Button("Test server") {
                    Task {
                        if let f = await testServerConnection(server: $serverToEdit) {
                            showTestFailure = true
                            testFailure = f
                        }
                    }
                }
                .disabled(!valid)
                Spacer()
                showTestStatus(server: serverToEdit)
            }
            Toggle("Use for new connections", isOn: $serverToEdit.enabled)
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

func testServerConnection(server: Binding<ServerCfg>) async -> SMPTestFailure? {
    do {
        let r = try await testSMPServer(smpServer: server.wrappedValue.server)

            switch r {
            case .success:
                await MainActor.run { server.wrappedValue.tested = true }
                return nil
            case let .failure(f):
                await MainActor.run { server.wrappedValue.tested = false }
                return f
            }
    } catch let error {
        logger.error("testServerConnection \(responseError(error))")
        await MainActor.run {
            server.wrappedValue.tested = false
        }
        return nil
    }
}

func serverHostname(_ srv: ServerCfg) -> String {
    parseServerAddress(srv.server)?.hostnames.first ?? srv.server
}

struct SMPServerView_Previews: PreviewProvider {
    static var previews: some View {
        SMPServerView(server: Binding.constant(ServerCfg.sampleData.custom), serverToEdit: ServerCfg.sampleData.custom)
    }
}
