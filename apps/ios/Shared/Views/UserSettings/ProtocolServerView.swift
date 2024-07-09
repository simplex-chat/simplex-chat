//
//  SMPServerView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ProtocolServerView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    let serverProtocol: ServerProtocol
    @Binding var server: ServerCfg
    @State var serverToEdit: ServerCfg
    @State var serverEnabled: Bool
    @State private var showTestFailure = false
    @State private var testing = false
    @State private var testFailure: ProtocolTestFailure?

    var proto: String { serverProtocol.rawValue.uppercased() }

    var body: some View {
        ZStack {
            if server.preset {
                presetServer()
            } else {
                customServer()
            }
            if testing {
                ProgressView().scaleEffect(2)
            }
        }
        .modifier(BackButton(label: "Your \(proto) servers", disabled: Binding.constant(false)) {
            server = serverToEdit
            dismiss()
        })
        .alert(isPresented: $showTestFailure) {
            Alert(
                title: Text("Server test failed!"),
                message: Text(testFailure?.localizedDescription ?? "")
            )
        }
        .onChange(of: serverToEdit.server) { _ in
            serverToEdit.tested = serverToEdit.server == server.server ? server.tested : nil
        }
    }

    private func presetServer() -> some View {
        return VStack {
            List {
                Section(header: Text("Preset server address").foregroundColor(theme.colors.secondary)) {
                    Text(serverToEdit.server)
                        .textSelection(.enabled)
                }
                useServerSection(true)
            }
        }
    }

    private func customServer() -> some View {
        VStack {
            let serverAddress = parseServerAddress(serverToEdit.server)
            let valid = serverAddress?.valid == true && serverAddress?.serverProtocol == serverProtocol
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
                            .foregroundColor(theme.colors.secondary)
                        if !valid {
                            Spacer()
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    }
                }
                useServerSection(valid)
                if valid {
                    Section(header: Text("Add to another device").foregroundColor(theme.colors.secondary)) {
                        MutableQRCode(uri: $serverToEdit.server)
                            .listRowInsets(EdgeInsets(top: 12, leading: 12, bottom: 12, trailing: 12))
                    }
                }
            }
        }
    }

    private func useServerSection(_ valid: Bool) -> some View {
        Section(header: Text("Use server").foregroundColor(theme.colors.secondary)) {
            HStack {
                Button("Test server") {
                    testing = true
                    serverToEdit.tested = nil
                    Task {
                        if let f = await testServerConnection(server: $serverToEdit) {
                            showTestFailure = true
                            testFailure = f
                        }
                        await MainActor.run { testing = false }
                    }
                }
                .disabled(!valid || testing)
                Spacer()
                showTestStatus(server: serverToEdit)
            }
            Toggle("Use for new connections", isOn: $serverEnabled)
                .onChange(of: serverEnabled) { enabled in
                    serverToEdit.enabled = enabled ? .enabled : .disabled
                }
        }
    }
}

struct BackButton: ViewModifier {
    var label: LocalizedStringKey = "Back"
    @Binding var disabled: Bool
    var action: () -> Void

    func body(content: Content) -> some View {
        content
        .navigationBarBackButtonHidden(true)
        .toolbar {
            ToolbarItem(placement: .navigationBarLeading) {
                Button(action: action) {
                    HStack {
                        Image(systemName: "chevron.left")
                        Text(label)
                    }
                }
                .disabled(disabled)
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

func testServerConnection(server: Binding<ServerCfg>) async -> ProtocolTestFailure? {
    do {
        let r = try await testProtoServer(server: server.wrappedValue.server)
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

func serverHostname(_ srv: String) -> String {
    parseServerAddress(srv)?.hostnames.first ?? srv
}

struct ProtocolServerView_Previews: PreviewProvider {
    static var previews: some View {
        ProtocolServerView(
            serverProtocol: .smp,
            server: Binding.constant(ServerCfg.sampleData.custom),
            serverToEdit: ServerCfg.sampleData.custom,
            serverEnabled: true
        )
    }
}
