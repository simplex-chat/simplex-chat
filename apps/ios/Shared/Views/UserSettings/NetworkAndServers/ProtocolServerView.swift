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
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    @Binding var server: UserServer
    @State var serverToEdit: UserServer
    var backLabel: LocalizedStringKey
    @State private var showTestFailure = false
    @State private var testing = false
    @State private var testFailure: ProtocolTestFailure?

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
        .modifier(BackButton(label: backLabel, disabled: Binding.constant(false)) {
            if let (serverToEditProtocol, serverToEditOperator) = serverProtocolAndOperator(serverToEdit, userServers),
               let (serverProtocol, serverOperator) = serverProtocolAndOperator(server, userServers) {
                if serverToEditProtocol != serverProtocol {
                    dismiss()
                    showAlert(
                        NSLocalizedString("Error updating server", comment: "alert title"),
                        message: NSLocalizedString("Server protocol changed.", comment: "alert title")
                    )
                } else if serverToEditOperator != serverOperator {
                    dismiss()
                    showAlert(
                        NSLocalizedString("Error updating server", comment: "alert title"),
                        message: NSLocalizedString("Server operator changed.", comment: "alert title")
                    )
                } else {
                    server = serverToEdit
                    validateServers_($userServers, $serverErrors)
                    dismiss()
                }
            } else {
                dismiss()
                showAlert(
                    NSLocalizedString("Invalid server address!", comment: "alert title"),
                    message: NSLocalizedString("Check server address and try again.", comment: "alert title")
                )
            }
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
            let valid = serverAddress?.valid == true
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
            Toggle("Use for new connections", isOn: $serverToEdit.enabled)
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

@ViewBuilder func showTestStatus(server: UserServer) -> some View {
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

func testServerConnection(server: Binding<UserServer>) async -> ProtocolTestFailure? {
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

struct ProtocolServerView_Previews: PreviewProvider {
    static var previews: some View {
        ProtocolServerView(
            userServers: Binding.constant([UserOperatorServers.sampleDataNilOperator]),
            serverErrors: Binding.constant([]),
            server: Binding.constant(UserServer.sampleData.custom),
            serverToEdit: UserServer.sampleData.custom,
            backLabel: "Your SMP servers"
        )
    }
}
