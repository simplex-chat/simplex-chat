//
//  NewServerView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 13.11.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct NewServerView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    @State private var serverToEdit: UserServer = .empty
    @State private var showTestFailure = false
    @State private var testing = false
    @State private var testFailure: ProtocolTestFailure?

    var body: some View {
        ZStack {
            customServer()
            if testing {
                ProgressView().scaleEffect(2)
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            addServer(serverToEdit, $userServers, $serverErrors, dismiss)
        })
        .alert(isPresented: $showTestFailure) {
            Alert(
                title: Text("Server test failed!"),
                message: Text(testFailure?.localizedDescription ?? "")
            )
        }
    }

    // TODO Possibly refactor - similar functions in ProtocolServerView
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

func serverProtocolAndOperator(_ server: UserServer, _ userServers: [UserOperatorServers]) -> (ServerProtocol, ServerOperator?)? {
    if let serverAddress = parseServerAddress(server.server) {
        let serverProtocol = serverAddress.serverProtocol
        let hostnames = serverAddress.hostnames
        let matchingOperator = userServers.compactMap { $0.operator }.first { op in
            op.serverDomains.contains { domain in
                hostnames.contains { hostname in
                    hostname.hasSuffix(domain)
                }
            }
        }
        return (serverProtocol, matchingOperator)
    } else {
        return nil
    }
}

func addServer(
    _ server: UserServer,
    _ userServers: Binding<[UserOperatorServers]>,
    _ serverErrors: Binding<[UserServersError]>,
    _ dismiss: DismissAction
) {
    if let (serverProtocol, matchingOperator) = serverProtocolAndOperator(server, userServers.wrappedValue) {
        if let i = userServers.wrappedValue.firstIndex(where: { $0.operator?.operatorId == matchingOperator?.operatorId }) {
            switch serverProtocol {
            case .smp: userServers[i].wrappedValue.smpServers.append(server)
            case .xftp: userServers[i].wrappedValue.xftpServers.append(server)
            }
            validateServers_(userServers, serverErrors)
            dismiss()
            if let op = matchingOperator {
                showAlert(
                    NSLocalizedString("Operator server", comment: "alert title"),
                    message: String.localizedStringWithFormat(NSLocalizedString("Server added to operator %@.", comment: "alert message"), op.tradeName)
                )
            }
        } else { // Shouldn't happen
            dismiss()
            showAlert(NSLocalizedString("Error adding server", comment: "alert title"))
        }
    } else {
        dismiss()
        if server.server.trimmingCharacters(in: .whitespaces) != "" {
            showAlert(
                NSLocalizedString("Invalid server address!", comment: "alert title"),
                message: NSLocalizedString("Check server address and try again.", comment: "alert title")
            )
        }
    }
}

#Preview {
    NewServerView(
        userServers: Binding.constant([UserOperatorServers.sampleDataNilOperator]),
        serverErrors: Binding.constant([])
    )
}
