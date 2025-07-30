//
//  ProtocolServersView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let howToUrl = URL(string: "https://simplex.chat/docs/server.html")!

struct YourServersView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject private var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.editMode) private var editMode
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    var operatorIndex: Int
    @State private var selectedServer: String? = nil
    @State private var showAddServer = false
    @State private var newServerNavLinkActive = false
    @State private var showScanProtoServer = false
    @State private var testing = false

    var body: some View {
        yourServersView()
            .opacity(testing ? 0.4 : 1)
            .overlay {
                if testing {
                    ProgressView()
                        .scaleEffect(2)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                }
            }
            .allowsHitTesting(!testing)
    }

    private func yourServersView() -> some View {
        let duplicateHosts = findDuplicateHosts(serverErrors)
        return List {
            if !userServers[operatorIndex].smpServers.filter({ !$0.deleted }).isEmpty {
                Section {
                    ForEach($userServers[operatorIndex].smpServers) { srv in
                        if !srv.wrappedValue.deleted {
                            ProtocolServerViewLink(
                                userServers: $userServers,
                                serverErrors: $serverErrors,
                                duplicateHosts: duplicateHosts,
                                server: srv,
                                serverProtocol: .smp,
                                backLabel: "Your servers",
                                selectedServer: $selectedServer
                            )
                        } else {
                            EmptyView()
                        }
                    }
                    .onDelete { indexSet in
                        deleteSMPServer($userServers, operatorIndex, indexSet)
                        validateServers_($userServers, $serverErrors)
                    }
                } header: {
                    Text("Message servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    if let errStr = globalSMPServersError(serverErrors) {
                        ServersErrorView(errStr: errStr)
                    } else {
                        Text("The servers for new connections of your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(10)
                    }
                }
            }

            if !userServers[operatorIndex].xftpServers.filter({ !$0.deleted }).isEmpty {
                Section {
                    ForEach($userServers[operatorIndex].xftpServers) { srv in
                        if !srv.wrappedValue.deleted {
                            ProtocolServerViewLink(
                                userServers: $userServers,
                                serverErrors: $serverErrors,
                                duplicateHosts: duplicateHosts,
                                server: srv,
                                serverProtocol: .xftp,
                                backLabel: "Your servers",
                                selectedServer: $selectedServer
                            )
                        } else {
                            EmptyView()
                        }
                    }
                    .onDelete { indexSet in
                        deleteXFTPServer($userServers, operatorIndex, indexSet)
                        validateServers_($userServers, $serverErrors)
                    }
                } header: {
                    Text("Media & file servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    if let errStr = globalXFTPServersError(serverErrors) {
                        ServersErrorView(errStr: errStr)
                    } else {
                        Text("The servers for new files of your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(10)
                    }
                }
            }

            Section {
                ZStack {
                    Button("Add server") {
                        showAddServer = true
                    }

                    NavigationLink(isActive: $newServerNavLinkActive) {
                        newServerDestinationView()
                    } label: {
                        EmptyView()
                    }
                    .frame(width: 1, height: 1)
                    .hidden()
                }
            } footer: {
                if let errStr = globalServersError(serverErrors) {
                    ServersErrorView(errStr: errStr)
                }
            }

            Section {
                TestServersButton(
                    smpServers: $userServers[operatorIndex].smpServers,
                    xftpServers: $userServers[operatorIndex].xftpServers,
                    testing: $testing
                )
                howToButton()
            }
        }
        .toolbar {
            if (
                !userServers[operatorIndex].smpServers.filter({ !$0.deleted }).isEmpty ||
                !userServers[operatorIndex].xftpServers.filter({ !$0.deleted }).isEmpty
            ) {
                EditButton()
            }
        }
        .confirmationDialog("Add server", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Enter server manually") { newServerNavLinkActive = true }
            Button("Scan server QR code") { showScanProtoServer = true }
        }
        .sheet(isPresented: $showScanProtoServer) {
            ScanProtocolServer(
                userServers: $userServers,
                serverErrors: $serverErrors
            )
            .modifier(ThemedBackground(grouped: true))
        }
    }

    private func newServerDestinationView() -> some View {
        NewServerView(
            userServers: $userServers,
            serverErrors: $serverErrors
        )
        .navigationTitle("New server")
        .navigationBarTitleDisplayMode(.large)
        .modifier(ThemedBackground(grouped: true))
    }

    func howToButton() -> some View {
        Button {
            DispatchQueue.main.async {
                UIApplication.shared.open(howToUrl)
            }
        } label: {
            HStack {
                Text("How to use your servers")
                Image(systemName: "arrow.up.right.circle")
            }
        }
    }
}

struct ProtocolServerViewLink: View {
    @EnvironmentObject var theme: AppTheme
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    var duplicateHosts: Set<String>
    @Binding var server: UserServer
    var serverProtocol: ServerProtocol
    var backLabel: LocalizedStringKey
    @Binding var selectedServer: String?

    var body: some View {
        let proto = serverProtocol.rawValue.uppercased()

        NavigationLink(tag: server.id, selection: $selectedServer) {
            ProtocolServerView(
                userServers: $userServers,
                serverErrors: $serverErrors,
                server: $server,
                serverToEdit: server,
                backLabel: backLabel
            )
            .navigationBarTitle("\(proto) server")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            let address = parseServerAddress(server.server)
            HStack {
                Group {
                    if let address = address {
                        if !address.valid || address.serverProtocol != serverProtocol {
                            invalidServer()
                        } else if address.hostnames.contains(where: duplicateHosts.contains) {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        } else if !server.enabled {
                            Image(systemName: "slash.circle").foregroundColor(theme.colors.secondary)
                        } else {
                            showTestStatus(server: server)
                        }
                    } else {
                        invalidServer()
                    }
                }
                .frame(width: 16, alignment: .center)
                .padding(.trailing, 4)

                let v = Text(address?.hostnames.first ?? server.server).lineLimit(1)
                if server.enabled {
                    v
                } else {
                    v.foregroundColor(theme.colors.secondary)
                }
            }
        }
    }

    private func invalidServer() -> some View {
        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
    }
}

func deleteSMPServer(
    _ userServers: Binding<[UserOperatorServers]>,
    _ operatorServersIndex: Int,
    _ serverIndexSet: IndexSet
) {
    if let idx = serverIndexSet.first {
        let server = userServers[operatorServersIndex].wrappedValue.smpServers[idx]
        if server.serverId == nil {
            userServers[operatorServersIndex].wrappedValue.smpServers.remove(at: idx)
        } else {
            var updatedServer = server
            updatedServer.deleted = true
            userServers[operatorServersIndex].wrappedValue.smpServers[idx] = updatedServer
        }
    }
}

func deleteXFTPServer(
    _ userServers: Binding<[UserOperatorServers]>,
    _ operatorServersIndex: Int,
    _ serverIndexSet: IndexSet
) {
    if let idx = serverIndexSet.first {
        let server = userServers[operatorServersIndex].wrappedValue.xftpServers[idx]
        if server.serverId == nil {
            userServers[operatorServersIndex].wrappedValue.xftpServers.remove(at: idx)
        } else {
            var updatedServer = server
            updatedServer.deleted = true
            userServers[operatorServersIndex].wrappedValue.xftpServers[idx] = updatedServer
        }
    }
}

struct TestServersButton: View {
    @Binding var smpServers: [UserServer]
    @Binding var xftpServers: [UserServer]
    @Binding var testing: Bool

    var body: some View {
        Button("Test servers", action: testServers)
            .disabled(testing || allServersDisabled)
    }

    private var allServersDisabled: Bool {
        smpServers.allSatisfy { !$0.enabled } && xftpServers.allSatisfy { !$0.enabled }
    }

    private func testServers() {
        resetTestStatus()
        testing = true
        Task {
            let fs = await runServersTest()
            await MainActor.run {
                testing = false
                if !fs.isEmpty {
                    let msg = fs.map { (srv, f) in
                        "\(srv): \(f.localizedDescription)"
                    }.joined(separator: "\n")
                    showAlert(
                        NSLocalizedString("Tests failed!", comment: "alert title"),
                        message: String.localizedStringWithFormat(NSLocalizedString("Some servers failed the test:\n%@", comment: "alert message"), msg)
                    )
                }
            }
        }
    }

    private func resetTestStatus() {
        for i in 0..<smpServers.count {
            if smpServers[i].enabled {
                smpServers[i].tested = nil
            }
        }

        for i in 0..<xftpServers.count {
            if xftpServers[i].enabled {
                xftpServers[i].tested = nil
            }
        }
    }

    private func runServersTest() async -> [String: ProtocolTestFailure] {
        var fs: [String: ProtocolTestFailure] = [:]
        for i in 0..<smpServers.count {
            if smpServers[i].enabled {
                if let f = await testServerConnection(server: $smpServers[i]) {
                    fs[serverHostname(smpServers[i].server)] = f
                }
            }
        }

        for i in 0..<xftpServers.count {
            if xftpServers[i].enabled {
                if let f = await testServerConnection(server: $xftpServers[i]) {
                    fs[serverHostname(xftpServers[i].server)] = f
                }
            }
        }
        return fs
    }
}

struct YourServersView_Previews: PreviewProvider {
    static var previews: some View {
        YourServersView(
            userServers: Binding.constant([UserOperatorServers.sampleDataNilOperator]),
            serverErrors: Binding.constant([]),
            operatorIndex: 1
        )
    }
}
