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
    var operatorServersIndex: Int
    @State var customServers: UserOperatorServers // nil operator is passed to this view
    @State private var selectedServer: String? = nil
    @State private var showAddServer = false
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
        List {
            if !customServers.smpServers.isEmpty {
                Section {
                    ForEach($customServers.smpServers) { srv in
                        ProtocolServerViewLink(
                            server: srv,
                            serverProtocol: .smp,
                            preset: false,
                            backLabel: "Your servers",
                            selectedServer: $selectedServer
                        )
                    }
                    .onDelete { indexSet in
                        customServers.smpServers.remove(atOffsets: indexSet)
                    }
                } header: {
                    Text("Message servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    Text("The servers for new connections of your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(10)
                }
            }

            if !customServers.xftpServers.isEmpty {
                Section {
                    ForEach($customServers.xftpServers) { srv in
                        ProtocolServerViewLink(
                            server: srv,
                            serverProtocol: .xftp,
                            preset: false,
                            backLabel: "Your servers",
                            selectedServer: $selectedServer
                        )
                    }
                    .onMove { indexSet, offset in
                        customServers.xftpServers.move(fromOffsets: indexSet, toOffset: offset)
                    }
                    .onDelete { indexSet in
                        customServers.xftpServers.remove(atOffsets: indexSet)
                    }
                } header: {
                    Text("Media & file servers")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    Text("The servers for new files of your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(10)
                }
            }

            Section {
                Button("Add server") {
                    showAddServer = true
                }
            }

            Section {
                TestServersButton(
                    smpServers: $customServers.smpServers,
                    xftpServers: $customServers.xftpServers,
                    testing: $testing
                )
                howToButton()
            }
        }
        .toolbar { EditButton() }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            userServers[operatorServersIndex] = customServers
            dismiss()
        })
        .confirmationDialog("Add server", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Enter SMP server manually") {
                customServers.smpServers.append(UserServer.empty)
                selectedServer = customServers.smpServers.last?.id
            }
            Button("Enter XFTP server manually") {
                customServers.xftpServers.append(UserServer.empty)
                selectedServer = customServers.xftpServers.last?.id
            }
            Button("Scan server QR code") { showScanProtoServer = true }
        }
        .sheet(isPresented: $showScanProtoServer) {
            ScanProtocolServer(smpServers: $customServers.smpServers, xftpServers: $customServers.xftpServers)
                .modifier(ThemedBackground(grouped: true))
        }
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
    @Binding var server: UserServer
    var serverProtocol: ServerProtocol
    var preset: Bool
    var backLabel: LocalizedStringKey
    @Binding var selectedServer: String?

    var body: some View {
        let proto = serverProtocol.rawValue.uppercased()

        NavigationLink(tag: server.id, selection: $selectedServer) {
            ProtocolServerView(
                serverProtocol: serverProtocol,
                server: $server,
                serverToEdit: server,
                preset: preset,
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
                            // TODO Show based on validateServers
                            // } else if !uniqueAddress(srv, address) {
                            //     Image(systemName: "exclamationmark.circle").foregroundColor(.red)
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
            operatorServersIndex: 1,
            customServers: UserOperatorServers.sampleDataNilOperator
        )
    }
}
