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

struct ProtocolServersView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject private var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.editMode) private var editMode
    let serverProtocol: ServerProtocol
    @State private var currServers: [UserServer] = []
    @State private var presetServers: [UserServer] = []
    @State private var servers: [UserServer] = []
    @State private var selectedServer: String? = nil
    @State private var showAddServer = false
    @State private var showScanProtoServer = false
    @State private var justOpened = true
    @State private var testing = false
    @State private var alert: ServerAlert? = nil
    @State private var showSaveDialog = false

    var proto: String { serverProtocol.rawValue.uppercased() }

    var body: some View {
        ZStack {
            protocolServersView()
            if testing {
                ProgressView().scaleEffect(2)
            }
        }
    }

    enum ServerAlert: Identifiable {
        case testsFailed(failures: [String: ProtocolTestFailure])
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case .testsFailed: return "testsFailed"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    private func protocolServersView() -> some View {
        List {
            Section {
                ForEach($servers) { srv in
                    protocolServerView(srv)
                }
                .onMove { indexSet, offset in
                    servers.move(fromOffsets: indexSet, toOffset: offset)
                }
                .onDelete { indexSet in
                    servers.remove(atOffsets: indexSet)
                }
                Button("Add server") {
                    showAddServer = true
                }
            } header: {
                Text("\(proto) servers")
                    .foregroundColor(theme.colors.secondary)
            } footer: {
                Text("The servers for new connections of your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                    .foregroundColor(theme.colors.secondary)
                    .lineLimit(10)
            }

            Section {
                Button("Reset") { servers = currServers }
                    .disabled(servers == currServers || testing)
                Button("Test servers", action: testServers)
                    .disabled(testing || allServersDisabled)
                Button("Save servers", action: saveServers)
                    .disabled(saveDisabled)
                howToButton()
            }
        }
        .toolbar { EditButton() }
        .confirmationDialog("Add server", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Enter server manually") {
                servers.append(UserServer.empty)
                selectedServer = servers.last?.id
            }
            Button("Scan server QR code") { showScanProtoServer = true }
            Button("Add preset servers", action: addAllPresets)
                .disabled(hasAllPresets())
        }
        .sheet(isPresented: $showScanProtoServer) {
            ScanProtocolServer(servers: $servers)
                .modifier(ThemedBackground(grouped: true))
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if saveDisabled {
                dismiss()
                justOpened = false
            } else {
                showSaveDialog = true
            }
        })
        .confirmationDialog("Save servers?", isPresented: $showSaveDialog, titleVisibility: .visible) {
            Button("Save") {
                saveServers()
                dismiss()
                justOpened = false
            }
            Button("Exit without saving") { dismiss() }
        }
        .alert(item: $alert) { a in
            switch a {
            case let .testsFailed(fs):
                let msg = fs.map { (srv, f) in
                    "\(srv): \(f.localizedDescription)"
                }.joined(separator: "\n")
                return Alert(
                    title: Text("Tests failed!"),
                    message: Text("Some servers failed the test:\n" + msg)
                )
            case .error:
                return Alert(
                    title: Text("Error")
                )
            }
        }
        .onAppear {
            // this condition is needed to prevent re-setting the servers when exiting single server view
            if justOpened {
                do {
                    let r = try getUserProtoServers(serverProtocol)
                    currServers = r.protoServers
                    presetServers = r.presetServers
                    servers = currServers
                } catch let error {
                    alert = .error(
                        title: "Error loading \(proto) servers",
                        error: "Error: \(responseError(error))"
                    )
                }
                justOpened = false
            }
        }
    }

    private var saveDisabled: Bool {
        servers.isEmpty ||
        servers == currServers ||
        testing ||
        !servers.allSatisfy { srv in
            if let address = parseServerAddress(srv.server) {
                return uniqueAddress(srv, address)
            }
            return false
        } ||
        allServersDisabled
    }

    private var allServersDisabled: Bool {
        servers.allSatisfy { !$0.enabled }
    }

    private func protocolServerView(_ server: Binding<UserServer>) -> some View {
        let srv = server.wrappedValue
        return NavigationLink(tag: srv.id, selection: $selectedServer) {
            ProtocolServerView(
                serverProtocol: serverProtocol,
                server: server,
                serverToEdit: srv,
                preset: false,
                backLabel: "Your \(proto) servers"
            )
            .navigationBarTitle("Your server")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            let address = parseServerAddress(srv.server)
            HStack {
                Group {
                    if let address = address {
                        if !address.valid || address.serverProtocol != serverProtocol {
                            invalidServer()
                        } else if !uniqueAddress(srv, address) {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        } else if !srv.enabled {
                            Image(systemName: "slash.circle").foregroundColor(theme.colors.secondary)
                        } else {
                            showTestStatus(server: srv)
                        }
                    } else {
                        invalidServer()
                    }
                }
                .frame(width: 16, alignment: .center)
                .padding(.trailing, 4)

                let v = Text(address?.hostnames.first ?? srv.server).lineLimit(1)
                if srv.enabled {
                    v
                } else {
                    v.foregroundColor(theme.colors.secondary)
                }
            }
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

    private func invalidServer() -> some View {
        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
    }

    private func uniqueAddress(_ s: UserServer, _ address: ServerAddress) -> Bool {
        servers.allSatisfy { srv in
            address.hostnames.allSatisfy { host in
                srv.id == s.id || !srv.server.contains(host)
            }
        }
    }

    private func hasAllPresets() -> Bool {
        presetServers.allSatisfy { hasPreset($0) }
    }

    private func addAllPresets() {
        for srv in presetServers {
            if !hasPreset(srv) {
                servers.append(srv)
            }
        }
    }

    private func hasPreset(_ srv: UserServer) -> Bool {
        servers.contains(where: { $0.server == srv.server })
    }

    private func testServers() {
        resetTestStatus()
        testing = true
        Task {
            let fs = await runServersTest()
            await MainActor.run {
                testing = false
                if !fs.isEmpty {
                    alert = .testsFailed(failures: fs)
                }
            }
        }
    }

    private func resetTestStatus() {
        for i in 0..<servers.count {
            if servers[i].enabled {
                servers[i].tested = nil
            }
        }
    }

    private func runServersTest() async -> [String: ProtocolTestFailure] {
        var fs: [String: ProtocolTestFailure] = [:]
        for i in 0..<servers.count {
            if servers[i].enabled {
                if let f = await testServerConnection(server: $servers[i]) {
                    fs[serverHostname(servers[i].server)] = f
                }
            }
        }
        return fs
    }

    func saveServers() {
        Task {
            do {
                try await setUserProtoServers(serverProtocol, servers: servers)
                await MainActor.run {
                    currServers = servers
                    editMode?.wrappedValue = .inactive
                }
            } catch let error {
                let err = responseError(error)
                logger.error("saveServers setUserProtocolServers error: \(err)")
                await MainActor.run {
                    alert = .error(
                        title: "Error saving \(proto) servers",
                        error: "Make sure \(proto) server addresses are in correct format, line separated and are not duplicated (\(responseError(error)))."
                    )
                }
            }
        }
    }
}

struct ProtocolServersView_Previews: PreviewProvider {
    static var previews: some View {
        ProtocolServersView(serverProtocol: .smp)
    }
}
