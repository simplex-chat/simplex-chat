//
//  SMPServersView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let howToUrl = URL(string: "https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SERVER.md")!

struct SMPServersView: View {
    @EnvironmentObject private var m: ChatModel
    @Environment(\.editMode) private var editMode
    @State private var servers = ChatModel.shared.userSMPServers ?? []
    @State private var selectedServer: String? = nil
    @State private var showAddServer = false
    @State private var showScanSMPServer = false
    @State private var testing = false
    @State private var alert: SMPServerAlert? = nil

    var body: some View {
        ZStack {
            smpServersView()
            if testing {
                ProgressView().scaleEffect(2)
            }
        }
    }

    enum SMPServerAlert: Identifiable {
        case testsFailed(failures: [String: SMPTestFailure])
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case .testsFailed: return "testsFailed"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    private func smpServersView() -> some View {
        List {
            Section("SMP servers") {
                ForEach($servers) { srv in
                    smpServerView(srv)
                }
                .onMove { indexSet, offset in
                    servers.move(fromOffsets: indexSet, toOffset: offset)
                }
                .onDelete { indexSet in
                    servers.remove(atOffsets: indexSet)
                }
                Button("Add server…") {
                    showAddServer = true
                }
            }

            Section {
                Button("Reset") { servers = m.userSMPServers ?? [] }
                    .disabled(servers == m.userSMPServers || testing)
                Button("Test servers", action: testServers)
                    .disabled(testing || allServersDisabled)
                Button("Save servers", action: saveSMPServers)
                    .disabled(saveDisabled)
                howToButton()
            }
        }
        .toolbar { EditButton() }
        .confirmationDialog("Add server…", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Enter server manually") {
                servers.append(ServerCfg.empty)
                selectedServer = servers.last?.id
            }
            Button("Scan server QR code") { showScanSMPServer = true }
            Button("Add preset servers", action: addAllPresets)
                .disabled(hasAllPresets())
        }
        .sheet(isPresented: $showScanSMPServer) {
            ScanSMPServer(servers: $servers)
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
    }

    private var saveDisabled: Bool {
        servers.isEmpty ||
        servers == m.userSMPServers ||
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

    private func smpServerView(_ server: Binding<ServerCfg>) -> some View {
        let srv = server.wrappedValue
        return NavigationLink(tag: srv.id, selection: $selectedServer) {
            SMPServerView(server: server, serverToEdit: srv)
                .navigationBarTitle(srv.preset ? "Preset server" : "Your server")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            let address = parseServerAddress(srv.server)
            HStack {
                Group {
                    if let address = address {
                        if !address.valid {
                            invalidServer()
                        } else if !uniqueAddress(srv, address) {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        } else if !srv.enabled {
                            Image(systemName: "slash.circle").foregroundColor(.secondary)
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
                    v.foregroundColor(.secondary)
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

    private func uniqueAddress(_ s: ServerCfg, _ address: ServerAddress) -> Bool {
        servers.allSatisfy { srv in
            address.hostnames.allSatisfy { host in
                srv.id == s.id || !srv.server.contains(host)
            }
        }
    }

    private func hasAllPresets() -> Bool {
        m.presetSMPServers?.allSatisfy { hasPreset($0) } ?? true
    }

    private func addAllPresets() {
        for srv in m.presetSMPServers ?? [] {
            if !hasPreset(srv) {
                servers.append(ServerCfg(server: srv, preset: true, tested: nil, enabled: true))
            }
        }
    }

    private func hasPreset(_ srv: String) -> Bool {
        servers.contains(where: { $0.server == srv })
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

    private func runServersTest() async -> [String: SMPTestFailure] {
        var fs: [String: SMPTestFailure] = [:]
        for i in 0..<servers.count {
            if servers[i].enabled {
                if let f = await testServerConnection(server: $servers[i]) {
                    fs[serverHostname(servers[i].server)] = f
                }
            }
        }
        return fs
    }

    func saveSMPServers() {
        Task {
            do {
                try await setUserSMPServers(smpServers: servers)
                await MainActor.run {
                    m.userSMPServers = servers
                    editMode?.wrappedValue = .inactive
                }
            } catch let error {
                let err = responseError(error)
                logger.error("saveSMPServers setUserSMPServers error: \(err)")
                await MainActor.run {
                    alert = .error(
                        title: "Error saving SMP servers",
                        error: "Make sure SMP server addresses are in correct format, line separated and are not duplicated (\(responseError(error)))."
                    )
                }
            }
        }
    }
}

struct SMPServersView_Previews: PreviewProvider {
    static var previews: some View {
        SMPServersView()
    }
}
