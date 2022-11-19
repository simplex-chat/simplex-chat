//
//  SMPServersView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SMPServersView: View {
    @EnvironmentObject private var m: ChatModel
    @Environment(\.editMode) private var editMode
    @State private var servers = ChatModel.shared.userSMPServers ?? []
    @State private var selectedServer: String? = nil
    @State private var showAddServer = false
    @State private var showScanSMPServer = false
    @State private var testing = false

    var body: some View {
        ZStack {
            smpServersView()
            if testing {
                ProgressView().scaleEffect(2)
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
                Button("Reset") {
                    servers = m.userSMPServers ?? []
                }
                .disabled(servers == m.userSMPServers || testing)
                Button("Test servers") {
                    resetTestStatus()
                    testing = true
                    Task {
                        _ = await testServers()
                        await MainActor.run {
                            testing = false
                        }
                        // TODO show alert if not passed
                    }
                }
                .disabled(testing)
                Button("Save servers") {
                    saveSMPServers()
                }
                .disabled(servers.count == 0 || servers == m.userSMPServers || testing || !servers.allSatisfy { serverHostname($0) != nil })
            }
        }
        .toolbar { EditButton() }
        .confirmationDialog("Add server…", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Enter server manually") {
                servers.append(ServerCfg.empty)
                selectedServer = servers.last?.id
            }
            Button("Scan server QR code") {
                showScanSMPServer = true
            }
            Button("Add preset servers") {
                addAllPresets()
            }
            .disabled(hasAllPresets())
        }
        .sheet(isPresented: $showScanSMPServer) {
            ScanSMPServer(servers: $servers)
        }
    }

    private var isEditing: Bool {
        editMode?.wrappedValue.isEditing == true
    }

    private func smpServerView(_ server: Binding<ServerCfg>) -> some View {
        let srv = server.wrappedValue
        return NavigationLink(tag: srv.id, selection: $selectedServer) {
            SMPServerView(server: server, serverToEdit: srv)
                .navigationBarTitle(srv.preset ? "Preset server" : "Your server")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            let hostname = serverHostname(srv)
            HStack {
                Group {
                    if hostname == nil {
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        // TODO show duplicate servers with error
                    } else if !srv.enabled {
                        Image(systemName: "slash.circle").foregroundColor(.secondary)
                    } else {
                        showTestStatus(server: srv)
                    }
                }
                .frame(width: 16, alignment: .center)
                .padding(.trailing, 4)

                let v = Text(hostname ?? srv.server).lineLimit(1)
                if srv.enabled {
                    v
                } else {
                    v.foregroundColor(.secondary)
                }
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

    private func resetTestStatus() {
        for i in 0..<servers.count {
            if servers[i].enabled {
                servers[i].tested = nil
            }
        }
    }

    private func testServers() async -> Bool {
        for i in 0..<servers.count {
            if servers[i].enabled {
                await testServerConnection(server: $servers[i])
            }
        }
        return servers.allSatisfy { $0.tested == true }
    }

    func saveSMPServers() {
        Task {
            do {
                try await setUserSMPServers(smpServers: servers)
                await MainActor.run {
                    m.userSMPServers = servers
                    editMode?.wrappedValue = .inactive
//                    if smpServers.isEmpty {
//                        isUserSMPServers = false
//                        editSMPServers = true
//                    } else {
//                        editSMPServers = false
//                    }
                }
            } catch {
                let err = error.localizedDescription
                logger.error("SMPServers.saveServers setUserSMPServers error: \(err)")

                // TODO show alert if not saved

//                await MainActor.run {
//                    showBadServersAlert = true
//                }
            }
        }
    }
}

struct SMPServersView_Previews: PreviewProvider {
    static var previews: some View {
        SMPServersView()
    }
}
