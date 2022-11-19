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
    @EnvironmentObject var m: ChatModel
    @Environment(\.editMode) var editMode
    @State var servers = ChatModel.shared.userSMPServers ?? []
    @State var showAddServer = false

    var body: some View {
        List {
            Section("SMP servers") {
                ForEach($servers) { srv in
                    // TODO make view ID include something unique
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
                Button("Save servers") {
                    saveSMPServers()
                }
                Button("Test & save servers") {
                    resetTestStatus()
                    Task {
                        if await testServers() {
                            saveSMPServers()
                        }
                        // TODO show alert if not passed
                    }
                }
            }
            .disabled(servers.count == 0 || servers == m.userSMPServers || !servers.allSatisfy { serverHostname($0) != nil })
        }
        .toolbar { EditButton() }
        .confirmationDialog("Add server…", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Scan server QR code") {
                // TODO
            }
            Button("Add preset servers") {
                for srv in m.presetSMPServers ?? [] {
                    if !servers.contains(where: { $0.server == srv }) {
                        servers.append(ServerCfg(server: srv, preset: true, tested: nil, enabled: true))
                    }
                }
            }
            Button("Enter server manually") {
                servers.append(ServerCfg.empty)
                // TODO open detail view with that server
            }
        }
    }

    private var isEditing: Bool {
        editMode?.wrappedValue.isEditing == true
    }

    private func smpServerView(_ server: Binding<ServerCfg>) -> some View {
        let srv = server.wrappedValue
        return NavigationLink {
            SMPServerView(server: server, serverToEdit: srv)
                .navigationBarTitle("Server")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            let hostname = serverHostname(srv)
            HStack {
                Group {
                    if hostname == nil {
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
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

    private func serverHostname(_ srv: ServerCfg) -> String? {
        parseServerAddress(srv.server)?.hostnames.first
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
