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
    @Environment(\.editMode) var editMode
    @State var servers: [ServerCfg] = [
        ServerCfg.sampleData.preset,
        ServerCfg.sampleData.custom,
        ServerCfg.sampleData.untested,
    ]
    @State var showAddServer = false
    @State var showSaveAlert = false

    var body: some View {
        List {
            Section("SMP servers") {
                ForEach(servers) { srv in
                    smpServerView(srv)
                }
                .onMove { indexSet, offset in
                    servers.move(fromOffsets: indexSet, toOffset: offset)
                }
                .onDelete { indexSet in
                    servers.remove(atOffsets: indexSet)
                }
                if isEditing {
                    Button("Add server…") {
                        showAddServer = true
                    }
                }
            }
        }
        .onChange(of: isEditing) { value in
            if value == false {
                showSaveAlert = true
            }
        }
        .toolbar { EditButton() }
        .confirmationDialog("Add server…", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Scan server QR code") {
            }
            Button("Add preset servers") {
            }
            Button("Enter server manually") {
                servers.append(ServerCfg.empty)
            }
        }
        .confirmationDialog("Save servers?", isPresented: $showSaveAlert, titleVisibility: .visible) {
            Button("Test & save servers") {
                for i in 0..<servers.count {
                    servers[i].tested = nil
                }
                Task {
                    for i in 0..<servers.count {
                        await testServerConnection(server: $servers[i])
                    }
                }
            }
            Button("Save servers") {
            }
            Button("Revert changes") {
            }
            Button("Cancel", role: .cancel) {
                editMode?.wrappedValue = .active
            }
        }
    }

    private var isEditing: Bool {
        editMode?.wrappedValue.isEditing == true
    }

    private func smpServerView(_ srv: ServerCfg) -> some View {
        NavigationLink {
            SMPServerView(server: srv)
                .navigationBarTitle("Server")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            let v = Text(srv.server)
            HStack {
                showTestStatus(server: srv)
                    .frame(width: 16, alignment: .center)
                    .padding(.trailing, 4)
                if srv.enabled {
                    v
                } else {
                    (v + Text(" (disabled)")).foregroundColor(.secondary)
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
