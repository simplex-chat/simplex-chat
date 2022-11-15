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
    @State var servers: [SMPServerCfg] = [
        SMPServerCfg.sampleData.name,
        SMPServerCfg.sampleData.params
    ]
    @State var showAddServer = false

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
                Button("Add server…") {
                    showAddServer = true
                }
            }
        }
        .toolbar { EditButton() }
        .confirmationDialog("Add server…", isPresented: $showAddServer, titleVisibility: .hidden) {
            Button("Scan server QR code") {

            }
            Button("Add preset servers") {
            }
            Button("Enter server manually") {
                servers.append(SMPServerCfg(server: .params(params: SMPServerParams.empty), tested: false, enabled: false))
            }
        }
    }

    private func smpServerView(_ srv: SMPServerCfg) -> some View {
        NavigationLink {
            SMPServerView(server: srv)
                .navigationBarTitle("Server")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            let v = Text(srv.server.label)
            if srv.enabled {
                v
            } else {
                (v + Text(" (disabled)")).foregroundColor(.secondary)
            }
        }
    }
}

struct SMPServersView_Previews: PreviewProvider {
    static var previews: some View {
        SMPServersView()
    }
}
