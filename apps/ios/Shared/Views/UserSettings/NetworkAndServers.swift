//
//  NetworkServersView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 02/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct NetworkAndServers: View {
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    var body: some View {
        VStack {
            List {
                Section("") {
                    NavigationLink {
                        SMPServers()
                            .navigationTitle("Your SMP servers")
                    } label: {
                        settingsRow("server.rack") { Text("SMP servers") }
                    }

                    NavigationLink {
                        AdvancedNetworkSettings()
                            .navigationTitle("Network settings")
                    } label: {
                        settingsRow("app.connected.to.app.below.fill") { Text("Advanced network settings") }
                    }
                }
            }
        }
    }
}

struct NetworkServersView_Previews: PreviewProvider {
    static var previews: some View {
        NetworkAndServers()
    }
}
