//
//  DeveloperView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/03/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct DeveloperView: View {
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @AppStorage(GROUP_DEFAULT_CONFIRM_DB_UPGRADES, store: groupDefaults) private var confirmDatabaseUpgrades = false
    @AppStorage(GROUP_DEFAULT_XFTP_SEND_ENABLED, store: groupDefaults) private var xftpSendEnabled = false
    @Environment(\.colorScheme) var colorScheme

    var body: some View {
        VStack {
            List {
                Section {
                    ZStack(alignment: .leading) {
                        Image(colorScheme == .dark ? "github_light" : "github")
                            .resizable()
                            .frame(width: 24, height: 24)
                            .opacity(0.5)
                        Text("Install [SimpleX Chat for terminal](https://github.com/simplex-chat/simplex-chat)")
                            .padding(.leading, 36)
                    }
                    NavigationLink {
                        TerminalView()
                    } label: {
                        settingsRow("terminal") { Text("Chat console") }
                    }
                    settingsRow("internaldrive") {
                        Toggle("Confirm database upgrades", isOn: $confirmDatabaseUpgrades)
                    }
                    settingsRow("chevron.left.forwardslash.chevron.right") {
                        Toggle("Show developer options", isOn: $developerTools)
                    }
                } footer: {
                    (developerTools ? Text("Show: ") : Text("Hide: ")) + Text("Database IDs and Trasport isolation option.")
                }

                Section {
                    settingsRow("arrow.up.doc") {
                        Toggle("Send files via XFTP", isOn: $xftpSendEnabled)
                            .onChange(of: xftpSendEnabled) { _ in
                                do {
                                    try setXFTPConfig(getXFTPCfg())
                                } catch {
                                    logger.error("setXFTPConfig: cannot set XFTP config \(responseError(error))")
                                }
                            }
                    }
                } header: {
                    Text("Experimental")
                } footer: {
                    if xftpSendEnabled {
                        Text("v4.6.1+ required to receive via XFTP.")
                    }
                }
            }
        }
    }
}

struct DeveloperView_Previews: PreviewProvider {
    static var previews: some View {
        DeveloperView()
    }
}
