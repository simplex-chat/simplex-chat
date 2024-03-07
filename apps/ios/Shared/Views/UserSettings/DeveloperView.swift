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
    @AppStorage(GROUP_DEFAULT_PQ_EXPERIMENTAL_ENABLED, store: groupDefaults) private var pqExperimentalEnabled = false
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
                } header: {
                    Text("")
                } footer: {
                    (developerTools ? Text("Show:") : Text("Hide:")) + Text(" ") + Text("Database IDs and Transport isolation option.")
                }

                if developerTools {
                    Section {
                        settingsRow("key") {
                            Toggle("Post-quantum E2EE", isOn: $pqExperimentalEnabled)
                                .onChange(of: pqExperimentalEnabled) {
                                    setPQExperimentalEnabled($0)
                                }
                        }
                    } header: {
                        Text(String("Experimental"))
                    } footer: {
                        Text(String("In this version applies only to new contacts."))
                    }
                }
            }
        }
    }

    private func setPQExperimentalEnabled(_ enable: Bool) {
        do {
            try apiSetPQEnabled(enable)
        } catch let error {
            let err = responseError(error)
            logger.error("apiSetPQEnabled \(err)")
        }
    }
}

struct DeveloperView_Previews: PreviewProvider {
    static var previews: some View {
        DeveloperView()
    }
}
