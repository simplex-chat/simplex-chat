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
    @AppStorage(DEFAULT_ONE_HAND_UI) private var oneHandUI = false
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
                        settingsRow("internaldrive") {
                            Toggle("Confirm database upgrades", isOn: $confirmDatabaseUpgrades)
                        }
                        settingsRow("hand.wave") {
                            Toggle("One-hand UI", isOn: $oneHandUI)
                        }
                    } header: {
                        Text("Developer options")
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
