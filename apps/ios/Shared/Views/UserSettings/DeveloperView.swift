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
    @State private var appLogLevel = appLogLevelGroupDefault.get()
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
                    settingsRow("text.justify") {
                        Picker("Log level", selection: $appLogLevel) {
                            ForEach(ChatLogLevel.allCases, id: \.self) { ll in
                                Text(ll.rawValue)
                            }
                        }
                        .frame(height: 36)
                        .onChange(of: appLogLevel) { ll in
                            Task {
                                do {
                                    try await apiSetAppLogLevel(ll)
                                    appLogLevelGroupDefault.set(ll)
                                } catch let e {
                                    logger.error("apiSetAppLogLevel error: \(responseError(e))")
                                }
                            }
                        }
                    }
                } header: {
                    Text("")
                } footer: {
                    (developerTools ? Text("Show:") : Text("Hide:")) + Text(" ") + Text("Database IDs and Transport isolation option.")
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
