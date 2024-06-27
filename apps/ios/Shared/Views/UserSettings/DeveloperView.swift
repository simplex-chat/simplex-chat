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
    @EnvironmentObject var theme: AppTheme
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @AppStorage(GROUP_DEFAULT_CONFIRM_DB_UPGRADES, store: groupDefaults) private var confirmDatabaseUpgrades = false
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
                            .colorMultiply(theme.colors.secondary)
                        Text("Install [SimpleX Chat for terminal](https://github.com/simplex-chat/simplex-chat)")
                            .padding(.leading, 36)
                    }
                    NavigationLink {
                        TerminalView()
                    } label: {
                        settingsRow("terminal", color: theme.colors.secondary) { Text("Chat console") }
                    }
                    settingsRow("internaldrive", color: theme.colors.secondary) {
                        Toggle("Confirm database upgrades", isOn: $confirmDatabaseUpgrades)
                    }
                    settingsRow("chevron.left.forwardslash.chevron.right", color: theme.colors.secondary) {
                        Toggle("Show developer options", isOn: $developerTools)
                    }
                } header: {
                    Text("")
                } footer: {
                    ((developerTools ? Text("Show:") : Text("Hide:")) + Text(" ") + Text("Database IDs and Transport isolation option."))
                        .foregroundColor(theme.colors.secondary)
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
