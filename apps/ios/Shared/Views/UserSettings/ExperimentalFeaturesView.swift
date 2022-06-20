//
//  ExperimentalFeaturesView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 30/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ExperimentalFeaturesView: View {
    @AppStorage(DEFAULT_EXPERIMENTAL_CALLS) private var enableCalls = false

    var body: some View {
        List {
            Section("") {
                settingsRow("video") {
                    Toggle("Audio & video calls", isOn: $enableCalls)
                }
            }
            NavigationLink {
                DatabaseView()
                    .navigationTitle("Your chat database")
            } label: {
                settingsRow("internaldrive") { Text("Your chat database") }
            }
        }
    }
}

struct ExperimentalFeaturesView_Previews: PreviewProvider {
    static var previews: some View {
        ExperimentalFeaturesView()
    }
}
