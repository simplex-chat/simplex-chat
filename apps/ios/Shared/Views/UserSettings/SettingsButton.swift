//
//  SettingsButton.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SettingsButton: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var showSettings = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false

    var body: some View {
        Button { showSettings = true } label: {
            Image(systemName: "gearshape")
        }
        .sheet(isPresented: $showSettings, content: {
            SettingsView(showSettings: $showSettings, performLA: prefPerformLA)
        })
    }
}

struct SettingsButton_Previews: PreviewProvider {
    static var previews: some View {
        SettingsButton()
    }
}
