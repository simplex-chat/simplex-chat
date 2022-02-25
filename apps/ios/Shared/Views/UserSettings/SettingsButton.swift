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

    var body: some View {
        Button { showSettings = true } label: {
            Image(systemName: "gearshape")
        }
        .sheet(isPresented: $showSettings, content: {
            SettingsView(showSettings: $showSettings)
                .onAppear {
                    Task {
                        do {
                            let userAddress = try await apiGetUserAddress()
                            DispatchQueue.main.async {
                                chatModel.userAddress = userAddress
                            }
                        } catch {
                            logger.error("SettingsButton apiGetUserAddress error: \(error.localizedDescription)")
                        }
                    }
                }
        })
    }
}

struct SettingsButton_Previews: PreviewProvider {
    static var previews: some View {
        SettingsButton()
    }
}
