//
//  CallSettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 27/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct CallSettings: View {
    @AppStorage(DEFAULT_WEBRTC_POLICY_RELAY) private var webrtcPolicyRelay = true

    var body: some View {
        VStack {
            List {
                Section("Settings") {
                    Toggle("Connect via relay", isOn: $webrtcPolicyRelay)
                }

                Section("Limitations") {
                    VStack(alignment: .leading, spacing: 8) {
                        textListItem("1.", "Do NOT use SimpleX for emergency calls.")
                        textListItem("2.", "Pre-arrange the calls, as notifications arrive with a delay (we are improving it).")
                        textListItem("3.", "The microphone does not work when the app is in the background.")
                        textListItem("4.", "To prevent the call interruption, enable Do Not Disturb mode.")
                        textListItem("5.", "If the video fails to connect, flip the camera to resolve it.")
                    }
                    .font(.callout)
                    .padding(.vertical, 8)
                }
            }
        }
    }

    private func textListItem(_ n: String, _ text: LocalizedStringKey) -> some View {
        ZStack(alignment: .topLeading) {
            Text(n)
            Text(text).frame(maxWidth: .infinity, alignment: .leading).padding(.leading, 20)
        }
    }
}

struct CallSettings_Previews: PreviewProvider {
    static var previews: some View {
        CallSettings()
    }
}
