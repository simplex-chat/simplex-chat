//
//  CallSettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 27/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CallSettings: View {
    @AppStorage(DEFAULT_WEBRTC_POLICY_RELAY) private var webrtcPolicyRelay = true
    @AppStorage(GROUP_DEFAULT_CALL_KIT_ENABLED) private var callKitEnabled = true
    @AppStorage(DEFAULT_CALL_KIT_CALLS_IN_RECENTS) private var callKitCallsInRecents = false
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    private let allowChangingCallsHistory = false

    var body: some View {
        VStack {
            List {
                Section {
                    Toggle("Connect via relay", isOn: $webrtcPolicyRelay)

                    if !CallController.isInChina && developerTools {
                        Toggle("Use CallKit", isOn: $callKitEnabled)

                        if allowChangingCallsHistory {
                            Toggle("Show calls in phone history", isOn: $callKitCallsInRecents)
                            .disabled(!callKitEnabled)
                            .onChange(of: callKitCallsInRecents) { value in
                                CallController.shared.showInRecents(value)
                            }
                        }
                    }

                    NavigationLink {
                        RTCServers()
                            .navigationTitle("Your ICE servers")
                    } label: {
                        Text("WebRTC ICE servers")
                    }
                } header: {
                    Text("Settings")
                } footer: {
                    if webrtcPolicyRelay {
                        Text("Relay server protects your IP address, but it can observe the duration of the call.")
                    } else {
                        Text("Relay server is only used if necessary. Another party can observe your IP address.")
                    }
                }

                Section("Limitations") {
                    VStack(alignment: .leading, spacing: 8) {
                        textListItem("1.", "Do NOT use SimpleX for emergency calls.")
                        textListItem("2.", "The microphone does not work when the app is in the background.")
                        textListItem("3.", "To prevent the call interruption, enable Do Not Disturb mode.")
                        textListItem("4.", "If the video fails to connect, flip the camera to resolve it.")
                    }
                    .font(.callout)
                    .padding(.vertical, 8)
                }
            }
        }
    }
}

func textListItem(_ n: String, _ text: LocalizedStringKey) -> some View {
    ZStack(alignment: .topLeading) {
        Text(n)
        Text(text).frame(maxWidth: .infinity, alignment: .leading).padding(.leading, 20)
    }
}

struct CallSettings_Previews: PreviewProvider {
    static var previews: some View {
        CallSettings()
    }
}
