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
        List {
            Toggle("Connect via relay", isOn: $webrtcPolicyRelay)
        }
    }
}

struct CallSettings_Previews: PreviewProvider {
    static var previews: some View {
        CallSettings()
    }
}
