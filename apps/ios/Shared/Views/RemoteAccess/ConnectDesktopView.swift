//
//  ConnectDesktopView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 13/10/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ConnectDesktopView: View {
    @AppStorage(DEFAULT_DEVICE_NAME_FOR_REMOTE_ACCESS) private var deviceName = UIDevice.current.name
    @FocusState private var deviceNameFocussed

    var body: some View {
        List {
            Section {
                TextField("Enter this device name…", text: $deviceName)
                    .focused($deviceNameFocussed)
                Button {

                } label: {
                    Label("Connect desktop client", systemImage: "plus")
                }
            } header: {
                Text("This device name")
            } footer: {
                Text("The device name will be shared with the connected desktop client.")
            }
        }
        .onAppear {
            deviceNameFocussed = true
        }
    }
}

#Preview {
    ConnectDesktopView()
}
