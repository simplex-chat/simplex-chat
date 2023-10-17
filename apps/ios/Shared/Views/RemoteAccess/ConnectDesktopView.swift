//
//  ConnectDesktopView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 13/10/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ConnectDesktopView: View {
    @AppStorage(DEFAULT_DEVICE_NAME_FOR_REMOTE_ACCESS) private var deviceName = UIDevice.current.name
    @FocusState private var deviceNameFocussed

    var body: some View {
        List {
            Section {
                TextField("Enter this device name…", text: $deviceName)
                    .focused($deviceNameFocussed)
                Button {
                    Task {
                        do {
                            try await startRemoteCtrl()
                            // TODO show connecting state
                        } catch let error {
                            logger.error("startRemoteCtrl \(responseError(error))")
                        }
                    }
                } label: {
                    Label("Connect desktop client", systemImage: "plus")
                }
            } header: {
                Text("This device name")
            } footer: {
                Text("The device name will be shared with the connected desktop client.")
            }

//            Section {
//
//            } header: {
//                CRRemoteCtrlAnnounce
//            }
        }
        .onAppear {
            deviceNameFocussed = true
            setDeviceName(deviceName)
        }
        .onChange(of: deviceName) {
            setDeviceName($0)
        }
    }

    private func setDeviceName(_ name: String) {
        do {
            try setLocalDeviceName(deviceName)
        } catch let error {
            logger.error("setLocalDeviceName \(responseError(error))")
        }
    }
}

#Preview {
    ConnectDesktopView()
}
