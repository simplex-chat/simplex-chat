//
//  ExperimentalFeaturesView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 30/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ExperimentalFeaturesView: View {
    @AppStorage(GROUP_DEFAULT_XFTP_SEND_ENABLED, store: UserDefaults(suiteName: APP_GROUP_NAME)!) private var xftpSendEnabled = false

    var body: some View {
        List {
            Section("") {
                settingsRow("arrow.up.doc") {
                    Toggle("Send files via XFTP", isOn: $xftpSendEnabled)
                        .onChange(of: xftpSendEnabled) { _ in
                            do {
                                try setXFTPConfig(getXFTPCfg())
                            } catch {
                                logger.error("setXFTPConfig: cannot set XFTP config \(responseError(error))")
                            }
                        }
                }
            }
        }
    }
}

struct ExperimentalFeaturesView_Previews: PreviewProvider {
    static var previews: some View {
        ExperimentalFeaturesView()
    }
}
