//
//  PrivacySettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 29/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct PrivacySettings: View {
    @AppStorage(DEFAULT_PRIVACY_ACCEPT_IMAGES) private var autoAcceptImages = true
    @AppStorage(DEFAULT_PRIVACY_LINK_PREVIEWS) private var useLinkPreviews = true

    var body: some View {
        VStack {
            List {
                Section("Device") {
                    SimplexLockSetting()
                }
                Section("Chats") {
                    settingsRow("photo") {
                        Toggle("Auto-accept images", isOn: $autoAcceptImages)
                    }
                    settingsRow("network") {
                        Toggle("Send link previews", isOn: $useLinkPreviews)
                    }
                }
            }
        }
    }
}

struct SimplexLockSetting: View {
    @AppStorage(DEFAULT_LA_NOTICE_SHOWN) private var prefLANoticeShown = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State var performLA: Bool = UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    @State private var performLAToggleReset = false
    @State var laAlert: laSettingViewAlert? = nil

    enum laSettingViewAlert: Identifiable {
        case laTurnedOnAlert
        case laFailedAlert
        case laUnavailableInstructionAlert
        case laUnavailableTurningOffAlert

        var id: laSettingViewAlert { get { self } }
    }

    var body: some View {
        settingsRow("lock") {
            Toggle("SimpleX Lock", isOn: $performLA)
        }
        .onChange(of: performLA) { performLAToggle in
            prefLANoticeShown = true
            if performLAToggleReset {
                performLAToggleReset = false
            } else {
                if performLAToggle {
                    enableLA()
                } else {
                    disableLA()
                }
            }
        }
        .alert(item: $laAlert) { alertItem in
            switch alertItem {
            case .laTurnedOnAlert: return laTurnedOnAlert()
            case .laFailedAlert: return laFailedAlert()
            case .laUnavailableInstructionAlert: return laUnavailableInstructionAlert()
            case .laUnavailableTurningOffAlert: return laUnavailableTurningOffAlert()
            }
        }

    }

    private func enableLA() {
        authenticate(reason: NSLocalizedString("Enable SimpleX Lock", comment: "authentication reason")) { laResult in
            switch laResult {
            case .success:
                prefPerformLA = true
                laAlert = .laTurnedOnAlert
            case .failed:
                prefPerformLA = false
                withAnimation() {
                    performLA = false
                }
                performLAToggleReset = true
                laAlert = .laFailedAlert
            case .unavailable:
                prefPerformLA = false
                withAnimation() {
                    performLA = false
                }
                performLAToggleReset = true
                laAlert = .laUnavailableInstructionAlert
            }
        }
    }

    private func disableLA() {
        authenticate(reason: NSLocalizedString("Disable SimpleX Lock", comment: "authentication reason")) { laResult in
            switch (laResult) {
            case .success:
                prefPerformLA = false
            case .failed:
                prefPerformLA = true
                withAnimation() {
                    performLA = true
                }
                performLAToggleReset = true
                laAlert = .laFailedAlert
            case .unavailable:
                prefPerformLA = false
                laAlert = .laUnavailableTurningOffAlert
            }
        }
    }
}

struct PrivacySettings_Previews: PreviewProvider {
    static var previews: some View {
        PrivacySettings()
    }
}
