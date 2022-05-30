//
//  GroupDefaults.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

let GROUP_DEFAULT_APP_IN_BACKGROUND = "appInBackground"

func getGroupDefaults() -> UserDefaults? {
    UserDefaults(suiteName: "group.chat.simplex.app")
}

public func setAppState(_ phase: ScenePhase) {
    if let defaults = getGroupDefaults() {
        defaults.set(phase == .background, forKey: GROUP_DEFAULT_APP_IN_BACKGROUND)
        defaults.synchronize()
    }
}

public func getAppState() -> ScenePhase {
    if let defaults = getGroupDefaults() {
        if defaults.bool(forKey: GROUP_DEFAULT_APP_IN_BACKGROUND) {
            return .background
        }
    }
    return .active
}
