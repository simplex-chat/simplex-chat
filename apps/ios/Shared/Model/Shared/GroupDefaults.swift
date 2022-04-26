//
//  GroupDefaults.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

func getGroupDefaults() -> UserDefaults? {
    UserDefaults(suiteName: "group.chat.simplex.app")
}

func setAppState(_ phase: ScenePhase) {
    if let defaults = getGroupDefaults() {
        defaults.set(phase == .background, forKey: "appInBackground")
        defaults.synchronize()
    }
}

func getAppState() -> ScenePhase {
    if let defaults = getGroupDefaults() {
        if defaults.bool(forKey: "appInBackground") {
            return .background
        }
    }
    return .active
}
