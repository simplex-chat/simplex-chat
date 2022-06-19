//
//  GroupDefaults.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

let GROUP_DEFAULT_APP_STATE = "appState"

let APP_GROUP_NAME = "group.chat.simplex.app"

func getGroupDefaults() -> UserDefaults? {
    UserDefaults(suiteName: APP_GROUP_NAME)
}

public enum AppState: String {
    case active
    case pausing
    case paused
    case suspending
    case suspended

    public init(appPhase: AgentPhase) {
        switch appPhase {
        case .active: self = .active
        case .paused: self = .paused
        case .suspended: self = .suspended
        }
    }

    public var running: Bool {
        switch self {
        case .paused: return false
        case .suspending: return false
        case .suspended: return false
        default: return true
        }
    }
}

public func setAppState(_ state: AppState) {
    if let defaults = getGroupDefaults() {
        defaults.set(state.rawValue, forKey: GROUP_DEFAULT_APP_STATE)
        defaults.synchronize()
    }
}

public func getAppState() -> AppState {
    if let defaults = getGroupDefaults(),
       let rawValue = defaults.string(forKey: GROUP_DEFAULT_APP_STATE),
       let state = AppState(rawValue: rawValue) {
        return state
    }
    return .active
}
