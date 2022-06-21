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
let GROUP_DEFAULT_DB_CONTAINER = "dbContainer"
public let GROUP_DEFAULT_CHAT_LAST_START = "chatLastStart"

let APP_GROUP_NAME = "group.chat.simplex.app"

public let groupDefaults = UserDefaults(suiteName: APP_GROUP_NAME)!

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

public enum DBContainer: String {
    case documents
    case group
}

public let appStateGroupDefault = EnumDefault<AppState>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_APP_STATE,
    withDefault: .active
)

public let dbContainerGroupDefault = EnumDefault<DBContainer>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_DB_CONTAINER,
    withDefault: .documents
)

public class EnumDefault<T: RawRepresentable> where T.RawValue == String {
    var defaults: UserDefaults
    var key: String
    var defaultValue: T

    public init(defaults: UserDefaults = UserDefaults.standard, forKey: String, withDefault: T) {
        self.defaults = defaults
        self.key = forKey
        self.defaultValue = withDefault
    }

    public func get() -> T {
        if let rawValue = groupDefaults.string(forKey: key),
           let value = T(rawValue: rawValue) {
            return value
        }
        return defaultValue
    }

    public func set(_ value: T) {
        groupDefaults.set(value.rawValue, forKey: key)
        groupDefaults.synchronize()
    }
}
