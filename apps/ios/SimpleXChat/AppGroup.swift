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
let GROUP_DEFAULT_NTF_PREVIEW_MODE = "ntfPreviewMode"

let APP_GROUP_NAME = "group.chat.simplex.app"

public let groupDefaults = UserDefaults(suiteName: APP_GROUP_NAME)!

public enum AppState: String {
    case active
    case bgRefresh
    case suspending
    case suspended

    public var inactive: Bool {
        switch self {
        case .suspending: return true
        case .suspended: return true
        default: return false
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

public let chatLastStartGroupDefault = DateDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_CHAT_LAST_START)

public let ntfPreviewModeGroupDefault = EnumDefault<NotificationPreviewMode>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NTF_PREVIEW_MODE,
    withDefault: .message
)

public class DateDefault {
    var defaults: UserDefaults
    var key: String

    public init(defaults: UserDefaults = UserDefaults.standard, forKey: String) {
        self.defaults = defaults
        self.key = forKey
    }

    public func get() -> Date {
        let ts = defaults.double(forKey: key)
        return Date(timeIntervalSince1970: ts)
    }

    public func set(_ ts: Date) {
        defaults.set(ts.timeIntervalSince1970, forKey: key)
        defaults.synchronize()
    }
}

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
        if let rawValue = defaults.string(forKey: key),
           let value = T(rawValue: rawValue) {
            return value
        }
        return defaultValue
    }

    public func set(_ value: T) {
        defaults.set(value.rawValue, forKey: key)
        defaults.synchronize()
    }
}
