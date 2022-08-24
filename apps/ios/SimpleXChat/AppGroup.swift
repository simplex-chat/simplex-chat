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
let GROUP_DEFAULT_PRIVACY_ACCEPT_IMAGES = "privacyAcceptImages"
let GROUP_DEFAULT_NTF_BADGE_COUNT = "ntgBadgeCount"
let GROUP_DEFAULT_NETWORK_USE_ONION_HOSTS = "networkUseOnionHosts"
let GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT = "networkTCPConnectTimeout"
let GROUP_DEFAULT_NETWORK_TCP_TIMEOUT = "networkTCPTimeout"
let GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL = "networkSMPPingInterval"
let GROUP_DEFAULT_NETWORK_ENABLE_KEEP_ALIVE = "networkEnableKeepAlive"
let GROUP_DEFAULT_NETWORK_TCP_KEEP_IDLE = "networkTCPKeepIdle"
let GROUP_DEFAULT_NETWORK_TCP_KEEP_INTVL = "networkTCPKeepIntvl"
let GROUP_DEFAULT_NETWORK_TCP_KEEP_CNT = "networkTCPKeepCnt"
let GROUP_DEFAULT_INCOGNITO = "incognito"

let APP_GROUP_NAME = "group.chat.simplex.app"

public let groupDefaults = UserDefaults(suiteName: APP_GROUP_NAME)!

public func registerGroupDefaults() {
    groupDefaults.register(defaults: [
        GROUP_DEFAULT_NETWORK_USE_ONION_HOSTS: OnionHosts.no.rawValue,
        GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT: NetCfg.defaults.tcpConnectTimeout,
        GROUP_DEFAULT_NETWORK_TCP_TIMEOUT: NetCfg.defaults.tcpTimeout,
        GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL: NetCfg.defaults.smpPingInterval,
        GROUP_DEFAULT_NETWORK_ENABLE_KEEP_ALIVE: NetCfg.defaults.enableKeepAlive,
        GROUP_DEFAULT_NETWORK_TCP_KEEP_IDLE: KeepAliveOpts.defaults.keepIdle,
        GROUP_DEFAULT_NETWORK_TCP_KEEP_INTVL: KeepAliveOpts.defaults.keepIntvl,
        GROUP_DEFAULT_NETWORK_TCP_KEEP_CNT: KeepAliveOpts.defaults.keepCnt,
        GROUP_DEFAULT_INCOGNITO: false
    ])
}

public enum AppState: String {
    case active
    case bgRefresh
    case suspending
    case suspended
    case stopped

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

public let incognitoGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_INCOGNITO)

public let privacyAcceptImagesGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_PRIVACY_ACCEPT_IMAGES)

public let ntfBadgeCountGroupDefault = IntDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_NTF_BADGE_COUNT)

public let networkUseOnionHostsGroupDefault = EnumDefault<OnionHosts>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NETWORK_USE_ONION_HOSTS,
    withDefault: .no
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

public class BoolDefault: Default<Bool> {
    public func get() -> Bool {
        self.defaults.bool(forKey: self.key)
    }
}

public class IntDefault: Default<Int> {
    public func get() -> Int {
        self.defaults.integer(forKey: self.key)
    }
}

public class Default<T> {
    var defaults: UserDefaults
    var key: String

    public init(defaults: UserDefaults = UserDefaults.standard, forKey: String) {
        self.defaults = defaults
        self.key = forKey
    }

    public func set(_ value: T) {
        defaults.set(value, forKey: key)
        defaults.synchronize()
    }
}

public func getNetCfg() -> NetCfg {
    let onionHosts = networkUseOnionHostsGroupDefault.get()
    let (hostMode, requiredHostMode) = onionHosts.hostMode
    let tcpConnectTimeout = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT)
    let tcpTimeout = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_TIMEOUT)
    let smpPingInterval = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL)
    let enableKeepAlive = groupDefaults.bool(forKey: GROUP_DEFAULT_NETWORK_ENABLE_KEEP_ALIVE)
    var tcpKeepAlive: KeepAliveOpts?
    if enableKeepAlive {
        let keepIdle = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_KEEP_IDLE)
        let keepIntvl = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_KEEP_INTVL)
        let keepCnt = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_KEEP_CNT)
        tcpKeepAlive = KeepAliveOpts(keepIdle: keepIdle, keepIntvl: keepIntvl, keepCnt: keepCnt)
    } else {
        tcpKeepAlive = nil
    }
    return NetCfg(
        hostMode: hostMode,
        requiredHostMode: requiredHostMode,
        tcpConnectTimeout: tcpConnectTimeout,
        tcpTimeout: tcpTimeout,
        tcpKeepAlive: tcpKeepAlive,
        smpPingInterval: smpPingInterval
    )
}

public func setNetCfg(_ cfg: NetCfg) {
    networkUseOnionHostsGroupDefault.set(OnionHosts(netCfg: cfg))
    groupDefaults.set(cfg.tcpConnectTimeout, forKey: GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT)
    groupDefaults.set(cfg.tcpTimeout, forKey: GROUP_DEFAULT_NETWORK_TCP_TIMEOUT)
    groupDefaults.set(cfg.smpPingInterval, forKey: GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL)
    if let tcpKeepAlive = cfg.tcpKeepAlive {
        groupDefaults.set(true, forKey: GROUP_DEFAULT_NETWORK_ENABLE_KEEP_ALIVE)
        groupDefaults.set(tcpKeepAlive.keepIdle, forKey: GROUP_DEFAULT_NETWORK_TCP_KEEP_IDLE)
        groupDefaults.set(tcpKeepAlive.keepIntvl, forKey: GROUP_DEFAULT_NETWORK_TCP_KEEP_INTVL)
        groupDefaults.set(tcpKeepAlive.keepCnt, forKey: GROUP_DEFAULT_NETWORK_TCP_KEEP_CNT)
    } else {
        groupDefaults.set(false, forKey: GROUP_DEFAULT_NETWORK_ENABLE_KEEP_ALIVE)
    }
    groupDefaults.synchronize()
}
