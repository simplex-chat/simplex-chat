//
//  GroupDefaults.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

public let appSuspendTimeout: Int = 15 // seconds

public let defaultProfileImageCorner: Double = 22.5

let GROUP_DEFAULT_APP_STATE = "appState"
let GROUP_DEFAULT_NSE_STATE = "nseState"
let GROUP_DEFAULT_SE_STATE = "seState"
let GROUP_DEFAULT_DB_CONTAINER = "dbContainer"
public let GROUP_DEFAULT_CHAT_LAST_START = "chatLastStart"
public let GROUP_DEFAULT_CHAT_LAST_BACKGROUND_RUN = "chatLastBackgroundRun"
let GROUP_DEFAULT_NTF_PREVIEW_MODE = "ntfPreviewMode"
public let GROUP_DEFAULT_NTF_ENABLE_LOCAL = "ntfEnableLocal" // no longer used
public let GROUP_DEFAULT_NTF_ENABLE_PERIODIC = "ntfEnablePeriodic" // no longer used
// replaces DEFAULT_PERFORM_LA
let GROUP_DEFAULT_APP_LOCAL_AUTH_ENABLED = "appLocalAuthEnabled"
public let GROUP_DEFAULT_ALLOW_SHARE_EXTENSION = "allowShareExtension"
// replaces DEFAULT_PRIVACY_LINK_PREVIEWS
let GROUP_DEFAULT_PRIVACY_LINK_PREVIEWS = "privacyLinkPreviews"
// This setting is a main one, while having an unused duplicate from the past: DEFAULT_PRIVACY_ACCEPT_IMAGES
let GROUP_DEFAULT_PRIVACY_ACCEPT_IMAGES = "privacyAcceptImages"
public let GROUP_DEFAULT_PRIVACY_TRANSFER_IMAGES_INLINE = "privacyTransferImagesInline" // no longer used
public let GROUP_DEFAULT_PRIVACY_ENCRYPT_LOCAL_FILES = "privacyEncryptLocalFiles"
public let GROUP_DEFAULT_PRIVACY_ASK_TO_APPROVE_RELAYS = "privacyAskToApproveRelays"
// replaces DEFAULT_PROFILE_IMAGE_CORNER_RADIUS
public let GROUP_DEFAULT_PROFILE_IMAGE_CORNER_RADIUS = "profileImageCornerRadius"
let GROUP_DEFAULT_NTF_BADGE_COUNT = "ntgBadgeCount"
public let GROUP_DEFAULT_NETWORK_SOCKS_PROXY = "networkSocksProxy"
let GROUP_DEFAULT_NETWORK_USE_ONION_HOSTS = "networkUseOnionHosts"
let GROUP_DEFAULT_NETWORK_SESSION_MODE = "networkSessionMode"
let GROUP_DEFAULT_NETWORK_SMP_PROXY_MODE = "networkSMPProxyMode"
let GROUP_DEFAULT_NETWORK_SMP_PROXY_FALLBACK = "networkSMPProxyFallback"
let GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT = "networkTCPConnectTimeout"
let GROUP_DEFAULT_NETWORK_TCP_TIMEOUT = "networkTCPTimeout"
let GROUP_DEFAULT_NETWORK_TCP_TIMEOUT_PER_KB = "networkTCPTimeoutPerKb"
let GROUP_DEFAULT_NETWORK_RCV_CONCURRENCY = "networkRcvConcurrency"
let GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL = "networkSMPPingInterval"
let GROUP_DEFAULT_NETWORK_SMP_PING_COUNT = "networkSMPPingCount"
let GROUP_DEFAULT_NETWORK_ENABLE_KEEP_ALIVE = "networkEnableKeepAlive"
let GROUP_DEFAULT_NETWORK_TCP_KEEP_IDLE = "networkTCPKeepIdle"
let GROUP_DEFAULT_NETWORK_TCP_KEEP_INTVL = "networkTCPKeepIntvl"
let GROUP_DEFAULT_NETWORK_TCP_KEEP_CNT = "networkTCPKeepCnt"
public let GROUP_DEFAULT_INCOGNITO = "incognito"
let GROUP_DEFAULT_STORE_DB_PASSPHRASE = "storeDBPassphrase"
public let GROUP_DEFAULT_INITIAL_RANDOM_DB_PASSPHRASE = "initialRandomDBPassphrase"
public let GROUP_DEFAULT_CONFIRM_DB_UPGRADES = "confirmDBUpgrades"
public let GROUP_DEFAULT_CALL_KIT_ENABLED = "callKitEnabled"
public let GROUP_DEFAULT_PQ_EXPERIMENTAL_ENABLED = "pqExperimentalEnabled" // no longer used
public let GROUP_DEFAULT_ONE_HAND_UI = "oneHandUI"

public let APP_GROUP_NAME = "group.chat.simplex.app"

public let groupDefaults = UserDefaults(suiteName: APP_GROUP_NAME)!

public func registerGroupDefaults() {
    groupDefaults.register(defaults: [
        GROUP_DEFAULT_NTF_ENABLE_LOCAL: false,
        GROUP_DEFAULT_NTF_ENABLE_PERIODIC: false,
        GROUP_DEFAULT_NETWORK_USE_ONION_HOSTS: OnionHosts.no.rawValue,
        GROUP_DEFAULT_NETWORK_SESSION_MODE: TransportSessionMode.session.rawValue,
        GROUP_DEFAULT_NETWORK_SMP_PROXY_MODE: SMPProxyMode.unknown.rawValue,
        GROUP_DEFAULT_NETWORK_SMP_PROXY_FALLBACK: SMPProxyFallback.allowProtected.rawValue,
        GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT: NetCfg.defaults.tcpConnectTimeout,
        GROUP_DEFAULT_NETWORK_TCP_TIMEOUT: NetCfg.defaults.tcpTimeout,
        GROUP_DEFAULT_NETWORK_TCP_TIMEOUT_PER_KB: NetCfg.defaults.tcpTimeoutPerKb,
        GROUP_DEFAULT_NETWORK_RCV_CONCURRENCY: NetCfg.defaults.rcvConcurrency,
        GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL: NetCfg.defaults.smpPingInterval,
        GROUP_DEFAULT_NETWORK_SMP_PING_COUNT: NetCfg.defaults.smpPingCount,
        GROUP_DEFAULT_NETWORK_ENABLE_KEEP_ALIVE: NetCfg.defaults.enableKeepAlive,
        GROUP_DEFAULT_NETWORK_TCP_KEEP_IDLE: KeepAliveOpts.defaults.keepIdle,
        GROUP_DEFAULT_NETWORK_TCP_KEEP_INTVL: KeepAliveOpts.defaults.keepIntvl,
        GROUP_DEFAULT_NETWORK_TCP_KEEP_CNT: KeepAliveOpts.defaults.keepCnt,
        GROUP_DEFAULT_INCOGNITO: false,
        GROUP_DEFAULT_STORE_DB_PASSPHRASE: true,
        GROUP_DEFAULT_INITIAL_RANDOM_DB_PASSPHRASE: false,
        GROUP_DEFAULT_APP_LOCAL_AUTH_ENABLED: true,
        GROUP_DEFAULT_ALLOW_SHARE_EXTENSION: false,
        GROUP_DEFAULT_PRIVACY_LINK_PREVIEWS: true,
        GROUP_DEFAULT_PRIVACY_ACCEPT_IMAGES: true,
        GROUP_DEFAULT_PRIVACY_TRANSFER_IMAGES_INLINE: false,
        GROUP_DEFAULT_PRIVACY_ENCRYPT_LOCAL_FILES: true,
        GROUP_DEFAULT_PRIVACY_ASK_TO_APPROVE_RELAYS: true,
        GROUP_DEFAULT_PROFILE_IMAGE_CORNER_RADIUS: defaultProfileImageCorner,
        GROUP_DEFAULT_CONFIRM_DB_UPGRADES: false,
        GROUP_DEFAULT_CALL_KIT_ENABLED: true,
        GROUP_DEFAULT_PQ_EXPERIMENTAL_ENABLED: false,
        GROUP_DEFAULT_ONE_HAND_UI: true
    ])
}

public enum AppState: String, Codable {
    case active
    case activating
    case bgRefresh
    case suspending
    case suspended
    case stopped

    public var running: Bool {
        switch self {
        case .active: return true
        case .activating: return true
        case .bgRefresh: return true
        default: return false
        }
    }

    public var inactive: Bool {
        switch self {
        case .suspending: return true
        case .suspended: return true
        default: return false
        }
    }

    public var canSuspend: Bool {
        switch self {
        case .active: return true
        case .activating: return true
        case .bgRefresh: return true
        default: return false
        }
    }
}

public enum NSEState: String, Codable {
    case created
    case starting
    case active
    case suspending
    case suspended

    public var inactive: Bool {
        switch self {
        case .created: true
        case .suspended: true
        default: false
        }
    }

    public var canSuspend: Bool {
        if case .active = self { true } else { false }
    }
}

public enum SEState: String, Codable {
    case inactive
    case sendingMessage
}

public enum DBContainer: String {
    case documents
    case group
}

// appStateGroupDefault must not be used in the app directly, only via AppChatState singleton
public let appStateGroupDefault = EnumDefault<AppState>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_APP_STATE,
    withDefault: .active
)

// nseStateGroupDefault must not be used in NSE directly, only via NSEChatState singleton
public let nseStateGroupDefault = EnumDefault<NSEState>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NSE_STATE,
    withDefault: .suspended // so that NSE that was never launched does not delay the app from resuming
)

public let seStateGroupDefault = EnumDefault<SEState>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_SE_STATE,
    withDefault: .inactive
)

// inactive app states do not include "stopped" state
public func allowBackgroundRefresh() -> Bool {
    appStateGroupDefault.get().inactive && nseStateGroupDefault.get().inactive
}

public let dbContainerGroupDefault = EnumDefault<DBContainer>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_DB_CONTAINER,
    withDefault: .documents
)

public let chatLastStartGroupDefault = DateDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_CHAT_LAST_START)

public let chatLastBackgroundRunGroupDefault = DateDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_CHAT_LAST_BACKGROUND_RUN)

public let ntfPreviewModeGroupDefault = EnumDefault<NotificationPreviewMode>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NTF_PREVIEW_MODE,
    withDefault: .message
)

public let incognitoGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_INCOGNITO)

public let appLocalAuthEnabledGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_APP_LOCAL_AUTH_ENABLED)

public let allowShareExtensionGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_ALLOW_SHARE_EXTENSION)

public let privacyLinkPreviewsGroupDefault =  BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_PRIVACY_LINK_PREVIEWS)

// This setting is a main one, while having an unused duplicate from the past: DEFAULT_PRIVACY_ACCEPT_IMAGES
public let privacyAcceptImagesGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_PRIVACY_ACCEPT_IMAGES)

public let privacyEncryptLocalFilesGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_PRIVACY_ENCRYPT_LOCAL_FILES)

public let privacyAskToApproveRelaysGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_PRIVACY_ASK_TO_APPROVE_RELAYS)

public let profileImageCornerRadiusGroupDefault = Default<Double>(defaults: groupDefaults, forKey: GROUP_DEFAULT_PROFILE_IMAGE_CORNER_RADIUS)

public let ntfBadgeCountGroupDefault = IntDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_NTF_BADGE_COUNT)

public let networkUseOnionHostsGroupDefault = EnumDefault<OnionHosts>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NETWORK_USE_ONION_HOSTS,
    withDefault: .no
)

public let networkSessionModeGroupDefault = EnumDefault<TransportSessionMode>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NETWORK_SESSION_MODE,
    withDefault: .session
)

public let networkSMPProxyModeGroupDefault = EnumDefault<SMPProxyMode>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NETWORK_SMP_PROXY_MODE,
    withDefault: .unknown
)

public let networkSMPProxyFallbackGroupDefault = EnumDefault<SMPProxyFallback>(
    defaults: groupDefaults,
    forKey: GROUP_DEFAULT_NETWORK_SMP_PROXY_FALLBACK,
    withDefault: .allowProtected
)

public let storeDBPassphraseGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_STORE_DB_PASSPHRASE)

public let initialRandomDBPassphraseGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_INITIAL_RANDOM_DB_PASSPHRASE)

public let confirmDBUpgradesGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_CONFIRM_DB_UPGRADES)

public let callKitEnabledGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_CALL_KIT_ENABLED)

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
    let socksProxy = groupDefaults.string(forKey: GROUP_DEFAULT_NETWORK_SOCKS_PROXY)
    let onionHosts = networkUseOnionHostsGroupDefault.get()
    let (hostMode, requiredHostMode) = onionHosts.hostMode
    let sessionMode = networkSessionModeGroupDefault.get()
    let smpProxyMode = networkSMPProxyModeGroupDefault.get()
    let smpProxyFallback = networkSMPProxyFallbackGroupDefault.get()
    let tcpConnectTimeout = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT)
    let tcpTimeout = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_TIMEOUT)
    let tcpTimeoutPerKb = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_TCP_TIMEOUT_PER_KB)
    let rcvConcurrency = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_RCV_CONCURRENCY)
    let smpPingInterval = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL)
    let smpPingCount = groupDefaults.integer(forKey: GROUP_DEFAULT_NETWORK_SMP_PING_COUNT)
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
        socksProxy: socksProxy,
        hostMode: hostMode,
        requiredHostMode: requiredHostMode,
        sessionMode: sessionMode,
        smpProxyMode: smpProxyMode,
        smpProxyFallback: smpProxyFallback,
        smpWebPort: false,
        tcpConnectTimeout: tcpConnectTimeout,
        tcpTimeout: tcpTimeout,
        tcpTimeoutPerKb: tcpTimeoutPerKb,
        rcvConcurrency: rcvConcurrency,
        tcpKeepAlive: tcpKeepAlive,
        smpPingInterval: smpPingInterval,
        smpPingCount: smpPingCount,
        logTLSErrors: false
    )
}

public func setNetCfg(_ cfg: NetCfg, networkProxy: NetworkProxy?) {
    networkUseOnionHostsGroupDefault.set(OnionHosts(netCfg: cfg))
    networkSessionModeGroupDefault.set(cfg.sessionMode)
    networkSMPProxyModeGroupDefault.set(cfg.smpProxyMode)
    networkSMPProxyFallbackGroupDefault.set(cfg.smpProxyFallback)
    let socksProxy = networkProxy?.toProxyString()
    groupDefaults.set(socksProxy, forKey: GROUP_DEFAULT_NETWORK_SOCKS_PROXY)
    groupDefaults.set(cfg.tcpConnectTimeout, forKey: GROUP_DEFAULT_NETWORK_TCP_CONNECT_TIMEOUT)
    groupDefaults.set(cfg.tcpTimeout, forKey: GROUP_DEFAULT_NETWORK_TCP_TIMEOUT)
    groupDefaults.set(cfg.tcpTimeoutPerKb, forKey: GROUP_DEFAULT_NETWORK_TCP_TIMEOUT_PER_KB)
    groupDefaults.set(cfg.rcvConcurrency, forKey: GROUP_DEFAULT_NETWORK_RCV_CONCURRENCY)
    groupDefaults.set(cfg.smpPingInterval, forKey: GROUP_DEFAULT_NETWORK_SMP_PING_INTERVAL)
    groupDefaults.set(cfg.smpPingCount, forKey: GROUP_DEFAULT_NETWORK_SMP_PING_COUNT)
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
