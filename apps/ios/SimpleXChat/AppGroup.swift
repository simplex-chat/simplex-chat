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

let APP_GROUP_NAME = "group.chat.simplex.app"

public let NSE_MACH_PORT = "\(APP_GROUP_NAME).nse" as CFString

public let APP_MACH_PORT = "\(APP_GROUP_NAME).app" as CFString

public let FPS_MACH_PORT = "\(APP_GROUP_NAME).fps" as CFString

func getGroupDefaults() -> UserDefaults? {
    UserDefaults(suiteName: APP_GROUP_NAME)
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

let MACH_SEND_TIMEOUT: CFTimeInterval = 1.0
let MACH_REPLY_TIMEOUT: CFTimeInterval = 1.0

public class MachMessenger {
    public init(_ localPortName: CFString, callback: @escaping Callback) {
        self.localPortName = localPortName
        self.callback = callback
        self.localPort = nil
    }

    var localPortName: CFString
    var callback: Callback
    var localPort: CFMessagePort?

    public enum SendError: Error {
        case sndTimeout
        case rcvTimeout
        case portInvalid
        case portBecameInvalid
        case sendError(Int32)
        case msgError
    }

    public typealias Callback = (_ msgId: Int32, _ msg: String) -> String?

    class CallbackInfo {
        internal init(callback: @escaping MachMessenger.Callback) {
            self.callback = callback
        }

        var callback: Callback
    }

    public static func sendError(_ code: Int32) -> SendError? {
        switch code {
        case kCFMessagePortSuccess: return nil
        case kCFMessagePortSendTimeout:  return .sndTimeout
        case kCFMessagePortReceiveTimeout: return .rcvTimeout
        case kCFMessagePortIsInvalid: return .portInvalid
        case kCFMessagePortBecameInvalidError: return .portBecameInvalid
        default: return .sendError(code)
        }
    }

    public func start() {
        logger.debug("MachMessenger.start")
        localPort = createLocalPort(localPortName, callback: callback)
    }

    public func stop() {
        if let port = localPort {
            logger.debug("MachMessenger.stop")
            CFMessagePortInvalidate(port)
            localPort = nil
        }
    }

    public func sendMessage(_ remotePortName: CFString, msgId: Int32 = 0, msg: String) -> SendError? {
        logger.debug("MachMessenger.sendMessage")
        if let port = createRemotePort(remotePortName) {
            logger.debug("MachMessenger.sendMessage: sending...")
            return sendMessage(port, msgId: msgId, msg: msg)
        } else {
            logger.debug("MachMessenger.sendMessage: no remote port")
            return .portInvalid
        }
    }

    public func sendMessage(_ remotePortName: CFString, msgId: Int32 = 0, data: Data) -> SendError? {
        logger.debug("MachMessenger.sendMessage")
        if let port = createRemotePort(remotePortName) {
            logger.debug("MachMessenger.sendMessage: sending...")
            return sendMessage(port, msgId: msgId, data: data)
        } else {
            logger.debug("MachMessenger.sendMessage: no remote port")
            return .portInvalid
        }
    }

    public func sendMessageWithReply(_ remotePortName: CFString, msgId: Int32 = 0, msg: String) -> Result<String?, SendError> {
        logger.debug("MachMessenger.sendMessageWithReply")
        if let port = createRemotePort(remotePortName) {
            logger.debug("MachMessenger.sendMessageWithReply: sending...")
            return sendMessageWithReply(port, msgId: msgId, msg: msg)
        } else {
            logger.debug("MachMessenger.sendMessageWithReply: no remote port")
            return .failure(.portInvalid)
        }
    }

    private func createLocalPort(_ portName: CFString, callback: @escaping Callback) -> CFMessagePort? {
        logger.debug("MachMessenger.createLocalPort")
        if let port = localPort { return port }
        logger.debug("MachMessenger.createLocalPort: creating...")
        var context = CFMessagePortContext()
        context.version = 0
        context.info = Unmanaged.passRetained(CallbackInfo(callback: callback)).toOpaque()
        let callout: CFMessagePortCallBack = { port, msgId, msgData, info in
            logger.debug("MachMessenger CFMessagePortCallBack called")
            if let data = msgData,
               let msg = String(data: data as Data, encoding: .utf8),
               let info = info,
               let resp = Unmanaged<CallbackInfo>.fromOpaque(info).takeUnretainedValue().callback(msgId, msg),
               let respData = resp.data(using: .utf8) {
                return Unmanaged.passRetained(respData as CFData)
            }
            return nil
        }
        return withUnsafeMutablePointer(to: &context) { cxt in
            let port = CFMessagePortCreateLocal(kCFAllocatorDefault, portName, callout, cxt, nil)
            CFMessagePortSetDispatchQueue(port, DispatchQueue.main);
            localPort = port
            logger.debug("MachMessenger.createLocalPort created: \(portName)")
            return port
        }
    }

    private func createRemotePort(_ portName: CFString) -> CFMessagePort? {
        CFMessagePortCreateRemote(kCFAllocatorDefault, portName)
    }

    private func sendMessage(_ remotePort: CFMessagePort, msgId: Int32 = 0, msg: String) -> SendError? {
        if let data = msg.data(using: .utf8) {
            logger.debug("MachMessenger sendMessage")
            return sendMessage(remotePort, msgId: msgId, data: data)
        }
        return .msgError
    }

    private func sendMessage(_ remotePort: CFMessagePort, msgId: Int32 = 0, data: Data) -> SendError? {
        let code = CFMessagePortSendRequest(remotePort, msgId, data as CFData, MACH_SEND_TIMEOUT, 0, nil, nil)
        logger.debug("MachMessenger sendMessage \(code)")
        return MachMessenger.sendError(code)
    }

    private func sendMessageWithReply(_ remotePort: CFMessagePort, msgId: Int32 = 0, msg: String) -> Result<String?, SendError> {
        if let data = msg.data(using: .utf8) {
            let msgData = data as CFData
            var respData: Unmanaged<CFData>? = nil
            let code = CFMessagePortSendRequest(remotePort, msgId, msgData, MACH_SEND_TIMEOUT, MACH_REPLY_TIMEOUT, CFRunLoopMode.defaultMode.rawValue, &respData)
            if let err = MachMessenger.sendError(code) {
                return .failure(err)
            } else if let data = respData?.takeUnretainedValue(),
                      let resp = String(data: data as Data, encoding: .utf8) {
                return .success(resp)
            } else {
                return .success(nil)
            }

        }
        return .failure(.msgError)
    }
}
