//
//  ServiceAPI.swift
//  SimpleXAppShared
//
//  Created by Evgeny on 09/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXServiceProtocol
import SimpleXChatSDK

private var serviceProxy: SimpleXServiceProtocol?

public func sendSimpleXCmd(_ cmd: ChatCommand) async -> ChatResponse {
    let proxy = await getServiceProxy()
    let resp = await proxy.chatSendCmd(cmd.cmdString)
    return chatResponse(resp)
}

public func recvSimpleXMsg() async -> ChatResponse {
    let proxy = await getServiceProxy()
    let msg = await proxy.chatRecvMsg()
    return chatResponse(msg)
}

public func getServiceProxy() async -> SimpleXServiceProtocol {
    if let proxy = serviceProxy { return proxy }
    var err: Error?
    for i in 1...20 {
        do {
            let proxy = try await _getServiceProxy()
            serviceProxy = proxy
            logger.debug("getServiceProxy \(i): success")
            return proxy
        } catch let error {
            err = error
            logger.debug("getServiceProxy \(i): \(String(describing: error), privacy: .public)")
            try! await Task.sleep(nanoseconds: 250_000)
        }
    }
    fatalError("getServiceProxy: error \(String(describing: err))")
}

private func _getServiceProxy() async throws -> SimpleXServiceProtocol {
    let services = try await FileManager.default.fileProviderServicesForItem(at: SERVICE_PROXY_ITEM_URL)
    guard let service = services[SIMPLEX_SERVICE_NAME] else { throw ServiceError.noService }
    let connection = try await service.fileProviderConnection()
    connection.remoteObjectInterface = simpleXServiceInterface
    connection.resume()
    var err: ServiceError?
    let rawProxy = connection.remoteObjectProxyWithErrorHandler { error in err = .noProxy(error) }
    if let err = err { throw ServiceError.noProxy(err) }
    guard let proxy = rawProxy as? SimpleXServiceProtocol else { throw ServiceError.badProxy }
    return proxy
}

enum ServiceError: Error {
    case noService
    case noProxy(Error)
    case badProxy
}
