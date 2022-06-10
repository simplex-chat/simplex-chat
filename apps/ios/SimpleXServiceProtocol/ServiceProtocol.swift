//
//  ServiceProtocol.swift
//  SimpleXServiceProtocol
//
//  Created by Evgeny on 09/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import FileProvider
import OSLog

let logger = Logger()

public let SIMPLEX_SERVICE_NAME = NSFileProviderServiceName("group.chat.simplex.app.service")
public let SERVICE_PROXY_ITEM_NAME = "123"
public let SERVICE_PROXY_ITEM_ID = NSFileProviderItemIdentifier(SERVICE_PROXY_ITEM_NAME)
public let SERVICE_PROXY_ITEM_URL = URL(string: "\(NSFileProviderManager.default.documentStorageURL)\(SERVICE_PROXY_ITEM_NAME)")!
public let simpleXServiceInterface: NSXPCInterface = {
    NSXPCInterface(with: SimpleXServiceProtocol.self)
}()

@objc public protocol SimpleXServiceProtocol {
    func chatSendCmd(_ cmd: String) async -> String
    func chatRecvMsg() async -> String
}

