//
//  ServiceProtocol.swift
//  SimpleXServiceProtocol
//
//  Created by Evgeny on 09/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import FileProvider

public let SIMPLEX_SERVICE_NAME = NSFileProviderServiceName("group.chat.simplex.app.service")
public let SERVICE_PROXY_ITEM_ID = NSFileProviderItemIdentifier("123")
public let simpleXServiceInterface: NSXPCInterface = {
    NSXPCInterface(with: SimpleXFPServiceProtocol.self)
}()

@objc public protocol SimpleXFPServiceProtocol {
    func upperCaseString(_ string: String, withReply reply: @escaping (String) -> Void)
}
