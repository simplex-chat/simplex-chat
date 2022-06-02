//
//  SimpleXFPService.swift
//  SimpleX Service
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import FileProvider

let SIMPLEX_SERVICE_NAME = NSFileProviderServiceName("group.chat.simplex.app.service")
//let SERVICE_PROXY_ITEM = "chat.simplex.service:/123"
//let SERVICE_PROXY_ITEM_URL = URL(string: SERVICE_PROXY_ITEM)!
let SERVICE_PROXY_ITEM_ID = NSFileProviderItemIdentifier("123")

class SimpleXFPService: SimpleXFPServiceProtocol {
    func upperCaseString(_ string: String, withReply reply: @escaping (String) -> Void) {
        logger.debug("FileProviderExtension SimpleXFPService.upperCaseString")
        let response = string.uppercased()
        reply(response)
    }
}

@objc public protocol SimpleXFPServiceProtocol {
    func upperCaseString(_ string: String, withReply reply: @escaping (String) -> Void)
}

class SimpleXFPServiceDelegate: NSObject, NSXPCListenerDelegate {
    func listener(_ listener: NSXPCListener, shouldAcceptNewConnection newConnection: NSXPCConnection) -> Bool {
        logger.debug("FileProviderExtension SimpleXFPServiceDelegate.listener")
        newConnection.exportedInterface = NSXPCInterface(with: SimpleXFPServiceProtocol.self)
        newConnection.exportedObject = SimpleXFPService()
        newConnection.resume()
        return true
    }
}

extension FileProviderExtension: NSFileProviderServiceSource {
    override func supportedServiceSources(for itemIdentifier: NSFileProviderItemIdentifier) throws -> [NSFileProviderServiceSource] {
        logger.debug("FileProviderExtension.supportedServiceSources")
        return [self]
    }

    var serviceName: NSFileProviderServiceName { SIMPLEX_SERVICE_NAME }

    func makeListenerEndpoint() throws -> NSXPCListenerEndpoint {
        logger.debug("FileProviderExtension.makeListenerEndpoint")
        return serviceListener.endpoint
    }
}
