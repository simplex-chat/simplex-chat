//
//  SimpleXFPService.swift
//  SimpleX Service
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import FileProvider
import SimpleXServiceProtocol

extension FileProviderExtension {
    class SimpleXFPService: NSObject, NSFileProviderServiceSource, SimpleXFPServiceProtocol, NSXPCListenerDelegate {
        var serviceName: NSFileProviderServiceName { SIMPLEX_SERVICE_NAME }

        func makeListenerEndpoint() throws -> NSXPCListenerEndpoint {
            logger.debug("SimpleXFPService.makeListenerEndpoint")
            let listener = NSXPCListener.anonymous()
            listener.delegate = self
            synchronized(self) {
                listeners.add(listener)
            }
            listener.resume()
            return listener.endpoint
        }

        func listener(_ listener: NSXPCListener, shouldAcceptNewConnection newConnection: NSXPCConnection) -> Bool {
            logger.debug("SimpleXFPService.listener")
            newConnection.exportedInterface = simpleXServiceInterface
            newConnection.exportedObject = self

            synchronized(self) {
                listeners.remove(listener)
            }

            newConnection.resume()
            return true
        }

        weak var ext: FileProviderExtension?
        let listeners = NSHashTable<NSXPCListener>()

        init(_ ext: FileProviderExtension) {
            self.ext = ext
        }

        func upperCaseString(_ string: String, withReply reply: @escaping (String) -> Void) {
            logger.debug("FileProviderExtension SimpleXFPService.upperCaseString")
            let response = string.uppercased()
            reply(response)
        }
    }

    override func supportedServiceSources(for itemIdentifier: NSFileProviderItemIdentifier) throws -> [NSFileProviderServiceSource] {
        logger.debug("FileProviderExtension.supportedServiceSources")
        return [SimpleXFPService(self)]
    }
}

public func synchronized<T>(_ lock: AnyObject, _ closure: () throws -> T) rethrows -> T {
  objc_sync_enter(lock)
  defer { objc_sync_exit(lock) }
  return try closure()
}
