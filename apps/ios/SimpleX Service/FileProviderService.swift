//
//  SimpleXFPService.swift
//  SimpleX Service
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import FileProvider
import SimpleXChat
import SimpleXServiceProtocol

extension FileProviderExtension {
    class FileProviderService: NSObject, NSFileProviderServiceSource, SimpleXServiceProtocol, NSXPCListenerDelegate {
        var serviceName: NSFileProviderServiceName { SIMPLEX_SERVICE_NAME }

        func makeListenerEndpoint() throws -> NSXPCListenerEndpoint {
            logger.debug("FileProviderService.makeListenerEndpoint")
            let listener = NSXPCListener.anonymous()
            listener.delegate = self
            synchronized(self) {
                listeners.add(listener)
            }
            listener.resume()
            return listener.endpoint
        }

        func listener(_ listener: NSXPCListener, shouldAcceptNewConnection newConnection: NSXPCConnection) -> Bool {
            logger.debug("FileProviderService.listener")
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
            hs_init(0, nil)
        }

        func chatSendCmd(_ cmd: String) async -> String {
            logger.debug("chatSendCmd cmd: \(cmd, privacy: .public)")
            let r = SimpleXChat.chatSendCmd(cmd)
            logger.debug("chatSendCmd resp: \(r, privacy: .public)")
            return r
        }

        func chatRecvMsg() async -> String {
            SimpleXChat.chatRecvMsg()
        }
    }

    override func supportedServiceSources(for itemIdentifier: NSFileProviderItemIdentifier) throws -> [NSFileProviderServiceSource] {
        logger.debug("FileProviderExtension.supportedServiceSources")
        return [FileProviderService(self)]
    }
}

private func synchronized<T>(_ lock: AnyObject, _ closure: () throws -> T) rethrows -> T {
  objc_sync_enter(lock)
  defer { objc_sync_exit(lock) }
  return try closure()
}
