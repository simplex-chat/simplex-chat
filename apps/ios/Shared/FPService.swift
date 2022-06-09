//
//  FPService.swift
//  SimpleX Service
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import FileProvider
import SimpleXChat
import SimpleXServiceProtocol

func testFPService() {
    logger.debug("testFPService get services")
    let manager = NSFileProviderManager.default
    // TODO try access file
    logger.debug("testFPService NSFileProviderManager.documentStorageURL \(manager.documentStorageURL, privacy: .public)")
    
//    let res = machMessenger.sendMessageWithReply(FPS_MACH_PORT, msg: "machMessenger before getFileProviderServicesForItem")
//    print("reply 1", res)

    FileManager.default.getFileProviderServicesForItem(at: URL(string: "\(manager.documentStorageURL)123")!) { (services, error) in
//        let res = machMessenger.sendMessageWithReply(FPS_MACH_PORT, msg: "machMessenger after getFileProviderServicesForItem")
//        print("reply 2", res)

        // Check to see if an error occurred.
        guard error == nil else {
            logger.debug("testFPService error getting service")
            print(error!) // <-- this prints the error I posted
            // Handle the error here...
            return
        }

        if let desiredService = services?[SIMPLEX_SERVICE_NAME] {
            logger.debug("testFPService has desiredService")

            // The named service is available for the item at the provided URL.
            // To use the service, get the connection object.
            desiredService.getFileProviderConnection(completionHandler: { (connectionOrNil, connectionError) in

                guard connectionError == nil else {
                    // Handle the error here...
                    return
                }

                guard let connection = connectionOrNil else {
                    // No connection object found.
                    return
                }

                // Set the remote interface.
                connection.remoteObjectInterface = simpleXServiceInterface

                // Start the connection.
                connection.resume()

                // Get the proxy object.
                let rawProxy = connection.remoteObjectProxyWithErrorHandler({ (errorAccessingRemoteObject) in
                    // Handle the error here...
                })

                // Cast the proxy object to the interface's protocol.
                guard let proxy = rawProxy as? SimpleXFPServiceProtocol else {
                    // If the interface is set up properly, this should never fail.
                    fatalError("*** Unable to cast \(rawProxy) to a DesiredProtocol instance ***")
                }

                logger.debug("testFPService calling service")
                proxy.upperCaseString("hello to service", withReply: { reply in
                    logger.debug("testFPService reply from service \(reply)")
                })
            })
        }
    }

}
