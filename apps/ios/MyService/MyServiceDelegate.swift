//
//  MyServiceDelegate.swift
//  MyService
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

// MyServiceDelegate.swift
import Foundation

class MyServiceDelegate: NSObject, NSXPCListenerDelegate {
    func listener(_ listener: NSXPCListener, shouldAcceptNewConnection newConnection: NSXPCConnection) -> Bool {
        let exportedObject = MyService()
        newConnection.exportedInterface = NSXPCInterface(with: MyServiceProtocol.self)
        newConnection.exportedObject = exportedObject
        newConnection.resume()
        return true
    }
}
