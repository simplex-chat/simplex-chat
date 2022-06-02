//
//  MyService.swift
//  MyService
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

class MyService: NSObject, MyServiceProtocol {
    func upperCaseString(_ string: String, withReply reply: @escaping (String) -> Void) {
        let response = string.uppercased()
        reply(response)
    }
}
