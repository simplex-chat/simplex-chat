//
//  main.swift
//  MyService
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

import Foundation

let delegate = MyServiceDelegate()
let listener = NSXPCListener.service()
listener.delegate = delegate
listener.resume()
