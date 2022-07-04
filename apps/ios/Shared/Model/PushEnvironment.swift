//
//  PushEnvironment.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 27/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

let pushEnvironment: PushEnvironment = {
    guard let provisioningProfile = try? provisioningProfile(),
          let entitlements = provisioningProfile["Entitlements"] as? [String: Any],
          let environment = entitlements["aps-environment"] as? String,
          let env = PushEnvironment(rawValue: environment)
    else {
        logger.warning("pushEnvironment: unknown, assuming production")
        return .production
    }
    logger.debug("pushEnvironment: \(env.rawValue)")
    return env
}()

private func provisioningProfile() throws -> [String: Any]? {
    guard let url = Bundle.main.url(forResource: "embedded", withExtension: "mobileprovision") else {
        return nil
    }

    let binaryString = try String(contentsOf: url, encoding: .isoLatin1)

    let scanner = Scanner(string: binaryString)
    guard scanner.scanUpToString("<plist") != nil,
          let plistString = scanner.scanUpToString("</plist>"),
          let data = (plistString + "</plist>").data(using: .isoLatin1)
    else {
        return nil
    }

    return try PropertyListSerialization.propertyList(from: data, options: [], format: nil) as? [String: Any]
}
