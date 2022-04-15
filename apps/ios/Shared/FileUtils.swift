//
//  FileUtils.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.04.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

func getDocumentsDirectory() -> URL {
    return FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
}

func getAppFilesDirectory() -> URL {
    return getDocumentsDirectory().appendingPathComponent("app_files", isDirectory: true)
}
