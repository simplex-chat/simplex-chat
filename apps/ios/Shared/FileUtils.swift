//
//  FileUtils.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.04.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

func getDocumentsDirectory() -> URL {
    return FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
}

func getAppFilesDirectory() -> URL {
    return getDocumentsDirectory().appendingPathComponent("app_files", isDirectory: true)
}

func getStoredFilePath(_ file: CIFile?) -> String? {
    if let file = file,
       file.stored,
       let savedFile = file.filePath {
        return getAppFilesDirectory().path + "/" + savedFile
    }
    return nil
}

func getStoredImage(_ file: CIFile?) -> UIImage? {
    if let filePath = getStoredFilePath(file) {
        return UIImage(contentsOfFile: filePath)
    }
    return nil
}
