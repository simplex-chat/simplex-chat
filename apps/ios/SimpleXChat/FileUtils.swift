//
//  FileUtils.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.04.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import OSLog

let logger = Logger()

// maximum image file size to be auto-accepted
public let maxImageSize: Int64 = 236700

public let maxFileSize: Int64 = 8000000

private let CHAT_DB: String = "_chat.db"

private let AGENT_DB: String = "_agent.db"

private let CHAT_DB_BAK: String = "_chat.db.bak"

private let AGENT_DB_BAK: String = "_agent.db.bak"

public func getDocumentsDirectory() -> URL {
    FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
}

func getGroupContainerDirectory() -> URL {
    FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: APP_GROUP_NAME)!
}

func getAppDirectory() -> URL {
    dbContainerGroupDefault.get() == .group
    ? getGroupContainerDirectory()
    : getDocumentsDirectory()
//    getDocumentsDirectory()
}

let DB_FILE_PREFIX = "simplex_v1"

func getLegacyDatabasePath() -> URL {
    getDocumentsDirectory().appendingPathComponent("mobile_v1", isDirectory: false)
}

public func getAppDatabasePath() -> URL {
    dbContainerGroupDefault.get() == .group
    ? getGroupContainerDirectory().appendingPathComponent(DB_FILE_PREFIX, isDirectory: false)
    : getLegacyDatabasePath()
}

func fileModificationDate(_ path: String) -> Date? {
    do {
        let attr = try FileManager.default.attributesOfItem(atPath: path)
        return attr[FileAttributeKey.modificationDate] as? Date
    } catch {
        return nil
    }
}

public func deleteAppFiles() {
    let fm = FileManager.default
    do {
        let fileNames = try fm.contentsOfDirectory(atPath: getAppFilesDirectory().path)
        for fileName in fileNames {
            removeFile(fileName)
        }
    } catch {
        logger.error("FileUtils deleteAppFiles error: \(error.localizedDescription)")
    }
}

func fileSize(_ url: URL) -> Int? { // in bytes
    do {
        let val = try url.resourceValues(forKeys: [.totalFileAllocatedSizeKey, .fileAllocatedSizeKey])
        return val.totalFileAllocatedSize ?? val.fileAllocatedSize
    } catch {
        logger.error("FileUtils fileSize error: \(error.localizedDescription)")
        return nil
    }
}

public func directoryFileCountAndSize(_ dir: URL) -> (Int, Int)? { // size in bytes
    let fm = FileManager.default
    if let enumerator = fm.enumerator(at: dir, includingPropertiesForKeys: [.totalFileAllocatedSizeKey, .fileAllocatedSizeKey], options: [], errorHandler: { (_, error) -> Bool in
        logger.error("FileUtils directoryFileCountAndSize error: \(error.localizedDescription)")
        return false
    }) {
        var fileCount = 0
        var bytes = 0
        for case let url as URL in enumerator {
            fileCount += 1
            bytes += fileSize(url) ?? 0
        }
        return (fileCount, bytes)
    } else {
        return nil
    }
}

public func hasBackup(newerThan date: Date) -> Bool {
    let dbPath = getAppDatabasePath().path
    return hasBackupFile(dbPath + AGENT_DB_BAK, newerThan: date)
        && hasBackupFile(dbPath + CHAT_DB_BAK, newerThan: date)
}

private func hasBackupFile(_ path: String, newerThan date: Date) -> Bool {
    let fm = FileManager.default
    return fm.fileExists(atPath: path)
            && date <= (fileModificationDate(path) ?? Date.distantPast)
}

public func restoreBackup() throws {
    let dbPath = getAppDatabasePath().path
    try restoreBackupFile(fromPath: dbPath + AGENT_DB_BAK, toPath: dbPath + AGENT_DB)
    try restoreBackupFile(fromPath: dbPath + CHAT_DB_BAK, toPath: dbPath + CHAT_DB)
}

private func restoreBackupFile(fromPath: String, toPath: String) throws {
    let fm = FileManager.default
    if fm.fileExists(atPath: toPath) {
        try fm.removeItem(atPath: toPath)
    }
    try fm.copyItem(atPath: fromPath, toPath: toPath)
}

public func hasLegacyDatabase() -> Bool {
    hasDatabaseAtPath(getLegacyDatabasePath())
}

public func hasDatabase() -> Bool {
    hasDatabaseAtPath(getAppDatabasePath())
}

func hasDatabaseAtPath(_ dbPath: URL) -> Bool {
    let fm = FileManager.default
    return fm.isReadableFile(atPath: dbPath.path + AGENT_DB) &&
           fm.isReadableFile(atPath: dbPath.path + CHAT_DB)
}

public func removeLegacyDatabaseAndFiles() -> Bool {
    let dbPath = getLegacyDatabasePath()
    let appFiles = getDocumentsDirectory().appendingPathComponent("app_files", isDirectory: true)
    let fm = FileManager.default
    let r1 = nil != (try? fm.removeItem(atPath: dbPath.path + AGENT_DB))
    let r2 = nil != (try? fm.removeItem(atPath: dbPath.path + CHAT_DB))
    try? fm.removeItem(atPath: dbPath.path + AGENT_DB_BAK)
    try? fm.removeItem(atPath: dbPath.path + CHAT_DB_BAK)
    try? fm.removeItem(at: appFiles)
    return r1 && r2
}

public func getAppFilesDirectory() -> URL {
    getAppDirectory().appendingPathComponent("app_files", isDirectory: true)
}

func getAppFilePath(_ fileName: String) -> URL {
    getAppFilesDirectory().appendingPathComponent(fileName)
}

public func getLoadedFilePath(_ file: CIFile?) -> String? {
    if let file = file,
       file.loaded,
       let savedFile = file.filePath {
        return getAppFilePath(savedFile).path
    }
    return nil
}

public func getLoadedImage(_ file: CIFile?) -> UIImage? {
    if let filePath = getLoadedFilePath(file) {
        return UIImage(contentsOfFile: filePath)
    }
    return nil
}

public func saveFileFromURL(_ url: URL) -> String? {
    let savedFile: String?
    if url.startAccessingSecurityScopedResource() {
        do {
            let fileData = try Data(contentsOf: url)
            let fileName = uniqueCombine(url.lastPathComponent)
            savedFile = saveFile(fileData, fileName)
        } catch {
            logger.error("FileUtils.saveFileFromURL error: \(error.localizedDescription)")
            savedFile = nil
        }
    } else {
        logger.error("FileUtils.saveFileFromURL startAccessingSecurityScopedResource returned false")
        savedFile = nil
    }
    url.stopAccessingSecurityScopedResource()
    return savedFile
}

public func saveImage(_ uiImage: UIImage) -> String? {
    if let imageDataResized = resizeImageToDataSize(uiImage, maxDataSize: maxImageSize) {
        let timestamp = Date().getFormattedDate("yyyyMMdd_HHmmss")
        let fileName = uniqueCombine("IMG_\(timestamp).jpg")
        return saveFile(imageDataResized, fileName)
    }
    return nil
}

extension Date {
  func getFormattedDate(_ format: String) -> String {
    let df = DateFormatter()
      df.dateFormat = format
      df.locale = Locale(identifier: "US")
    return df.string(from: self)
  }
}

private func saveFile(_ data: Data, _ fileName: String) -> String? {
    let filePath = getAppFilePath(fileName)
    do {
        try data.write(to: filePath)
        return fileName
    } catch {
        logger.error("FileUtils.saveFile error: \(error.localizedDescription)")
        return nil
    }
}

private func uniqueCombine(_ fileName: String) -> String {
    func tryCombine(_ fileName: String, _ n: Int) -> String {
        let ns = fileName as NSString
        let name = ns.deletingPathExtension
        let ext = ns.pathExtension
        let suffix = (n == 0) ? "" : "_\(n)"
        let f = "\(name)\(suffix).\(ext)"
        return (FileManager.default.fileExists(atPath: getAppFilePath(f).path)) ? tryCombine(fileName, n + 1) : f
    }
    return tryCombine(fileName, 0)
}

public func removeFile(_ fileName: String) {
    do {
        try FileManager.default.removeItem(atPath: getAppFilePath(fileName).path)
    } catch {
        logger.error("FileUtils.removeFile error: \(error.localizedDescription)")
    }
}

// image utils

public func dropImagePrefix(_ s: String) -> String {
    dropPrefix(dropPrefix(s, "data:image/png;base64,"), "data:image/jpg;base64,")
}

private func dropPrefix(_ s: String, _ prefix: String) -> String {
    s.hasPrefix(prefix) ? String(s.dropFirst(prefix.count)) : s
}

public func cropToSquare(_ image: UIImage) -> UIImage {
    let size = image.size
    let side = min(size.width, size.height)
    let newSize = CGSize(width: side, height: side)
    var origin = CGPoint.zero
    if size.width > side {
        origin.x -= (size.width - side) / 2
    } else if size.height > side {
        origin.y -= (size.height - side) / 2
    }
    return resizeImage(image, newBounds: CGRect(origin: .zero, size: newSize), drawIn: CGRect(origin: origin, size: size))
}

func resizeImageToDataSize(_ image: UIImage, maxDataSize: Int64) -> Data? {
    var img = image
    var data = img.jpegData(compressionQuality: 0.85)
    var dataSize = data?.count ?? 0
    while dataSize != 0 && dataSize > maxDataSize {
        let ratio = sqrt(Double(dataSize) / Double(maxDataSize))
        let clippedRatio = min(ratio, 2.0)
        img = reduceSize(img, ratio: clippedRatio)
        data = img.jpegData(compressionQuality: 0.85)
        dataSize = data?.count ?? 0
    }
    logger.debug("resizeImageToDataSize final \(dataSize)")
    return data
}

public func resizeImageToStrSize(_ image: UIImage, maxDataSize: Int64) -> String? {
    var img = image
    var str = compressImageStr(img)
    var dataSize = str?.count ?? 0
    while dataSize != 0 && dataSize > maxDataSize {
        let ratio = sqrt(Double(dataSize) / Double(maxDataSize))
        let clippedRatio = min(ratio, 2.0)
        img = reduceSize(img, ratio: clippedRatio)
        str = compressImageStr(img)
        dataSize = str?.count ?? 0
    }
    logger.debug("resizeImageToStrSize final \(dataSize)")
    return str
}

func compressImageStr(_ image: UIImage, _ compressionQuality: CGFloat = 0.85) -> String? {
    if let data = image.jpegData(compressionQuality: compressionQuality) {
        return "data:image/jpg;base64,\(data.base64EncodedString())"
    }
    return nil
}

private func reduceSize(_ image: UIImage, ratio: CGFloat) -> UIImage {
    let newSize = CGSize(width: floor(image.size.width / ratio), height: floor(image.size.height / ratio))
    let bounds = CGRect(origin: .zero, size: newSize)
    return resizeImage(image, newBounds: bounds, drawIn: bounds)
}

private func resizeImage(_ image: UIImage, newBounds: CGRect, drawIn: CGRect) -> UIImage {
    let format = UIGraphicsImageRendererFormat()
    format.scale = 1.0
    format.opaque = true
    return UIGraphicsImageRenderer(bounds: newBounds, format: format).image { _ in
        image.draw(in: drawIn)
    }
}
