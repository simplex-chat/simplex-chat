//
//  FileUtils.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.04.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import OSLog

let logger = Logger()

// maximum image file size to be auto-accepted
public let MAX_IMAGE_SIZE: Int64 = 236700

public let MAX_IMAGE_SIZE_AUTO_RCV: Int64 = MAX_IMAGE_SIZE * 2

public let MAX_FILE_SIZE: Int64 = 8000000

public let MAX_VOICE_MESSAGE_LENGTH = TimeInterval(30)

public let MAX_VOICE_MESSAGE_SIZE_INLINE_SEND: Int64 = 94680

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

public func fileSize(_ url: URL) -> Int? { // in bytes
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

public func getAppFilePath(_ fileName: String) -> URL {
    getAppFilesDirectory().appendingPathComponent(fileName)
}

public func saveFile(_ data: Data, _ fileName: String) -> String? {
    let filePath = getAppFilePath(fileName)
    do {
        try data.write(to: filePath)
        return fileName
    } catch {
        logger.error("FileUtils.saveFile error: \(error.localizedDescription)")
        return nil
    }
}

public func removeFile(_ fileName: String) {
    do {
        try FileManager.default.removeItem(atPath: getAppFilePath(fileName).path)
    } catch {
        logger.error("FileUtils.removeFile error: \(error.localizedDescription)")
    }
}
