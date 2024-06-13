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

// image file size for complession
public let MAX_IMAGE_SIZE: Int64 = 261_120 // 255KB

public let MAX_IMAGE_SIZE_AUTO_RCV: Int64 = MAX_IMAGE_SIZE * 2

public let MAX_VOICE_SIZE_AUTO_RCV: Int64 = MAX_IMAGE_SIZE * 2

public let MAX_VIDEO_SIZE_AUTO_RCV: Int64 = 1_047_552 // 1023KB

public let MAX_FILE_SIZE_XFTP: Int64 = 1_073_741_824 // 1GB

public let MAX_FILE_SIZE_LOCAL: Int64 = Int64.max

public let MAX_FILE_SIZE_SMP: Int64 = 8000000

public let MAX_VOICE_MESSAGE_LENGTH = TimeInterval(300)

let CHAT_DB: String = "_chat.db"

let AGENT_DB: String = "_agent.db"

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

public func deleteAppDatabaseAndFiles() {
    let fm = FileManager.default
    let dbPath = getAppDatabasePath().path
    do {
        try fm.removeItem(atPath: dbPath + CHAT_DB)
        try fm.removeItem(atPath: dbPath + AGENT_DB)
    } catch let error {
        logger.error("Failed to delete all databases: \(error)")
    }
    try? fm.removeItem(atPath: dbPath + CHAT_DB_BAK)
    try? fm.removeItem(atPath: dbPath + AGENT_DB_BAK)
    deleteAppTempFiles()
    deleteAppFiles()
    _ = kcDatabasePassword.remove()
    storeDBPassphraseGroupDefault.set(true)
}

public func deleteAppFiles() {
    let fm = FileManager.default
    do {
        try fm.removeItem(at: getAppFilesDirectory())
        try fm.createDirectory(at: getAppFilesDirectory(), withIntermediateDirectories: true)
    } catch {
        logger.error("FileUtils deleteAppFiles error: \(error.localizedDescription)")
    }
}

public func deleteAppTempFiles() {
    let fm = FileManager.default
    do {
        try fm.removeItem(at: getTempFilesDirectory())
        try fm.createDirectory(at: getTempFilesDirectory(), withIntermediateDirectories: true)
    } catch {
        logger.error("FileUtils deleteAppTempFiles error: \(error.localizedDescription)")
    }
    do {
        try fm.removeItem(at: getMigrationTempFilesDirectory())
    } catch {
        logger.error("FileUtils deleteAppTempFiles fm.removeItem(at: getMigrationTempFilesDirectory()) error: \(error.localizedDescription)")
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

public func getTempFilesDirectory() -> URL {
    getAppDirectory().appendingPathComponent("temp_files", isDirectory: true)
}

public func getMigrationTempFilesDirectory() -> URL {
    getDocumentsDirectory().appendingPathComponent("migration_temp_files", isDirectory: true)
}

public func getAppFilesDirectory() -> URL {
    getAppDirectory().appendingPathComponent("app_files", isDirectory: true)
}

public func getAppFilePath(_ fileName: String) -> URL {
    getAppFilesDirectory().appendingPathComponent(fileName)
}

public func saveFile(_ data: Data, _ fileName: String, encrypted: Bool) -> CryptoFile? {
    let filePath = getAppFilePath(fileName)
    do {
        if encrypted {
            let cfArgs = try writeCryptoFile(path: filePath.path, data: data)
            return CryptoFile(filePath: fileName, cryptoArgs: cfArgs)
        } else {
            try data.write(to: filePath)
            return CryptoFile.plain(fileName)
        }
    } catch {
        logger.error("FileUtils.saveFile error: \(error.localizedDescription)")
        return nil
    }
}

public func removeFile(_ url: URL) {
    do {
        try FileManager.default.removeItem(atPath: url.path)
    } catch {
        logger.error("FileUtils.removeFile error: \(error.localizedDescription)")
    }
}

public func removeFile(_ fileName: String) {
    do {
        try FileManager.default.removeItem(atPath: getAppFilePath(fileName).path)
    } catch {
        logger.error("FileUtils.removeFile error: \(error.localizedDescription)")
    }
}

public func cleanupDirectFile(_ aChatItem: AChatItem) {
    if aChatItem.chatInfo.chatType == .direct {
        cleanupFile(aChatItem)
    }
}

public func cleanupFile(_ aChatItem: AChatItem) {
    let cItem = aChatItem.chatItem
    let mc = cItem.content.msgContent
    if case .file = mc,
       let fileName = cItem.file?.fileSource?.filePath {
        removeFile(fileName)
    }
}

public func getMaxFileSize(_ fileProtocol: FileProtocol) -> Int64 {
    switch fileProtocol {
    case .xftp: return MAX_FILE_SIZE_XFTP
    case .smp: return MAX_FILE_SIZE_SMP
    case .local: return MAX_FILE_SIZE_LOCAL
    }
}

public struct RuntimeError: Error {
    let message: String

    public init(_ message: String) {
        self.message = message
    }

    public var localizedDescription: String {
        return message
    }
}
