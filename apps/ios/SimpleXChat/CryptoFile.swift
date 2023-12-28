//
//  CryptoFile.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 05/09/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import Foundation

enum WriteFileResult: Decodable {
    case result(cryptoArgs: CryptoFileArgs)
    case error(writeError: String)
}

public func writeCryptoFile(path: String, data: Data) throws -> CryptoFileArgs {
    let ptr: UnsafeMutableRawPointer = malloc(data.count)
    memcpy(ptr, (data as NSData).bytes, data.count)
    var cPath = path.cString(using: .utf8)!
    let cjson = chat_write_file(getChatCtrl(), &cPath, ptr, Int32(data.count))!
    let d = fromCString(cjson).data(using: .utf8)!
    switch try jsonDecoder.decode(WriteFileResult.self, from: d) {
    case let .result(cfArgs): return cfArgs
    case let .error(err): throw RuntimeError(err)
    }
}

public func readCryptoFile(path: String, cryptoArgs: CryptoFileArgs) throws -> Data {
    var cPath = path.cString(using: .utf8)!
    var cKey = cryptoArgs.fileKey.cString(using: .utf8)!
    var cNonce = cryptoArgs.fileNonce.cString(using: .utf8)!
    let ptr = chat_read_file(&cPath, &cKey, &cNonce)!
    let status = UInt8(ptr.pointee)
    switch status {
    case 0: // ok
        let dLen = Data(bytes: ptr.advanced(by: 1), count: 4)
        let len = dLen.withUnsafeBytes { $0.load(as: UInt32.self) }
        let d = Data(bytes: ptr.advanced(by: 5), count: Int(len))
        free(ptr)
        return d
    case 1: // error
        let err = String.init(cString: ptr)
        free(ptr)
        throw RuntimeError(err)
    default:
        throw RuntimeError("unexpected chat_read_file status: \(status)")
    }
}

public func encryptCryptoFile(fromPath: String, toPath: String) throws -> CryptoFileArgs {
    var cFromPath = fromPath.cString(using: .utf8)!
    var cToPath = toPath.cString(using: .utf8)!
    let cjson = chat_encrypt_file(getChatCtrl(), &cFromPath, &cToPath)!
    let d = fromCString(cjson).data(using: .utf8)!
    switch try jsonDecoder.decode(WriteFileResult.self, from: d) {
    case let .result(cfArgs): return cfArgs
    case let .error(err): throw RuntimeError(err)
    }
}

public func decryptCryptoFile(fromPath: String, cryptoArgs: CryptoFileArgs, toPath: String) throws {
    var cFromPath = fromPath.cString(using: .utf8)!
    var cKey = cryptoArgs.fileKey.cString(using: .utf8)!
    var cNonce = cryptoArgs.fileNonce.cString(using: .utf8)!
    var cToPath = toPath.cString(using: .utf8)!
    let cErr = chat_decrypt_file(&cFromPath, &cKey, &cNonce, &cToPath)!
    let err = fromCString(cErr)
    if err != "" { throw RuntimeError(err) }
}
