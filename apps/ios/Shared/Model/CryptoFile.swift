//
//  CryptoFile.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 05/09/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

enum WriteFileResult: Decodable {
    case result(cryptoArgs: CryptoFileArgs)
    case error(writeError: String)
}

func writeCryptoFile(path: String, data: Data) -> WriteFileResult {
    let ptr: UnsafeMutableRawPointer = malloc(data.count)
    memcpy(ptr, (data as NSData).bytes, data.count)
    var cPath = path.cString(using: .utf8)!
    let cjson = chat_write_file(&cPath, ptr, Int32(data.count))!
    let d = fromCString(cjson).data(using: .utf8)!
    do {
        return try jsonDecoder.decode(WriteFileResult.self, from: d)
    } catch {
        logger.error("writeCryptoFile jsonDecoder.decode error: \(error.localizedDescription)")
        return .error(writeError: error.localizedDescription)
    }
}

enum ReadFileResult: Decodable {
    case result(fileSize: Int)
    case error(readError: String)
}

enum ReadFileData {
    case result(Data)
    case error(String)
}

func readCryptoFile(path: String, cryptoArgs: CryptoFileArgs) throws -> Data {
    var cPath = path.cString(using: .utf8)!
    var cKey = cryptoArgs.fileKey.cString(using: .utf8)!
    var cNonce = cryptoArgs.fileNonce.cString(using: .utf8)!
    let r = chat_read_file(&cPath, &cKey, &cNonce)!
    let d = String.init(cString: r).data(using: .utf8)!
    switch try jsonDecoder.decode(ReadFileResult.self, from: d) {
    case let .error(err): throw RuntimeError(err)
    case let .result(size): return Data(bytes: r.advanced(by: d.count + 1), count: size)
    }
}
