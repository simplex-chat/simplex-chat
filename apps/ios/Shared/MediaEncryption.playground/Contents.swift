import UIKit
import SimpleXChat

hs_init(0, nil)

let totalBytes = 20
let ivTagBytes = 28
var base: UnsafeMutableRawPointer = malloc(totalBytes)

let assume = base.assumingMemoryBound(to: UInt8.self)
assume[0] = 0 // key frame
for i in 1..<totalBytes {
    assume[i] = UInt8(i)
}
let unencrypted = NSData(bytesNoCopy: base, length: totalBytes)
let aesKey = "PI-bV-FTgRqZM_lsDH9T21a0yRVMsvLFmvilJ9Ssk3g="

if var key: [CChar] = aesKey.cString(using: .utf8),
   let pointer: UnsafeMutableRawPointer = malloc(unencrypted.count + ivTagBytes) {
    debugPrint("AesKey \(aesKey), cString \(key)")
    memcpy(pointer, (unencrypted as NSData).bytes, unencrypted.count)
    let source_ = Data(bytes: pointer, count: unencrypted.count)
    //let raw: UInt8 = (unencrypted[0] as UInt8) | ((unencrypted[1] as UInt8) << 8) | ((unencrypted[2] as UInt8) << 16)
    let isKeyFrame = unencrypted[0] & 1 == 0
    debugPrint("Is key frame \(isKeyFrame)")
    let clearTextBytesSize = isKeyFrame ? 10 : 3
    for i in 0..<48 {
        debugPrint("Before \(i)  \(unencrypted[i])")
    }
    if let res = chat_encrypt_media(&key, pointer.advanced(by: clearTextBytesSize), Int32(unencrypted.count + ivTagBytes - clearTextBytesSize)) {
        printError("encrypt", res)
    }
    for i in 0..<48 {
        debugPrint("After \(i)  \(pointer.assumingMemoryBound(to: UInt8.self)[i])")
    }
    let res_ = Data(bytes: pointer, count: unencrypted.count + ivTagBytes)
    print(source_ == res_)



//    let encryptedBytes = [1,   1,   2,   3,   4,   5,   6,   7,   8,   9,
//                     250, 245, 192, 217, 164, 251,  23,  40,  36, 214,
//                      84,  55, 114, 237, 153, 113, 182, 123, 214, 189,
//                      35, 196, 148, 164, 235, 195, 122, 157, 141, 235,
//                       5,  92,  44,  35,  37, 244,  90, 254]
//    var base1: UnsafeMutableRawPointer = malloc(totalBytes + ivTagBytes)
//
//    let assume1 = base1.assumingMemoryBound(to: UInt8.self)
//    for i in 0..<(totalBytes + ivTagBytes) {
//        assume1[i] = UInt8(encryptedBytes[i])
//    }
//    let encrypted = NSData(bytesNoCopy: base1, length: totalBytes + ivTagBytes)
//    memcpy(pointer, (encrypted as NSData).bytes, encrypted.count)
//    for i in 0..<48 {
//        debugPrint("Before decrypt \(i)  \(pointer.assumingMemoryBound(to: UInt8.self)[i])")
//    }


    if let res = chat_decrypt_media(&key, pointer.advanced(by: clearTextBytesSize), Int32(unencrypted.count + ivTagBytes - clearTextBytesSize)) {
        printError("decrypt", res)
    }


    let decrypted_ = Data(bytes: pointer, count: unencrypted.count)
    for i in 0..<48 {
        debugPrint("After decrypt \(i)  \(pointer.assumingMemoryBound(to: UInt8.self)[i])")
    }
    print(source_ == decrypted_)
}

func printError(_ op: String, _ res: UnsafeMutablePointer<CChar>) {
    let err = fromCString(res)
    if err == "" {
        print("\(op) ok")
    } else {
        print("\(op) error: \(err)")
    }
}
