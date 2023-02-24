import UIKit
import SimpleXChat

hs_init(0, nil)

let totalBytes = 200
let ivTagBytes = 28
var base: UnsafeMutableRawPointer = malloc(totalBytes)

let assume = base.assumingMemoryBound(to: UInt8.self)
assume[0] = 1 // key frame
for i in 0..<28 {
    assume[totalBytes - ivTagBytes + i] = UInt8(i)
}
let unencrypted = NSData(bytesNoCopy: base, length: 200)
let aesKey = "PI-bV-FTgRqZM_lsDH9T21a0yRVMsvLFmvilJ9Ssk3g="

if var key: [CChar] = aesKey.cString(using: .utf8),
   let pointer: UnsafeMutableRawPointer = malloc(unencrypted.count + ivTagBytes) {
    debugPrint("AesKey \(aesKey), cString \(key)")
    memcpy(pointer, (unencrypted as NSData).bytes, unencrypted.count)
    //let raw: UInt8 = (unencrypted[0] as UInt8) | ((unencrypted[1] as UInt8) << 8) | ((unencrypted[2] as UInt8) << 16)
    let isKeyFrame = unencrypted[0] & 1 == 1
    debugPrint("Is key frame \(isKeyFrame)")
    let clearTextBytesSize = isKeyFrame ? 10 : 3
    for i in 0..<30 {
        debugPrint("Before \(i)  \(unencrypted[i + clearTextBytesSize])")
    }
    chat_encrypt_media(&key, pointer.advanced(by: clearTextBytesSize), Int32(unencrypted.count + ivTagBytes - clearTextBytesSize))
    for i in 0..<30 {
        debugPrint("After \(i)  \(pointer.advanced(by: clearTextBytesSize).assumingMemoryBound(to: UInt8.self)[i])")
    }
    let res_ = Data(bytes: pointer, count: unencrypted.count + ivTagBytes)
}
