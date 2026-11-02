// Prints the average color of a screenshot's region as "r g b" (0-255), to tell whether a call screen shows the
// peer's video (one of test-video.swift's saturated colors) or a dark view.
// Usage: call-frame <png> <x> <y> <width> <height>   (pixels of the screenshot)

import CoreGraphics
import Foundation
import ImageIO

let args = CommandLine.arguments
guard args.count == 6, let x = Int(args[2]), let y = Int(args[3]), let w = Int(args[4]), let h = Int(args[5]) else {
    fatalError("usage: call-frame <png> <x> <y> <width> <height>")
}
guard let source = CGImageSourceCreateWithURL(URL(fileURLWithPath: args[1]) as CFURL, nil),
      let image = CGImageSourceCreateImageAtIndex(source, 0, nil),
      let region = image.cropping(to: CGRect(x: x, y: y, width: w, height: h))
else { fatalError("cannot read the region \(x),\(y) \(w)x\(h) of \(args[1])") }

let space = CGColorSpace(name: CGColorSpace.sRGB)!
let ctx = CGContext(data: nil, width: w, height: h, bitsPerComponent: 8, bytesPerRow: 4 * w, space: space,
                    bitmapInfo: CGImageAlphaInfo.noneSkipLast.rawValue)!
ctx.draw(region, in: CGRect(x: 0, y: 0, width: w, height: h))
let pixels = ctx.data!.assumingMemoryBound(to: UInt8.self)
var sums = [0, 0, 0]
for i in 0..<(w * h) {
    for c in 0..<3 { sums[c] += Int(pixels[4 * i + c]) }
}
print(sums.map { String($0 / (w * h)) }.joined(separator: " "))
