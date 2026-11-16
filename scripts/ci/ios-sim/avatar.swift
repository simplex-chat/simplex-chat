// Makes a group avatar:
// a JPEG at quality 0.85 drawn at scale 1, shrunk by sqrt(size / 12500), at most halved per step,
// until its data URI is at most 12500 characters. The picture is synthetic and photo-like: a gradient,
// discs and noise, with more noise for a higher detail.
// Usage: avatar <seed> <detail 0...1> <output file> [side in pixels, default 1024]

import CoreGraphics
import Foundation
import ImageIO

let maxDataSize = 12500
let colorSpace = CGColorSpace(name: CGColorSpace.sRGB)!

struct Random {
    var state: UInt64

    mutating func next() -> Double {
        state = state &* 6364136223846793005 &+ 1442695040888963407
        return Double(state >> 11) / Double(UInt64(1) << 53)
    }

    mutating func color(alpha: Double = 1) -> CGColor {
        let components: [CGFloat] = [next(), next(), next(), alpha]
        return CGColor(colorSpace: colorSpace, components: components)!
    }
}

func context(_ width: Int, _ height: Int) -> CGContext {
    CGContext(data: nil, width: width, height: height, bitsPerComponent: 8, bytesPerRow: 0, space: colorSpace, bitmapInfo: CGImageAlphaInfo.noneSkipLast.rawValue)!
}

func picture(side: Int, detail: Double, random: inout Random) -> CGImage {
    let ctx = context(side, side)
    let gradient = CGGradient(colorsSpace: colorSpace, colors: [random.color(), random.color()] as CFArray, locations: [0, 1])!
    ctx.drawLinearGradient(gradient, start: .zero, end: CGPoint(x: 0, y: side), options: [])
    for _ in 0..<24 {
        let r = Double(side) * (0.05 + 0.25 * random.next())
        let alpha = 0.4 + 0.5 * random.next()
        ctx.setFillColor(random.color(alpha: alpha))
        ctx.fillEllipse(in: CGRect(x: Double(side) * random.next() - r, y: Double(side) * random.next() - r, width: 2 * r, height: 2 * r))
    }
    let pixels = ctx.data!.bindMemory(to: UInt8.self, capacity: ctx.bytesPerRow * side)
    let amplitude = 96 * detail
    for y in 0..<side {
        for x in 0..<side {
            let i = y * ctx.bytesPerRow + x * 4
            for c in 0..<3 {
                let v = Double(pixels[i + c]) + amplitude * (random.next() - 0.5)
                pixels[i + c] = UInt8(max(0, min(255, v)))
            }
        }
    }
    return ctx.makeImage()!
}

func jpegDataURI(_ image: CGImage) -> String {
    let data = NSMutableData()
    let destination = CGImageDestinationCreateWithData(data as CFMutableData, "public.jpeg" as CFString, 1, nil)!
    CGImageDestinationAddImage(destination, image, [kCGImageDestinationLossyCompressionQuality: 0.85] as CFDictionary)
    CGImageDestinationFinalize(destination)
    return "data:image/jpg;base64," + (data as Data).base64EncodedString()
}

func reduced(_ image: CGImage, ratio: Double) -> CGImage {
    let width = Int(floor(Double(image.width) / ratio))
    let height = Int(floor(Double(image.height) / ratio))
    let ctx = context(width, height)
    ctx.draw(image, in: CGRect(x: 0, y: 0, width: width, height: height))
    return ctx.makeImage()!
}

let args = CommandLine.arguments
guard args.count == 4 || args.count == 5, let seed = UInt64(args[1]), let detail = Double(args[2]), let side = Int(args.count == 5 ? args[4] : "1024") else {
    print("usage: avatar <seed> <detail 0...1> <output file> [side in pixels, default 1024]")
    exit(1)
}
var generator = Random(state: seed)
var image = picture(side: side, detail: detail, random: &generator)
var uri = jpegDataURI(image)
var steps = 0
while uri.count > maxDataSize {
    image = reduced(image, ratio: min((Double(uri.count) / Double(maxDataSize)).squareRoot(), 2))
    uri = jpegDataURI(image)
    steps += 1
}
try uri.write(toFile: args[3], atomically: true, encoding: .utf8)
print("\(args[3]): \(image.width)x\(image.height) px after \(steps) steps, \(uri.count) characters")
