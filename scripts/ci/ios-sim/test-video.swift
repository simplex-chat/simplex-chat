// Makes the video the app plays in place of the camera in the simulator (sounds/video.mp4 in the app bundle):
// H.264 frames, each filled with one color that changes every half second, so a screenshot of the peer's video
// shows which colors arrived and two screenshots a second apart differ.
// Usage: test-video <output file>

import AVFoundation
import CoreVideo
import Foundation

// the apps sent no video at all with 240x320 at 10 fps (run 41280699336)
let width = 480
let height = 640
let framesPerSecond: Int32 = 15
let seconds = 30
let framesPerColor = 7
// BGRA, saturated and far from black, so call-frame.swift tells them from a dark or empty view
let colors: [(b: UInt8, g: UInt8, r: UInt8)] = [
    (0, 0, 255), (0, 255, 0), (255, 0, 0), (0, 255, 255), (255, 0, 255), (255, 255, 0),
]

let output = URL(fileURLWithPath: CommandLine.arguments[1])
try? FileManager.default.removeItem(at: output)
let writer = try AVAssetWriter(outputURL: output, fileType: .mp4)
let input = AVAssetWriterInput(mediaType: .video, outputSettings: [
    AVVideoCodecKey: AVVideoCodecType.h264,
    AVVideoWidthKey: width,
    AVVideoHeightKey: height,
])
input.expectsMediaDataInRealTime = false
let adaptor = AVAssetWriterInputPixelBufferAdaptor(assetWriterInput: input, sourcePixelBufferAttributes: [
    kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA,
    kCVPixelBufferWidthKey as String: width,
    kCVPixelBufferHeightKey as String: height,
])
writer.add(input)
guard writer.startWriting() else { fatalError("cannot write \(output.path): \(String(describing: writer.error))") }
writer.startSession(atSourceTime: .zero)

for frame in 0..<(Int(framesPerSecond) * seconds) {
    while !input.isReadyForMoreMediaData { usleep(1000) }
    var buffer: CVPixelBuffer?
    CVPixelBufferPoolCreatePixelBuffer(nil, adaptor.pixelBufferPool!, &buffer)
    let pixels = buffer!
    CVPixelBufferLockBaseAddress(pixels, [])
    let color = colors[(frame / framesPerColor) % colors.count]
    let rowBytes = CVPixelBufferGetBytesPerRow(pixels)
    let base = CVPixelBufferGetBaseAddress(pixels)!.assumingMemoryBound(to: UInt8.self)
    for y in 0..<height {
        let row = base + y * rowBytes
        for x in 0..<width {
            row[4 * x] = color.b
            row[4 * x + 1] = color.g
            row[4 * x + 2] = color.r
            row[4 * x + 3] = 255
        }
    }
    CVPixelBufferUnlockBaseAddress(pixels, [])
    adaptor.append(pixels, withPresentationTime: CMTime(value: CMTimeValue(frame), timescale: framesPerSecond))
}

input.markAsFinished()
let done = DispatchSemaphore(value: 0)
writer.finishWriting { done.signal() }
done.wait()
guard writer.status == .completed else { fatalError("writing \(output.path) failed: \(String(describing: writer.error))") }
print("wrote \(output.path): \(seconds) s of \(width)x\(height) at \(framesPerSecond) fps")
