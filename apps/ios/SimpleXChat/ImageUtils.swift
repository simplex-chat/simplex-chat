//
//  ImageUtils.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import AVKit
import SwiftyGif
import LinkPresentation

public func getLoadedFileSource(_ file: CIFile?) -> CryptoFile? {
    if let file = file, file.loaded {
        return file.fileSource
    }
    return nil
}

public func getLoadedImage(_ file: CIFile?) -> UIImage? {
    if let fileSource = getLoadedFileSource(file) {
        let filePath = getAppFilePath(fileSource.filePath)
        do {
            let data = try getFileData(filePath, fileSource.cryptoArgs)
            let img = UIImage(data: data)
            do {
                try img?.setGifFromData(data, levelOfIntegrity: 1.0)
                return img
            } catch {
                return UIImage(data: data)
            }
        } catch {
            return nil
        }
    }
    return nil
}

public func getFileData(_ path: URL, _ cfArgs: CryptoFileArgs?) throws -> Data {
    if let cfArgs = cfArgs {
        return try readCryptoFile(path: path.path, cryptoArgs: cfArgs)
    } else {
        return try Data(contentsOf: path)
    }
}

public func getLoadedVideo(_ file: CIFile?) -> URL? {
    if let fileSource = getLoadedFileSource(file) {
        let filePath = getAppFilePath(fileSource.filePath)
        if FileManager.default.fileExists(atPath: filePath.path) {
            return filePath
        }
    }
    return nil
}

public func saveAnimImage(_ image: UIImage) -> CryptoFile? {
    let fileName = generateNewFileName("IMG", "gif")
    guard let imageData = image.imageData else { return nil }
    return saveFile(imageData, fileName, encrypted: privacyEncryptLocalFilesGroupDefault.get())
}

public func saveImage(_ uiImage: UIImage) -> CryptoFile? {
    let hasAlpha = imageHasAlpha(uiImage)
    let ext = hasAlpha ? "png" : "jpg"
    if let imageDataResized = resizeImageToDataSize(uiImage, maxDataSize: MAX_IMAGE_SIZE, hasAlpha: hasAlpha) {
        let fileName = generateNewFileName("IMG", ext)
        return saveFile(imageDataResized, fileName, encrypted: privacyEncryptLocalFilesGroupDefault.get())
    }
    return nil
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
    return resizeImage(image, newBounds: CGRect(origin: .zero, size: newSize), drawIn: CGRect(origin: origin, size: size), hasAlpha: imageHasAlpha(image))
}

public func resizeImageToDataSize(_ image: UIImage, maxDataSize: Int64, hasAlpha: Bool) -> Data? {
    var img = image
    var data = hasAlpha ? img.pngData() : img.jpegData(compressionQuality: 0.85)
    var dataSize = data?.count ?? 0
    while dataSize != 0 && dataSize > maxDataSize {
        let ratio = sqrt(Double(dataSize) / Double(maxDataSize))
        let clippedRatio = min(ratio, 2.0)
        img = reduceSize(img, ratio: clippedRatio, hasAlpha: hasAlpha)
        data = hasAlpha ? img.pngData() : img.jpegData(compressionQuality: 0.85)
        dataSize = data?.count ?? 0
    }
    logger.debug("resizeImageToDataSize final \(dataSize)")
    return data
}

public func resizeImageToStrSize(_ image: UIImage, maxDataSize: Int64) -> String? {
    var img = image
    let hasAlpha = imageHasAlpha(image)
    var str = compressImageStr(img, hasAlpha: hasAlpha)
    var dataSize = str?.count ?? 0
    while dataSize != 0 && dataSize > maxDataSize {
        let ratio = sqrt(Double(dataSize) / Double(maxDataSize))
        let clippedRatio = min(ratio, 2.0)
        img = reduceSize(img, ratio: clippedRatio, hasAlpha: hasAlpha)
        str = compressImageStr(img, hasAlpha: hasAlpha)
        dataSize = str?.count ?? 0
    }
    logger.debug("resizeImageToStrSize final \(dataSize)")
    return str
}

public func compressImageStr(_ image: UIImage, _ compressionQuality: CGFloat = 0.85, hasAlpha: Bool) -> String? {
    let ext = hasAlpha ? "png" : "jpg"
    if let data = hasAlpha ? image.pngData() : image.jpegData(compressionQuality: compressionQuality) {
        return "data:image/\(ext);base64,\(data.base64EncodedString())"
    }
    return nil
}

private func reduceSize(_ image: UIImage, ratio: CGFloat, hasAlpha: Bool) -> UIImage {
    let newSize = CGSize(width: floor(image.size.width / ratio), height: floor(image.size.height / ratio))
    let bounds = CGRect(origin: .zero, size: newSize)
    return resizeImage(image, newBounds: bounds, drawIn: bounds, hasAlpha: hasAlpha)
}

private func resizeImage(_ image: UIImage, newBounds: CGRect, drawIn: CGRect, hasAlpha: Bool) -> UIImage {
    let format = UIGraphicsImageRendererFormat()
    format.scale = 1.0
    format.opaque = !hasAlpha
    return UIGraphicsImageRenderer(bounds: newBounds, format: format).image { _ in
        image.draw(in: drawIn)
    }
}

public func imageHasAlpha(_ img: UIImage) -> Bool {
    if let cgImage = img.cgImage {
        let colorSpace = CGColorSpaceCreateDeviceRGB()
        let bitmapInfo = CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedFirst.rawValue)
        if let context = CGContext(data: nil, width: cgImage.width, height: cgImage.height, bitsPerComponent: 8, bytesPerRow: cgImage.width * 4, space: colorSpace, bitmapInfo: bitmapInfo.rawValue) {
            context.draw(cgImage, in: CGRect(x: 0, y: 0, width: cgImage.width, height: cgImage.height))
            if let data = context.data {
                let data = data.assumingMemoryBound(to: UInt8.self)
                let size = cgImage.width * cgImage.height * 4
                var i = 0
                while i < size {
                    if data[i] < 255 { return true }
                    i += 4
                }
            }
        }
    }
    return false
}

/// Reduces image size, while consuming less RAM
///
/// Used by ShareExtension to downsize large images
/// before passing them to regular image processing pipeline
/// to avoid exceeding 120MB memory
///
/// - Parameters:
///   - url: Location of the image data
///   - size: Maximum dimension (width or height)
/// - Returns: Downsampled image or `nil`, if the image can't be located
public func downsampleImage(at url: URL, to size: Int64) -> UIImage? {
    autoreleasepool {
        if let source = CGImageSourceCreateWithURL(url as CFURL, nil) {
            CGImageSourceCreateThumbnailAtIndex(
                    source,
                    0,
                    [
                        kCGImageSourceCreateThumbnailFromImageAlways: true,
                        kCGImageSourceShouldCacheImmediately: true,
                        kCGImageSourceCreateThumbnailWithTransform: true,
                        kCGImageSourceThumbnailMaxPixelSize: String(size) as CFString
                    ] as CFDictionary
                )
            .map { UIImage(cgImage: $0) }
        } else { nil }
    }
}

public func saveFileFromURL(_ url: URL) -> CryptoFile? {
    let encrypted = privacyEncryptLocalFilesGroupDefault.get()
    let savedFile: CryptoFile?
    if url.startAccessingSecurityScopedResource() {
        do {
            let fileName = uniqueCombine(url.lastPathComponent)
            let toPath = getAppFilePath(fileName).path
            if encrypted {
                let cfArgs = try encryptCryptoFile(fromPath: url.path, toPath: toPath)
                savedFile = CryptoFile(filePath: fileName, cryptoArgs: cfArgs)
            } else {
                try FileManager.default.copyItem(atPath: url.path, toPath: toPath)
                savedFile = CryptoFile.plain(fileName)
            }
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

public func moveTempFileFromURL(_ url: URL) -> CryptoFile? {
    do {
        let encrypted = privacyEncryptLocalFilesGroupDefault.get()
        let fileName = uniqueCombine(url.lastPathComponent)
        let savedFile: CryptoFile?
        if encrypted {
            let cfArgs = try encryptCryptoFile(fromPath: url.path, toPath: getAppFilePath(fileName).path)
            try FileManager.default.removeItem(atPath: url.path)
            savedFile = CryptoFile(filePath: fileName, cryptoArgs: cfArgs)
        } else {
            try FileManager.default.moveItem(at: url, to: getAppFilePath(fileName))
            savedFile = CryptoFile.plain(fileName)
        }
        return savedFile
    } catch {
        logger.error("ImageUtils.moveTempFileFromURL error: \(error.localizedDescription)")
        return nil
    }
}

public func saveWallpaperFile(url: URL) -> String? {
    let destFile = URL(fileURLWithPath: generateNewFileName(getWallpaperDirectory().path + "/" + "wallpaper", "jpg", fullPath: true))
    do {
        try FileManager.default.copyItem(atPath: url.path, toPath: destFile.path)
        return destFile.lastPathComponent
    } catch {
        logger.error("FileUtils.saveWallpaperFile error: \(error.localizedDescription)")
        return nil
    }
}

public func saveWallpaperFile(image: UIImage) -> String? {
    let hasAlpha = imageHasAlpha(image)
    let destFile = URL(fileURLWithPath: generateNewFileName(getWallpaperDirectory().path + "/" + "wallpaper", hasAlpha ? "png" : "jpg", fullPath: true))
    let dataResized = resizeImageToDataSize(image, maxDataSize: 5_000_000, hasAlpha: hasAlpha)
    do {
        try dataResized!.write(to: destFile)
        return destFile.lastPathComponent
    } catch {
        logger.error("FileUtils.saveWallpaperFile error: \(error.localizedDescription)")
        return nil
    }
}

public func removeWallpaperFile(fileName: String? = nil) {
    do {
        try FileManager.default.contentsOfDirectory(atPath: getWallpaperDirectory().path).forEach {
            if URL(fileURLWithPath: $0).lastPathComponent == fileName { try FileManager.default.removeItem(atPath: $0) }
        }
    } catch {
        logger.error("FileUtils.removeWallpaperFile error: \(error.localizedDescription)")
    }
    if let fileName {
        WallpaperType.cachedImages.removeValue(forKey: fileName)
    }
}

public func generateNewFileName(_ prefix: String, _ ext: String, fullPath: Bool = false) -> String {
    uniqueCombine("\(prefix)_\(getTimestamp()).\(ext)", fullPath: fullPath)
}

private func uniqueCombine(_ fileName: String, fullPath: Bool = false) -> String {
    func tryCombine(_ fileName: String, _ n: Int) -> String {
        let ns = fileName as NSString
        let name = ns.deletingPathExtension
        let ext = ns.pathExtension
        let suffix = (n == 0) ? "" : "_\(n)"
        let f = "\(name)\(suffix).\(ext)"
        return (FileManager.default.fileExists(atPath: fullPath ? f : getAppFilePath(f).path)) ? tryCombine(fileName, n + 1) : f
    }
    return tryCombine(fileName, 0)
}

private var tsFormatter: DateFormatter?

private func getTimestamp() -> String {
    var df: DateFormatter
    if let tsFormatter = tsFormatter {
        df = tsFormatter
    } else {
        df = DateFormatter()
        df.dateFormat = "yyyyMMdd_HHmmss"
        df.locale = Locale(identifier: "US")
        df.timeZone = TimeZone(secondsFromGMT: 0)
        tsFormatter = df
    }
    return df.string(from: Date())
}

public func dropImagePrefix(_ s: String) -> String {
    dropPrefix(dropPrefix(s, "data:image/png;base64,"), "data:image/jpg;base64,")
}

private func dropPrefix(_ s: String, _ prefix: String) -> String {
    s.hasPrefix(prefix) ? String(s.dropFirst(prefix.count)) : s
}

public func makeVideoQualityLower(_ input: URL, outputUrl: URL) async -> Bool {
    let asset: AVURLAsset = AVURLAsset(url: input, options: nil)
    if let s = AVAssetExportSession(asset: asset, presetName: AVAssetExportPreset640x480) {
        s.outputURL = outputUrl
        s.outputFileType = .mp4
        s.metadataItemFilter = AVMetadataItemFilter.forSharing()
        await s.export()
        if let err = s.error {
            logger.error("Failed to export video with error: \(err)")
        }
        return s.status == .completed
    }
    return false
}

extension AVAsset {
    public func generatePreview() -> (UIImage, Int)? {
        let generator = AVAssetImageGenerator(asset: self)
        generator.appliesPreferredTrackTransform = true
        var actualTime = CMTimeMake(value: 0, timescale: 0)
        if let image = try? generator.copyCGImage(at: CMTimeMakeWithSeconds(0.0, preferredTimescale: 1), actualTime: &actualTime) {
            return (UIImage(cgImage: image), Int(duration.seconds))
        }
        return nil
    }
}

extension UIImage {
    public func replaceColor(_ from: UIColor, _ to: UIColor) -> UIImage {
        if let cgImage = cgImage {
            let colorSpace = CGColorSpaceCreateDeviceRGB()
            let bitmapInfo = CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedFirst.rawValue)
            if let context = CGContext(data: nil, width: cgImage.width, height: cgImage.height, bitsPerComponent: 8, bytesPerRow: cgImage.width * 4, space: colorSpace, bitmapInfo: bitmapInfo.rawValue) {
                context.draw(cgImage, in: CGRect(x: 0, y: 0, width: cgImage.width, height: cgImage.height))
                if let data = context.data {
                    var fromAlpha: CGFloat = 0
                    var fromRed: CGFloat = 0
                    var fromGreen: CGFloat = 0
                    var fromBlue: CGFloat = 0
                    var toAlpha: CGFloat = 0
                    var toRed: CGFloat = 0
                    var toGreen: CGFloat = 0
                    var toBlue: CGFloat = 0
                    from.getRed(&fromRed, green: &fromGreen, blue: &fromBlue, alpha: &fromAlpha)
                    to.getRed(&toRed, green: &toGreen, blue: &toBlue, alpha: &toAlpha)
                    let fAlpha = UInt8(UInt8(fromAlpha * 255))
                    let fRed = UInt8(fromRed * 255)
                    let fGreen = UInt8(fromGreen * 255)
                    let fBlue = UInt8(fromBlue * 255)
                    let tAlpha = UInt8(toAlpha * 255)
                    let tRed = UInt8(toRed * 255)
                    let tGreen = UInt8(toGreen * 255)
                    let tBlue = UInt8(toBlue * 255)
                    let data = data.assumingMemoryBound(to: UInt8.self)
                    let size = cgImage.width * cgImage.height * 4
                    var i = 0
                    while i < size {
                        if data[i] == fAlpha && data[i + 1] == fRed && data[i + 2] == fGreen && data[i + 3] == fBlue {
                            data[i + 0] = tAlpha
                            data[i + 1] = tRed
                            data[i + 2] = tGreen
                            data[i + 3] = tBlue
                        }
                        i += 4
                    }
                }
                if let img = context.makeImage() {
                    return UIImage(cgImage: img)
                }
            }
        }
        return self
    }

    public convenience init?(base64Encoded: String?) {
        if let base64Encoded, let data = Data(base64Encoded: dropImagePrefix(base64Encoded)) {
            self.init(data: data)
        } else {
            return nil
        }
    }
}

public func getLinkPreview(url: URL, cb: @escaping (LinkPreview?) -> Void) {
    logger.debug("getLinkMetadata: fetching URL preview")
    LPMetadataProvider().startFetchingMetadata(for: url){ metadata, error in
        if let e = error {
            logger.error("Error retrieving link metadata: \(e.localizedDescription)")
        }
        if let metadata = metadata,
           let imageProvider = metadata.imageProvider,
           imageProvider.canLoadObject(ofClass: UIImage.self) {
            imageProvider.loadObject(ofClass: UIImage.self){ object, error in
                var linkPreview: LinkPreview? = nil
                if let error = error {
                    logger.error("Couldn't load image preview from link metadata with error: \(error.localizedDescription)")
                } else {
                    if let image = object as? UIImage,
                       let resized = resizeImageToStrSize(image, maxDataSize: 14000),
                       let title = metadata.title,
                       let uri = metadata.originalURL {
                        linkPreview = LinkPreview(uri: uri, title: title, image: resized)
                    }
                }
                cb(linkPreview)
            }
        } else {
            logger.error("Could not load link preview image")
            cb(nil)
        }
    }
}

public func getLinkPreview(for url: URL) async -> LinkPreview? {
    await withCheckedContinuation { cont in
        getLinkPreview(url: url) { cont.resume(returning: $0) }
    }
}

private let squareToCircleRatio = 0.935

private let radiusFactor = (1 - squareToCircleRatio) / 50

@ViewBuilder public func clipProfileImage(_ img: Image, size: CGFloat, radius: Double, blurred: Bool = false) -> some View {
    let v = img.resizable()
    if radius >= 50 {
        v.frame(width: size, height: size)
        .if(blurred) { $0.blur(radius: size / 4) }
        .clipShape(Circle())
    } else if radius <= 0 {
        let sz = size * squareToCircleRatio
        v.frame(width: sz, height: sz)
        .if(blurred) { $0.blur(radius: sz / 4) }
        .padding((size - sz) / 2)
    } else {
        let sz = size * (squareToCircleRatio + radius * radiusFactor)
        v.frame(width: sz, height: sz)
        .if(blurred) { $0.blur(radius: sz / 4) }
        .clipShape(RoundedRectangle(cornerRadius: sz * radius / 100, style: .continuous))
        .padding((size - sz) / 2)
    }
}

public extension View {
    @ViewBuilder func `if`<Content: View>(_ condition: Bool, transform: (Self) -> Content) -> some View {
        if condition {
            transform(self)
        } else {
            self
        }
    }
}
