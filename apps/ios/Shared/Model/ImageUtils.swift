//
//  ImageUtils.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat
import SwiftUI

func getLoadedFilePath(_ file: CIFile?) -> String? {
    if let fileName = getLoadedFileName(file) {
        return getAppFilePath(fileName).path
    }
    return nil
}

func getLoadedFileName(_ file: CIFile?) -> String? {
    if let file = file,
       file.loaded,
       let fileName = file.filePath {
        return fileName
    }
    return nil
}

func getLoadedImage(_ file: CIFile?) -> UIImage? {
    let loadedFilePath = getLoadedFilePath(file)
    if let loadedFilePath = loadedFilePath, let fileName = file?.filePath {
        let filePath = getAppFilePath(fileName)
        do {
            let data = try Data(contentsOf: filePath)
            let img = UIImage(data: data)
            try img?.setGifFromData(data, levelOfIntegrity: 1.0)
            return img
        } catch {
            return UIImage(contentsOfFile: loadedFilePath)
        }
    }
    return nil
}

func saveAnimImage(_ image: UIImage) -> String? {
    let fileName = generateNewFileName("IMG", "gif")
    guard let imageData = image.imageData else { return nil }
    return saveFile(imageData, fileName)
}

func saveImage(_ uiImage: UIImage) -> String? {
    if let imageDataResized = resizeImageToDataSize(uiImage, maxDataSize: MAX_IMAGE_SIZE) {
        let ext = imageHasAlpha(uiImage) ? "png" : "jpg"
        let fileName = generateNewFileName("IMG", ext)
        return saveFile(imageDataResized, fileName)
    }
    return nil
}

func cropToSquare(_ image: UIImage) -> UIImage {
    let size = image.size
    let side = min(size.width, size.height)
    let newSize = CGSize(width: side, height: side)
    var origin = CGPoint.zero
    if size.width > side {
        origin.x -= (size.width - side) / 2
    } else if size.height > side {
        origin.y -= (size.height - side) / 2
    }
    return resizeImage(image, newBounds: CGRect(origin: .zero, size: newSize), drawIn: CGRect(origin: origin, size: size))
}

func resizeImageToDataSize(_ image: UIImage, maxDataSize: Int64) -> Data? {
    var img = image
    let usePng = imageHasAlpha(image)
    var data = usePng ? img.pngData() : img.jpegData(compressionQuality: 0.85)
    var dataSize = data?.count ?? 0
    while dataSize != 0 && dataSize > maxDataSize {
        let ratio = sqrt(Double(dataSize) / Double(maxDataSize))
        let clippedRatio = min(ratio, 2.0)
        img = reduceSize(img, ratio: clippedRatio)
        data = usePng ? img.pngData() : img.jpegData(compressionQuality: 0.85)
        dataSize = data?.count ?? 0
    }
    logger.debug("resizeImageToDataSize final \(dataSize)")
    return data
}

func resizeImageToStrSize(_ image: UIImage, maxDataSize: Int64) -> String? {
    var img = image
    var str = compressImageStr(img)
    var dataSize = str?.count ?? 0
    while dataSize != 0 && dataSize > maxDataSize {
        let ratio = sqrt(Double(dataSize) / Double(maxDataSize))
        let clippedRatio = min(ratio, 2.0)
        img = reduceSize(img, ratio: clippedRatio)
        str = compressImageStr(img)
        dataSize = str?.count ?? 0
    }
    logger.debug("resizeImageToStrSize final \(dataSize)")
    return str
}

func compressImageStr(_ image: UIImage, _ compressionQuality: CGFloat = 0.85) -> String? {
    let ext = imageHasAlpha(image) ? "png" : "jpg"
    if let data = imageHasAlpha(image) ? image.pngData() : image.jpegData(compressionQuality: compressionQuality) {
        return "data:image/\(ext);base64,\(data.base64EncodedString())"
    }
    return nil
}

private func reduceSize(_ image: UIImage, ratio: CGFloat) -> UIImage {
    let newSize = CGSize(width: floor(image.size.width / ratio), height: floor(image.size.height / ratio))
    let bounds = CGRect(origin: .zero, size: newSize)
    return resizeImage(image, newBounds: bounds, drawIn: bounds)
}

private func resizeImage(_ image: UIImage, newBounds: CGRect, drawIn: CGRect) -> UIImage {
    let format = UIGraphicsImageRendererFormat()
    format.scale = 1.0
    format.opaque = !imageHasAlpha(image)
    return UIGraphicsImageRenderer(bounds: newBounds, format: format).image { _ in
        image.draw(in: drawIn)
    }
}

func imageHasAlpha(_ img: UIImage) -> Bool {
    let alpha = img.cgImage?.alphaInfo
    return alpha == .first || alpha == .last || alpha == .premultipliedFirst || alpha == .premultipliedLast || alpha == .alphaOnly
}

func saveFileFromURL(_ url: URL) -> String? {
    let savedFile: String?
    if url.startAccessingSecurityScopedResource() {
        do {
            let fileData = try Data(contentsOf: url)
            let fileName = uniqueCombine(url.lastPathComponent)
            savedFile = saveFile(fileData, fileName)
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

func generateNewFileName(_ prefix: String, _ ext: String) -> String {
    let fileName = uniqueCombine("\(prefix)_\(getTimestamp()).\(ext)")
    return fileName
}

private func uniqueCombine(_ fileName: String) -> String {
    func tryCombine(_ fileName: String, _ n: Int) -> String {
        let ns = fileName as NSString
        let name = ns.deletingPathExtension
        let ext = ns.pathExtension
        let suffix = (n == 0) ? "" : "_\(n)"
        let f = "\(name)\(suffix).\(ext)"
        return (FileManager.default.fileExists(atPath: getAppFilePath(f).path)) ? tryCombine(fileName, n + 1) : f
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
        tsFormatter = df
    }
    return df.string(from: Date())
}

func dropImagePrefix(_ s: String) -> String {
    dropPrefix(dropPrefix(s, "data:image/png;base64,"), "data:image/jpg;base64,")
}

private func dropPrefix(_ s: String, _ prefix: String) -> String {
    s.hasPrefix(prefix) ? String(s.dropFirst(prefix.count)) : s
}
