//
//  FileUtils.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.04.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

// maximum image file size to be auto-accepted
let maxImageSize: Int64 = 236700

let maxFileSize: Int64 = 8000000

func getDocumentsDirectory() -> URL {
    FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
}

func getAppFilesDirectory() -> URL {
    getDocumentsDirectory().appendingPathComponent("app_files", isDirectory: true)
}

func getAppFilePath(_ fileName: String) -> URL {
    getAppFilesDirectory().appendingPathComponent(fileName)
}

func getStoredFilePath(_ file: CIFile?) -> String? {
    if let file = file,
       file.stored,
       let savedFile = file.filePath {
        return getAppFilePath(savedFile).path
    }
    return nil
}

func getStoredImage(_ file: CIFile?) -> UIImage? {
    if let filePath = getStoredFilePath(file) {
        return UIImage(contentsOfFile: filePath)
    }
    return nil
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

func saveImage(_ uiImage: UIImage) -> String? {
    if let imageDataResized = resizeImageToDataSize(uiImage, maxDataSize: maxImageSize) {
        let timestamp = Date().getFormattedDate("yyyyMMdd_HHmmss")
        let fileName = uniqueCombine("IMG_\(timestamp).jpg")
        return saveFile(imageDataResized, fileName)
    }
    return nil
}

extension Date {
  func getFormattedDate(_ format: String) -> String {
    let df = DateFormatter()
      df.dateFormat = format
      df.locale = Locale(identifier: "US")
    return df.string(from: self)
  }
}

private func saveFile(_ data: Data, _ fileName: String) -> String? {
    let filePath = getAppFilePath(fileName)
    do {
        try data.write(to: filePath)
        return fileName
    } catch {
        logger.error("FileUtils.saveFile error: \(error.localizedDescription)")
        return nil
    }
}

private func uniqueCombine(_ fileName: String) -> String {
    func tryCombine(_ fileName: String, _ n: Int) -> String {
        let name = fileName.deletingPathExtension
        let ext = fileName.pathExtension
        let suffix = (n == 0) ? "" : "_\(n)"
        let f = "\(name)\(suffix).\(ext)"
        return (FileManager.default.fileExists(atPath: getAppFilePath(f).path)) ? tryCombine(fileName, n + 1) : f
    }
    return tryCombine(fileName, 0)
}

private extension String {
    var ns: NSString {
        return self as NSString
    }
    var pathExtension: String {
        return ns.pathExtension
    }
    var deletingPathExtension: String {
        return ns.deletingPathExtension
    }
}

func removeFile(_ fileName: String) {
    do {
        try FileManager.default.removeItem(atPath: getAppFilePath(fileName).path)
    } catch {
        logger.error("FileUtils.removeFile error: \(error.localizedDescription)")
    }
}

// image utils

func dropImagePrefix(_ s: String) -> String {
    dropPrefix(dropPrefix(s, "data:image/png;base64,"), "data:image/jpg;base64,")
}

private func dropPrefix(_ s: String, _ prefix: String) -> String {
    s.hasPrefix(prefix) ? String(s.dropFirst(prefix.count)) : s
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
    var data = img.jpegData(compressionQuality: 0.85)
    var dataSize = data?.count ?? 0
    while dataSize != 0 && dataSize > maxDataSize {
        let ratio = sqrt(Double(dataSize) / Double(maxDataSize))
        let clippedRatio = min(ratio, 2.0)
        img = reduceSize(img, ratio: clippedRatio)
        data = img.jpegData(compressionQuality: 0.85)
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
    if let data = image.jpegData(compressionQuality: compressionQuality) {
        return "data:image/jpg;base64,\(data.base64EncodedString())"
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
    format.opaque = true
    return UIGraphicsImageRenderer(bounds: newBounds, format: format).image { _ in
        image.draw(in: drawIn)
    }
}
