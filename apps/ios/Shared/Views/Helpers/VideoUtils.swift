//
//  VideoUtils.swift
//  SimpleX (iOS)
//
//  Created by Avently on 25.12.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import AVFoundation
import Foundation
import SimpleXChat

func makeVideoQualityLower(_ input: URL, outputUrl: URL) async -> Bool {
    let asset: AVURLAsset = AVURLAsset(url: input, options: nil)
    if let exportSession = AVAssetExportSession(asset: asset, presetName: AVAssetExportPreset1280x720) {
        exportSession.outputURL = outputUrl
        exportSession.outputFileType = .mp4
        await exportSession.export()
        if let err = exportSession.error {
            logger.error("Failed to export video with error: \(err)")
        }
        return exportSession.status == .completed
    }
    return false
}
