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
