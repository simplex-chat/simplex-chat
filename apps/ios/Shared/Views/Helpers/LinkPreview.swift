//
//  LinkPreview.swift
//  SimpleX
//
//  Created by Ian Davies on 04/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import LinkPresentation

// Struct to use with simplex API
struct LinkMetadata: Codable {
    var url: URL?
    var title: String?
    var description: String
    var icon: String?
    var image: String?
}

// TODO caching
func getMetaDataForURL(url: URL) -> LPLinkMetadata? {
    let metadataProvider = LPMetadataProvider()
    var collectedMetadata: LPLinkMetadata? = nil
    metadataProvider.startFetchingMetadata(for: url){ metadata, error in
        if error != nil {
            return
        }
        collectedMetadata = metadata
    }
    return collectedMetadata
}

func encodeLinkMetadataForAPI(metadata: LPLinkMetadata) -> LinkMetadata {
    var icon: UIImage? = nil
    var image: UIImage? = nil

    if let iconProvider = metadata.iconProvider {
        if iconProvider.canLoadObject(ofClass: UIImage.self) {
            iconProvider.loadObject(ofClass: UIImage.self) { object, error in
                DispatchQueue.main.async {
                    if let error = error {
                        logger.error("Couldn't load icon from link metadata with error: \(error.localizedDescription)")
                    }
                    icon = object as? UIImage
                }
            }
        }
    }

    if let imageProvider = metadata.imageProvider {
        if imageProvider.canLoadObject(ofClass: UIImage.self) {
            imageProvider.loadObject(ofClass: UIImage.self) { object, error in
                DispatchQueue.main.async {
                    if let error = error {
                        logger.error("Couldn't load image preview from link metadata with error: \(error.localizedDescription)")
                    }
                    image = object as? UIImage
                }
            }
        }
    }

    return LinkMetadata(
        url: metadata.url,
        title: metadata.title,
        description: metadata.description,
        icon: resizeAndCompressImage(image: icon),
        image: resizeAndCompressImage(image: image)
    )
}

func encodeLinkMetadataForPresentation(metadata: LinkMetadata) -> LPLinkMetadata {
    // TODO
    return LPLinkMetadata()
}
