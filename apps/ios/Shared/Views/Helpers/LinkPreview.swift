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
    var image: UIImage? = nil

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
        image: resizeAndCompressImage(image: image)
    )
}

struct LinkPreview: View {
    let link: String

    var body: some View {
        if let url = URL(string: link),
           let metadata = getMetaDataForURL(url: url) {
            let previewData = encodeLinkMetadataForAPI(metadata: metadata)
            if let image = previewData.image,
               let data = Data(base64Encoded: dropImagePrefix(image)),
               let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage)
                    .resizable()
            } else {
                if let title = metadata.title {
                    Text(title)
                }
                else {
                    Text("")
                }
                Text(metadata.description)
            }
        }
    }
}
