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
    @Environment(\.colorScheme) var colorScheme
    let metadata: LPLinkMetadata

    var body: some View {
        HStack {
           if let previewData = encodeLinkMetadataForAPI(metadata: metadata),
              let image = previewData.image,
              let data = Data(base64Encoded: dropImagePrefix(image)),
              let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage)
                    .resizable()
            }
            VStack {
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

//struct LinkPreview_Previews: PreviewProvider {
//    static var previews: some View {
//
//    }
//}
