//
//  LinkPreview.swift
//  SimpleX
//
//  Created by Ian Davies on 04/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import LinkPresentation


func getLinkMetadata(url: URL) async -> LinkPreview? {
    return await withCheckedContinuation { continuation in
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
                    }
                    else {
                        let image = object as? UIImage
                        if let image = image,
                           let title = metadata.title,
                           let uri = metadata.originalURL {
                            linkPreview = LinkPreview(uri: uri, title: title, image: resizeAndCompressImage(image: image, maxHeight: 96))
                        }
                    }
                    continuation.resume(returning: linkPreview)
                }
            }
            else {
                continuation.resume(returning: nil)
            }
        }
    }
}

struct SmallLinkPreviewView: View {
    @Environment(\.colorScheme) var colorScheme
    let metadata: LinkPreview
    var cancelPreview: (() -> Void)? = nil

    var body: some View {
        HStack {
            if let image = metadata.image,
              let data = Data(base64Encoded: dropImagePrefix(image)),
              let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage).frame(maxHeight: 8)
           }
            VStack {
                if let title = metadata.title {
                    Text(title).fontWeight(.bold)
                }
                else {
                    Text("")
                }
                if let url = metadata.uri.absoluteString {
                    Text(url).foregroundColor(.gray)
                }
                else {
                    Text("")
                }
            }
        }.background(.background)
    }
}

//struct LinkPreview_Previews: PreviewProvider {
//    static var previews: some View {
//
//    }
//}
