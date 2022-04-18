//
//  CIImageView.swift
//  SimpleX
//
//  Created by JRoberts on 12/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct CIImageView: View {
    @Environment(\.colorScheme) var colorScheme
    let image: String
    let file: CIFile?
    let maxWidth: CGFloat
    @Binding var imgWidth: CGFloat?

    var body: some View {
        VStack(alignment: .center, spacing: 6) {
            if let file = file,
               let savedFile = file.filePath,
               let filePath = getAppFilesDirectory().path + "/" + savedFile,
               file.stored, // TODO more advanced approach would be to send progressive jpeg and only check for filepath
               let uiImage = UIImage(contentsOfFile: filePath) {
                imageView(uiImage)
            } else if let data = Data(base64Encoded: dropImagePrefix(image)),
              let uiImage = UIImage(data: data) {
                imageView(uiImage)
            }
        }
    }

    private func imageView(_ img: UIImage) -> some View {
        let w = img.size.width > img.size.height ? .infinity : maxWidth * 0.75
        DispatchQueue.main.async { imgWidth = w }
        return Image(uiImage: img)
            .resizable()
            .scaledToFit()
            .frame(maxWidth: w)
    }
}
