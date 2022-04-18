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
    @State var showFullScreenImage = false

    var body: some View {
        VStack(alignment: .center, spacing: 6) {
            if let uiImage = getStoredImage(file) {
                imageView(uiImage)
                .fullScreenCover(isPresented: $showFullScreenImage) {
                    ZStack {
                        Color.black.edgesIgnoringSafeArea(.all)
                        Image(uiImage: uiImage)
                            .resizable()
                            .scaledToFit()
                    }
                    .onTapGesture { showFullScreenImage = false }
                }
                .onTapGesture { showFullScreenImage = true }
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
