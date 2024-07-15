//
//  ComposeImageView.swift
//  SimpleX
//
//  Created by JRoberts on 11/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ComposeImageView: View {
    @EnvironmentObject var theme: AppTheme
    let images: [String]
    let cancelImage: (() -> Void)
    let cancelEnabled: Bool

    var body: some View {
        HStack(alignment: .center, spacing: 8) {
            let imgs: [UIImage] = images.compactMap { image in
                UIImage(base64Encoded: image)
            }
            if imgs.count == 0 {
                ProgressView()
                    .padding(.leading, 12)
                    .frame(maxWidth: .infinity, minHeight: 60, maxHeight: 60, alignment: .leading)
            } else {
                ScrollView(.horizontal) {
                    HStack {
                        ForEach(imgs, id: \.hash) { img in
                            Image(uiImage: img)
                                .resizable()
                                .scaledToFit()
                                .frame(maxWidth: 80, minHeight: 40, maxHeight: 60)
                        }
                    }
                }
            }
            Spacer()
            if cancelEnabled {
                Button { cancelImage() } label: {
                    Image(systemName: "multiply")
                }
            }
        }
        .padding(.vertical, 1)
        .padding(.trailing, 12)
        .background(composeContextItemBackground())
        .frame(maxWidth: .infinity)
    }
}

//struct ComposeImageView: View {
//    @Environment(\.colorScheme) var colorScheme
//    let image: String
//    let cancelImage: (() -> Void)
//
//    var body: some View {
//        if let data = Data(base64Encoded: dropImagePrefix(image)),
//           let uiImage = UIImage(data: data) {
//            HStack(alignment: .center) {
//                ZStack(alignment: .topTrailing) {
//                    Image(uiImage: uiImage)
//                        .resizable()
//                        .scaledToFit()
//                        .cornerRadius(20)
//                        .frame(maxHeight: 150)
//                    Button { cancelImage() } label: {
//                        Image(systemName: "multiply")
//                            .foregroundColor(.white)
//                    }
//                    .padding(8)
//                }
//            }
//            .padding(.top, 8)
//        }
//    }
//}
