//
//  ComposeImageView.swift
//  SimpleX
//
//  Created by JRoberts on 11/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ComposeImageView: View {
    @Environment(\.colorScheme) var colorScheme
    let image: String
    let cancelImage: (() -> Void)

    var body: some View {
        HStack(alignment: .center, spacing: 8) {
            if let data = Data(base64Encoded: dropImagePrefix(image)),
               let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage)
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(maxWidth: 80, maxHeight: 60)
            }
            Spacer()
            Button { cancelImage() } label: {
                Image(systemName: "multiply")
            }
        }
        .padding(.vertical, 1)
        .padding(.trailing, 12)
        .background(colorScheme == .light ? sentColorLight : sentColorDark)
        .frame(maxWidth: .infinity)
        .padding(.top, 8)
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
