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
        if let data = Data(base64Encoded: dropImagePrefix(image)),
           let uiImage = UIImage(data: data) {
            HStack(alignment: .center) {
                ZStack(alignment: .topTrailing) {
                    Image(uiImage: uiImage)
                        .resizable()
                        .scaledToFit()
                        .cornerRadius(20)
                        .frame(maxHeight: 200)
                    Button { cancelImage() } label: {
                        Image(systemName: "multiply")
                            .renderingMode(.template)
                            .foregroundColor(.white)
                    }
                    .padding(8)
                }
            }
            .padding(.top, 8)
        }
    }
}
