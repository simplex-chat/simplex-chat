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
    var cancelImage: (() -> Void)? = nil

    var body: some View {
        HStack(alignment: .center, spacing: 8) {
            if let data = Data(base64Encoded: dropImagePrefix(image)),
               let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage)
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(maxWidth: 80, maxHeight: 60)
            }
            if let cancelImage = cancelImage {
                Button { cancelImage() } label: {
                    Image(systemName: "multiply")
                }
            }
        }
        .padding(.vertical, 1)
        .padding(.trailing, 12)
        .background(colorScheme == .light ? sentColorLight : sentColorDark)
        .frame(maxWidth: .infinity)
        .padding(.top, 8)
    }
}
