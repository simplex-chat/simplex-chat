//
//  ChatItemImageView.swift
//  SimpleX
//
//  Created by JRoberts on 12/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatItemImageView: View {
    @Environment(\.colorScheme) var colorScheme
    let image: String
    let file: CIFile?

    var body: some View {
        VStack(alignment: .center, spacing: 6) {
            if let file = file,
               let savedFile = file.filePath,
               let filePath = getAppFilesDirectory().path + "/" + savedFile,
               file.stored, // TODO more advanced approach would be to send progressive jpeg and only check for filepath
               let uiImage = UIImage(contentsOfFile: filePath) {
                Image(uiImage: uiImage)
                    .resizable()
                    .scaledToFit()
            } else if let data = Data(base64Encoded: dropImagePrefix(image)),
              let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage)
                    .resizable()
                    .scaledToFit()
            }
        }
    }
}
