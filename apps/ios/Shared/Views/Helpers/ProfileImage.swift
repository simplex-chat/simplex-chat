//
//  ProfileImage.swift
//  SimpleX
//
//  Created by Evgeny on 23/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let defaultProfileImageCorner = 22.5

struct ProfileImage: View {
    var imageStr: String? = nil
    var iconName: String = "person.crop.circle.fill"
    var size: CGFloat
    var color = Color(uiColor: .tertiarySystemGroupedBackground)
    var backgroundColor: Color? = nil
    @AppStorage(DEFAULT_PROFILE_IMAGE_CORNER_RADIUS) private var radius = defaultProfileImageCorner

    var body: some View {
        if let image = imageStr,
           let data = Data(base64Encoded: dropImagePrefix(image)),
           let uiImage = UIImage(data: data) {
            clipProfileImage(Image(uiImage: uiImage), size: size, radius: radius)
        } else {
            Image(systemName: iconName)
                .resizable()
                .foregroundColor(color)
                .frame(width: size, height: size)
                .background(Circle().fill(backgroundColor != nil ? backgroundColor! : .clear))
        }
    }
}

private let squareToCircleRatio = 0.935

private let radiusFactor = (1 - squareToCircleRatio) / 50

@ViewBuilder func clipProfileImage(_ img: Image, size: CGFloat, radius: Double) -> some View {
    let v = img.resizable()
    if radius >= 50 {
        v.frame(width: size, height: size).clipShape(Circle())
    } else if radius <= 0 {
        let sz = size * squareToCircleRatio
        v.frame(width: sz, height: sz).padding((size - sz) / 2)
    } else {
        let sz = size * (squareToCircleRatio + radius * radiusFactor)
        v.frame(width: sz, height: sz)
        .clipShape(RoundedRectangle(cornerRadius: sz * radius / 100, style: .continuous))
        .padding((size - sz) / 2)
    }
}

struct ProfileImage_Previews: PreviewProvider {
    static var previews: some View {
        ProfileImage(imageStr: "data:image/png;base64,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", size: 63)
            .previewLayout(.fixed(width: 63, height: 63))
            .background(.black)
    }
}
