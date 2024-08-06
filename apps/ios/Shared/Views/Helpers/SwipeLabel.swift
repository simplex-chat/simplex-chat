//
//  SwipeLabel.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 06/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SwipeLabel: View {
    private let text: String
    private let systemImage: String
    private let inverted: Bool

    init(_ text: String, systemImage: String, inverted: Bool) {
        self.text = text
        self.systemImage = systemImage
        self.inverted = inverted
    }

    var body: some View {
        if inverted {
            Image(
                uiImage: SwipeActionView(
                    systemName: systemImage,
                    text: text
                ).snapshot(inverted: inverted)
            )
        } else {
            Label(text, systemImage: systemImage)
        }
    }

    private class SwipeActionView: UIView {
        private let imageView = UIImageView()
        private let label = UILabel()
        private let fontSize: CGFloat

        init(systemName: String, text: String) {
            fontSize = UIFontDescriptor.preferredFontDescriptor(withTextStyle: .body).pointSize
            super.init(frame: CGRect(x: 0, y: 0, width: 64, height: 32 + fontSize))
            imageView.image = UIImage(systemName: systemName)
            imageView.contentMode = .scaleAspectFit
            label.text = NSLocalizedString(text, comment: "swipe action")
            label.textAlignment = .center
            label.font = UIFont.systemFont(ofSize: fontSize, weight: .medium)
            addSubview(imageView)
            addSubview(label)
        }

        override func layoutSubviews() {
            imageView.frame = CGRect(
                x: 18,
                y: 0,
                width: 24,
                height: 24
            )
            label.frame = CGRect(
                x: 0,
                y: 32,
                width: 64,
                height: fontSize
            )
        }

        @available(*, unavailable)
        required init?(coder: NSCoder) { fatalError("not implemented") }

        func snapshot(inverted: Bool) -> UIImage {
            UIGraphicsImageRenderer(bounds: bounds).image { context in
                if inverted {
                    context.cgContext.scaleBy(x: 1, y: -1)
                    context.cgContext.translateBy(x: 0, y: -bounds.height)
                }
                layer.render(in: context.cgContext)
            }.withRenderingMode(.alwaysTemplate)
        }
    }
}
