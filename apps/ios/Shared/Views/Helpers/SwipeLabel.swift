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

    var body: Image {
        Image(
            uiImage: SwipeActionView(
                systemName: systemImage,
                text: text
            ).snapshot(inverted: inverted)
        )
    }

    private class SwipeActionView: UIView {
        private let imageView = UIImageView()
        private let label = UILabel()

        init(systemName: String, text: String) {
            super.init(frame: CGRect(x: 0, y: 0, width: 64, height: 48))
            imageView.image = UIImage(systemName: systemName)
            imageView.contentMode = .scaleAspectFit
            label.text = NSLocalizedString(text, comment: "swipe action")
            label.textAlignment = .center
            label.font = UIFont.systemFont(ofSize: 16, weight: .medium)
            addSubview(imageView)
            addSubview(label)
        }

        override func layoutSubviews() {
            imageView.frame = CGRect(
                x: 18,
                y: 0,
                width: 28,
                height: 28
            )
            label.frame = CGRect(
                x: 0,
                y: 32,
                width: 64,
                height: 16
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
