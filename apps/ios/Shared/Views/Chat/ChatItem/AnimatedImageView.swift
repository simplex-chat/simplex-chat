//
// Created by Avently on 19.12.2022.
// Copyright (c) 2022 SimpleX Chat. All rights reserved.
//

import UIKit
import SwiftUI

class AnimatedImageView: UIView {
    var image: UIImage? = nil
    var imageView: UIImageView? = nil
    var cMode: UIView.ContentMode = .scaleAspectFit

    override init(frame: CGRect) {
        super.init(frame: frame)
    }

    required init?(coder: NSCoder) {
        fatalError("Not implemented")
    }

    convenience init(image: UIImage, contentMode: UIView.ContentMode) {
        self.init()
        self.image = image
        self.cMode = contentMode
        imageView = UIImageView(gifImage: image)
        imageView!.contentMode = contentMode
        self.addSubview(imageView!)
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        imageView!.frame = bounds
    }

    func updateImage(_ image: UIImage) {
        if let subview = self.subviews.first as? UIImageView {
            if image.imageData != subview.gifImage?.imageData {
                imageView = UIImageView(gifImage: image)
                imageView!.contentMode = contentMode
                self.addSubview(imageView!)
                subview.removeFromSuperview()
            }
        }
        imageView!.frame = bounds
        self.layoutSubviews()
    }
}

struct SwiftyGif: UIViewRepresentable {
    private let image: UIImage
    private let contentMode: UIView.ContentMode

    init(image: UIImage, contentMode: UIView.ContentMode = .scaleAspectFit) {
        self.image = image
        self.contentMode = contentMode
    }

    func makeUIView(context: Context) -> AnimatedImageView {
        AnimatedImageView(image: image, contentMode: contentMode)
    }

    func updateUIView(_ imageView: AnimatedImageView, context: Context) {
        imageView.updateImage(image)
        imageView.imageView!.startAnimatingGif()
    }
}
