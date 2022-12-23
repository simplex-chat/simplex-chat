//
// Created by Avently on 19.12.2022.
// Copyright (c) 2022 SimpleX Chat. All rights reserved.
//

import UIKit
import SwiftUI

class AnimatedImageView: UIView {
    var image: UIImage? = nil
    var imageView: UIImageView? = nil

    override init(frame: CGRect) {
        super.init(frame: frame)
    }

    required init?(coder: NSCoder) {
        fatalError("Not implemented")
    }

    convenience init(image: UIImage) {
        self.init()
        self.image = image
        imageView = UIImageView(gifImage: image)
        imageView!.contentMode = .scaleAspectFit
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
                imageView!.contentMode = .scaleAspectFit
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

    init(image: UIImage) {
        self.image = image
    }

    func makeUIView(context: Context) -> AnimatedImageView {
        AnimatedImageView(image: image)
    }

    func updateUIView(_ imageView: AnimatedImageView, context: Context) {
        imageView.updateImage(image)
        imageView.imageView!.startAnimatingGif()
    }
}
