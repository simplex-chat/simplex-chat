//
//  ImagePicker.swift
//  SimpleX
//
//  Created by Evgeny on 23/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI


func resize(_ image: UIImage, to newSize: CGSize) -> UIImage {
    let format = UIGraphicsImageRendererFormat()
    format.scale = 1.0
    format.opaque = true
    return UIGraphicsImageRenderer(bounds: CGRect(origin: .zero, size: newSize), format: format).image { _ in
        let size = image.size
        let hScale = newSize.height / size.height
        let vScale = newSize.width / size.width
        let scale = max(hScale, vScale) // scaleToFill
        let resizeSize = CGSize(width: size.width * scale, height: size.height * scale)
        var middle = CGPoint.zero
        if resizeSize.width > newSize.width {
            middle.x -= (resizeSize.width - newSize.width) / 2
        } else if resizeSize.height > newSize.height {
            middle.y -= (resizeSize.height - newSize.height) / 2
        }
        image.draw(in: CGRect(origin: middle, size: resizeSize))
    }
}

func resizeToSquare(_ image: UIImage, _ side: CGFloat) -> UIImage {
    resize(image, to: CGSize(width: side, height: side))
}

func resizeAndCompressImage(image: UIImage?, side: CGFloat = 104, compressionQuality: CGFloat = 0.85, maxSize: Int = 12500) -> String? {
    if let image = image,
       let data = resizeToSquare(image, side).jpegData(compressionQuality: compressionQuality) {
        let imageStr = "data:image/jpg;base64,\(data.base64EncodedString())"
        if imageStr.count <= maxSize {
            return imageStr
        } else {
            logger.error("resizeAndCompressImage: resized image is too big \(imageStr.count)")
        }
    }
    return nil
}


struct ImagePicker: UIViewControllerRepresentable {
    @Environment(\.presentationMode) var presentationMode
    var source: UIImagePickerController.SourceType
    @Binding var image: UIImage?
    @Binding var imageUrl: URL?
    
    class Coordinator: NSObject, UINavigationControllerDelegate, UIImagePickerControllerDelegate {
        let parent: ImagePicker
        
        init(_ parent: ImagePicker) {
            self.parent = parent
        }
        
        func imagePickerController(_ picker: UIImagePickerController,
                                   didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey: Any]) {
            if let uiImage = info[.originalImage] as? UIImage {
                parent.imageUrl = info[.imageURL] as? URL
                parent.image = uiImage
            }
            parent.presentationMode.wrappedValue.dismiss()
        }
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }
    

    func makeUIViewController(context: UIViewControllerRepresentableContext<ImagePicker>) -> UIImagePickerController {
        let picker = UIImagePickerController()
        picker.sourceType = source
        picker.allowsEditing = false
        picker.delegate = context.coordinator
        return picker
    }

    func updateUIViewController(_ uiViewController: UIImagePickerController, context: UIViewControllerRepresentableContext<ImagePicker>) {
   
    }
}
