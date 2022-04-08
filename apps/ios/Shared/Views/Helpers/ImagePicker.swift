//
//  ImagePicker.swift
//  SimpleX
//
//  Created by Evgeny on 23/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import PhotosUI

func dropPrefix(_ s: String, _ prefix: String) -> String {
    s.hasPrefix(prefix) ? String(s.dropFirst(prefix.count)) : s
}

func dropImagePrefix(_ s: String) -> String {
    dropPrefix(dropPrefix(s, "data:image/png;base64,"), "data:image/jpg;base64,")
}

func resizeAndCrop(_ image: UIImage, to newSize: CGSize) -> UIImage {
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

func cropToSquare(_ image: UIImage) -> UIImage {
    let side = min(image.size.width, image.size.height)
    return resizeAndCrop(image, to: CGSize(width: side, height: side))
}

func resizeImageToDataSize(_ image: UIImage, maxSize: Int) -> String? {
    let size = image.size
    var imageStr = compressImage(image)
    var resized = image
    var ratio: CGFloat = 1
    var dataSize = imageStr?.count ?? 0
    logger.debug("resizeImageToDataSize: initial size \(String(describing: size)), data size \(dataSize)")
    while dataSize != 0 && dataSize > maxSize {
        ratio *= sqrt(CGFloat(dataSize / maxSize) * 1.2)
        resized = resizeAndCrop(resized, to: CGSize(width: size.width / ratio, height: size.height / ratio))
        imageStr = compressImage(resized)
        dataSize = imageStr?.count ?? 0
        logger.debug("resizeImageToDataSize: ratio \(ratio)")
    }
    logger.debug("resizeImageToDataSize: final size \(String(describing: resized.size)), data size \(dataSize)")
    return imageStr
}

func compressImage(_ image: UIImage, _ compressionQuality: CGFloat = 0.85) -> String? {
    if let data = image.jpegData(compressionQuality: compressionQuality) {
        return "data:image/jpg;base64,\(data.base64EncodedString())"
    }
    return nil
}

enum ImageSource {
    case imageLibrary
    case camera
}

struct LibraryImagePicker: UIViewControllerRepresentable {
    typealias UIViewControllerType = PHPickerViewController
    @Binding var image: UIImage?
    var didFinishPicking: (_ didSelectItems: Bool) -> Void

    class Coordinator: PHPickerViewControllerDelegate {
        let parent: LibraryImagePicker

        init(_ parent: LibraryImagePicker) {
            self.parent = parent
        }

        func picker(_ picker: PHPickerViewController, didFinishPicking results: [PHPickerResult]) {
            parent.didFinishPicking(!results.isEmpty)
            guard !results.isEmpty else {
                return
            }

            if let chosenImageProvider = results.first?.itemProvider {
                if chosenImageProvider.canLoadObject(ofClass: UIImage.self) {
                    chosenImageProvider.loadObject(ofClass: UIImage.self)  { [weak self] image, error in
                        DispatchQueue.main.async {
                            self?.loadImage(object: image, error: error)
                        }
                    }
                }
            }
        }

        func loadImage(object: Any?, error: Error? = nil) {
            if let error = error {
                logger.error("Couldn't load image with error: \(error.localizedDescription)")
            }
            parent.image = object as? UIImage
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeUIViewController(context: Context) -> PHPickerViewController {
        var config = PHPickerConfiguration()
        config.filter = .images
        config.selectionLimit = 1
        let controller = PHPickerViewController(configuration: config)
        controller.delegate = context.coordinator
        return controller
    }

    func updateUIViewController(_ uiViewController: PHPickerViewController, context: Context) {

    }
}


struct CameraImagePicker: UIViewControllerRepresentable {
    @Environment(\.presentationMode) var presentationMode
    @Binding var image: UIImage?

    class Coordinator: NSObject, UINavigationControllerDelegate, UIImagePickerControllerDelegate {
        let parent: CameraImagePicker

        init(_ parent: CameraImagePicker) {
            self.parent = parent
        }

        func imagePickerController(_ picker: UIImagePickerController,
                                   didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey: Any]) {
            if let uiImage = info[.originalImage] as? UIImage {
                parent.image = uiImage
            }
            parent.presentationMode.wrappedValue.dismiss()
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeUIViewController(context: UIViewControllerRepresentableContext<CameraImagePicker>) -> UIImagePickerController {
        let picker = UIImagePickerController()
        picker.sourceType = .camera
        picker.allowsEditing = false
        picker.delegate = context.coordinator
        return picker
    }

    func updateUIViewController(_ uiViewController: UIImagePickerController, context: UIViewControllerRepresentableContext<CameraImagePicker>) {

    }
}
