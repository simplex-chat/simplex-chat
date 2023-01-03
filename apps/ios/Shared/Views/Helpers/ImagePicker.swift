//
//  ImagePicker.swift
//  SimpleX
//
//  Created by Evgeny on 23/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import PhotosUI
import SwiftyGif
import SimpleXChat

struct LibraryImagePicker: View {
    @Binding var image: UIImage?
    var didFinishPicking: (_ didSelectItems: Bool) -> Void
    @State var images: [UploadContent] = []

    var body: some View {
        LibraryImageListPicker(images: $images, selectionLimit: 1, didFinishPicking: didFinishPicking)
            .onChange(of: images) { _ in
                if let img = images.first {
                    image = img.uiImage
                }
            }
    }
}

struct LibraryImageListPicker: UIViewControllerRepresentable {
    typealias UIViewControllerType = PHPickerViewController
    @Binding var images: [UploadContent]
    var selectionLimit: Int
    var didFinishPicking: (_ didSelectItems: Bool) -> Void

    class Coordinator: PHPickerViewControllerDelegate {
        let parent: LibraryImageListPicker
        let dispatchQueue = DispatchQueue(label: "chat.simplex.app.LibraryImageListPicker")
        var images: [UploadContent] = []
        var imageCount: Int = 0

        init(_ parent: LibraryImageListPicker) {
            self.parent = parent
        }

        func picker(_ picker: PHPickerViewController, didFinishPicking results: [PHPickerResult]) {
            parent.didFinishPicking(!results.isEmpty)
            guard !results.isEmpty else {
                return
            }

            parent.images = []
            images = []
            imageCount = results.count
            for result in results {
                logger.log("LibraryImageListPicker result")
                let p = result.itemProvider
                if p.hasItemConformingToTypeIdentifier(UTType.data.identifier) {
                    p.loadFileRepresentation(forTypeIdentifier: UTType.data.identifier) { url, error in
                        self.loadImage(object: url, error: error)
                    }
                } else if p.canLoadObject(ofClass: UIImage.self) {
                    p.loadObject(ofClass: UIImage.self)  { image, error in
                        DispatchQueue.main.async {
                            self.loadImage(object: image, error: error)
                        }
                    }
                } else {
                    dispatchQueue.sync { self.imageCount -= 1}
                }
            }
            DispatchQueue.main.asyncAfter(deadline: .now() + 10) {
                self.dispatchQueue.sync {
                    if self.parent.images.count == 0 {
                        logger.log("LibraryImageListPicker: added \(self.images.count) images out of \(results.count)")
                        self.parent.images = self.images
                    }
                }
            }
        }

        func loadImage(object: Any?, error: Error? = nil) {
            if let error = error {
                logger.error("LibraryImageListPicker: couldn't load image with error: \(error.localizedDescription)")
            } else if let image = object as? UIImage {
                images.append(.simpleImage(image: image))
                logger.log("LibraryImageListPicker: added image")
            } else if let url = object as? URL, let image = UploadContent.loadFromURL(url: url) {
                images.append(image)
            }
            dispatchQueue.sync {
                self.imageCount -= 1
                if self.imageCount == 0 && self.parent.images.count == 0 {
                    logger.log("LibraryImageListPicker: added all images")
                    self.parent.images = self.images
                    self.images = []
                }
            }
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeUIViewController(context: Context) -> PHPickerViewController {
        var config = PHPickerConfiguration()
        config.filter = .images
        config.selectionLimit = selectionLimit
        let controller = PHPickerViewController(configuration: config)
        controller.delegate = context.coordinator
        return controller
    }

    func updateUIViewController(_ uiViewController: PHPickerViewController, context: Context) {

    }
}

struct CameraImageListPicker: View {
    @Binding var images: [UploadContent]
    @State var image: UIImage?

    var body: some View {
        CameraImagePicker(image: $image)
            .onChange(of: image) { img in
                if let img = img {
                    images = [UploadContent.simpleImage(image: img)]
                } else {
                    images = []
                }
            }
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
