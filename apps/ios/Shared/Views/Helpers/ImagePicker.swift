//
//  ImagePicker.swift
//  SimpleX
//
//  Created by Evgeny on 23/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import PhotosUI

struct ImagePicker: UIViewControllerRepresentable {
    typealias UIViewControllerType = PHPickerViewController
    @Environment(\.presentationMode) var presentationMode
    var source: UIImagePickerController.SourceType
    @Binding var image: UIImage?
    @Binding var imageUrl: URL?
    var didFinishPicking: (_ didSelectItems: Bool) -> Void

    class Coordinator: PHPickerViewControllerDelegate {
        let parent: ImagePicker

        init(_ parent: ImagePicker) {
            self.parent = parent
        }

        func picker(_ picker: PHPickerViewController, didFinishPicking results: [PHPickerResult]) {
            parent.didFinishPicking(!results.isEmpty)
            guard !results.isEmpty else {
                return
            }
            // TODO dont force unwrap
            let chosenImageProvider = results.first!.itemProvider
            if chosenImageProvider.canLoadObject(ofClass: UIImage.self) {
                chosenImageProvider.loadObject(ofClass: UIImage.self)  { [weak self] image, error in
                    DispatchQueue.main.async {
                        self?.loadImage(object: image, error: error)
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


//    func makeUIViewController(context: UIViewControllerRepresentableContext<ImagePicker>) -> UIImagePickerController {
//        let picker = UIImagePickerController()
//        picker.sourceType = source
//        picker.allowsEditing = false
//        picker.delegate = context.coordinator
//        return picker
//    }

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
