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
    var didFinishPicking: (_ didSelectImage: Bool) async -> Void
    @State var mediaAdded = false

    var body: some View {
        LibraryMediaListPicker(addMedia: addMedia, selectionLimit: 1, didFinishPicking: didFinishPicking)
    }

    private func addMedia(_ content: UploadContent) async {
        if mediaAdded { return }
        await MainActor.run {
            mediaAdded = true
            image = content.uiImage
        }
    }
}

struct LibraryMediaListPicker: UIViewControllerRepresentable {
    typealias UIViewControllerType = PHPickerViewController
    var addMedia: (_ content: UploadContent) async -> Void
    var selectionLimit: Int
    var finishedPreprocessing: () -> Void = {}
    var didFinishPicking: (_ didSelectItems: Bool) async -> Void

    class Coordinator: PHPickerViewControllerDelegate {
        let parent: LibraryMediaListPicker
        let dispatchQueue = DispatchQueue(label: "chat.simplex.app.LibraryMediaListPicker")

        init(_ parent: LibraryMediaListPicker) {
            self.parent = parent
        }

        func picker(_ picker: PHPickerViewController, didFinishPicking results: [PHPickerResult]) {
            Task {
                await parent.didFinishPicking(!results.isEmpty)
                if results.isEmpty { return }
                for r in results {
                    await loadItem(r.itemProvider)
                }
                parent.finishedPreprocessing()
            }
        }

        private func loadItem(_ p: NSItemProvider) async {
            logger.debug("LibraryMediaListPicker result")
            if p.hasItemConformingToTypeIdentifier(UTType.movie.identifier) {
                if let video = await loadVideo(p) {
                    await self.parent.addMedia(video)
                    logger.debug("LibraryMediaListPicker: added video")
                }
            } else if p.hasItemConformingToTypeIdentifier(UTType.data.identifier) {
                if let img = await loadImageData(p) {
                    await self.parent.addMedia(img)
                    logger.debug("LibraryMediaListPicker: added image")
                }
            } else if p.canLoadObject(ofClass: UIImage.self) {
                if let img = await loadImage(p) {
                    await self.parent.addMedia(.simpleImage(image: img))
                    logger.debug("LibraryMediaListPicker: added image")
                }
            }
        }

        private func loadImageData(_ p: NSItemProvider) async -> UploadContent? {
            await withCheckedContinuation { cont in
                loadFileURL(p, type: UTType.data) { url in
                    if let url = url {
                        let img = UploadContent.loadFromURL(url: url)
                        cont.resume(returning: img)
                    } else {
                        cont.resume(returning: nil)
                    }
                }
            }
        }

        private func loadImage(_ p: NSItemProvider) async -> UIImage? {
            await withCheckedContinuation { cont in
                p.loadObject(ofClass: UIImage.self)  { obj, err in
                    if let err = err {
                        logger.error("LibraryMediaListPicker result image error: \(err.localizedDescription)")
                        cont.resume(returning: nil)
                    } else  {
                        cont.resume(returning: obj as? UIImage)
                    }
                }
            }
        }

        private func loadVideo(_ p: NSItemProvider) async -> UploadContent? {
            await withCheckedContinuation { cont in
                loadFileURL(p, type: UTType.movie) { url in
                    if let url = url  {
                        let tempUrl = URL(fileURLWithPath: generateNewFileName(getTempFilesDirectory().path + "/" + "rawvideo", url.pathExtension, fullPath: true))
                        let convertedVideoUrl = URL(fileURLWithPath: generateNewFileName(getTempFilesDirectory().path + "/" + "video", "mp4", fullPath: true))
                        do {
//                          logger.debug("LibraryMediaListPicker copyItem \(url) to \(tempUrl)")
                            try FileManager.default.copyItem(at: url, to: tempUrl)
                        } catch let err {
                            logger.error("LibraryMediaListPicker copyItem error: \(err.localizedDescription)")
                            return cont.resume(returning: nil)
                        }
                        Task {
                            let success = await makeVideoQualityLower(tempUrl, outputUrl: convertedVideoUrl)
                            try? FileManager.default.removeItem(at: tempUrl)
                            if success {
                                _ = ChatModel.shared.filesToDelete.insert(convertedVideoUrl)
                                let video = UploadContent.loadVideoFromURL(url: convertedVideoUrl)
                                return cont.resume(returning: video)
                            }
                            try? FileManager.default.removeItem(at: convertedVideoUrl)
                            cont.resume(returning: nil)
                        }
                    }
                }
            }
        }

        private func loadFileURL(_ p: NSItemProvider, type: UTType, completion: @escaping (URL?) -> Void) {
            p.loadFileRepresentation(forTypeIdentifier: type.identifier) { url, err in
                if let err = err {
                    logger.error("LibraryMediaListPicker loadFileURL error: \(err.localizedDescription)")
                    completion(nil)
                } else {
                    completion(url)
                }
            }
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeUIViewController(context: Context) -> PHPickerViewController {
        var config = PHPickerConfiguration()
        config.filter = .any(of: [.images, .videos])
        config.selectionLimit = selectionLimit
        config.selection = .ordered
        config.preferredAssetRepresentationMode = .current
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
