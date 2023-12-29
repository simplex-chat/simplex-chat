//
//  ShareViewController.swift
//  SimpleX Share
//
//  Created by Evgeny on 10/12/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import MobileCoreServices
import OSLog
import Social
import SwiftUI
import UIKit
import UniformTypeIdentifiers
import SimpleXChat

let logger = Logger()

let maxTextLength = 15000

class ShareViewController: SLComposeServiceViewController {
    private var contentIsValid = true
    private var validated = false

    override func viewDidLoad() {
        super.viewDidLoad()
//        setupShareView()
        logger.debug("ShareViewController viewDidLoad")
        if !validated {
            validated = true
            validateShareContent()
        }
    }

    private func setupShareView() {
        let swiftUIView = ShareView()
        let hostingController = UIHostingController(rootView: swiftUIView)

        // Set up the hosting controller's view to fit the available space
        hostingController.view.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(hostingController.view)

        NSLayoutConstraint.activate([
            hostingController.view.topAnchor.constraint(equalTo: view.topAnchor),
            hostingController.view.bottomAnchor.constraint(equalTo: view.bottomAnchor),
            hostingController.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            hostingController.view.trailingAnchor.constraint(equalTo: view.trailingAnchor)
        ])

        addChild(hostingController)
        hostingController.didMove(toParent: self)
    }

    private func validateShareContent() {
        Task {
            guard let shareItem = extensionContext?.inputItems.first as? NSExtensionItem else {
                logger.debug("ShareViewController viewDidLoad: no input items")
                // contentIsValid = false
                return
            }
            logger.debug("ShareViewController viewDidLoad: \(shareItem.attachments?.count ?? 0) attachments")
            for attachment in shareItem.attachments ?? [] {
                logger.debug("ShareViewController viewDidLoad: attachment \(attachment.registeredTypeIdentifiers)")
                let valid = await validateContentItem(attachment)
                contentIsValid = contentIsValid && valid
            }
            await MainActor.run {
                self.validateContent()
            }
        }
    }

    private func validateContentItem(_ p: NSItemProvider) async -> Bool {
        var valid = false
        do {
            if p.hasItemConformingToTypeIdentifier(UTType.movie.identifier) {
                logger.debug("ShareViewController validateContentItem: movie")
                if let url = try await getFileURL(),
                   let fileSize = try? url.resourceValues(forKeys: [.fileSizeKey]).fileSize {
                    logger.debug("ShareViewController validateContentItem: movie file \(fileSize)")
                    valid = fileSize <= MAX_FILE_SIZE_XFTP
                }
            } else if let data = try await loadItem(type: UTType.plainText) {
//                logger.debug("ShareViewController validateContentItem: text")
                if let text = data as? String {
//                    logger.debug("ShareViewController validateContentItem: text \(text.count)")
                    valid = text.utf8.count <= maxTextLength
                }
            } else if let data = try await loadItem(type: UTType.image) {
//                logger.debug("ShareViewController validateContentItem: image")
                if let image = data as? UIImage, let size = image.pngData()?.count {
//                    logger.debug("ShareViewController validateContentItem: image \(size)")
                    valid = size <= MAX_FILE_SIZE_XFTP
                }
            } else if let data = try await loadItem(type: UTType.fileURL) {
//                logger.debug("ShareViewController validateContentItem: file")
                if let url = data as? URL, let fileSize = try? url.resourceValues(forKeys: [.fileSizeKey]).fileSize {
//                    logger.debug("ShareViewController validateContentItem: file \(fileSize)")
                    valid = fileSize <= MAX_FILE_SIZE_XFTP
                }
            } else if let data = try await loadItem(type: UTType.data) {
//                logger.debug("ShareViewController validateContentItem: data")
                if let data = data as? Data {
//                    logger.debug("ShareViewController validateContentItem: data \(data.count)")
                    valid = data.count <= MAX_FILE_SIZE_XFTP
                }
            }
        } catch let error {
            logger.error("ShareViewController validateContentItem: error \(error.localizedDescription)")
        }
        return valid

        func getFileURL() async throws -> URL? {
            try await withCheckedThrowingContinuation { cont in
                p.loadFileRepresentation(forTypeIdentifier: UTType.movie.identifier) { url, error in
                    if let url = url {
                        cont.resume(returning: url)
                    } else if let error = error {
                        cont.resume(throwing: error)
                    } else {
                        cont.resume(returning: nil)
                    }
                }
            }
        }

        func loadItem(type: UTType) async throws -> NSSecureCoding? {
            var item: NSSecureCoding?
            if p.hasItemConformingToTypeIdentifier(type.identifier) {
                logger.debug("ShareViewController validateContentItem: conforming to \(type.identifier)")
                item = try await p.loadItem(forTypeIdentifier: type.identifier)
            }
            return item
        }
    }

    override func isContentValid() -> Bool {
        contentIsValid
    }

    override func didSelectPost() {
        logger.debug("ShareViewController didSelectPost")
        // This is called after the user selects Post. Do the upload of contentText and/or NSExtensionContext attachments.
    
        // Inform the host that we're done, so it un-blocks its UI. Note: Alternatively you could call super's -didSelectPost, which will similarly complete the extension context.
        self.extensionContext!.completeRequest(returningItems: [], completionHandler: nil)
    }
}
