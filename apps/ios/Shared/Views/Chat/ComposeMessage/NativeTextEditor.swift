//
//  NativeTextEditor.swift
//  SimpleX (iOS)
//
//  Created by Avently on 15.12.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SwiftyGif
import SimpleXChat
import PhotosUI

struct NativeTextEditor: UIViewRepresentable {
    @Binding var text: String
    @Binding var disableEditing: Bool
    let height: CGFloat
    let font: UIFont
    @Binding var focused: Bool
    let alignment: TextAlignment
    let onImagesAdded: ([UploadContent]) -> Void
    
    func makeUIView(context: Context) -> UITextView {
        let field = CustomUITextField()
        field.text = text
        field.font = font
        field.textAlignment = alignment == .leading ? .left : .right
        field.autocapitalizationType = .sentences
        field.setOnTextChangedListener { newText, images in
            if !disableEditing {
                text = newText
            } else {
                field.text = text
            }
            if !images.isEmpty {
                onImagesAdded(images)
            }
        }
        field.setOnFocusChangedListener { focused = $0 }
        field.delegate = field
        field.textContainerInset = UIEdgeInsets(top: 8, left: 5, bottom: 6, right: 4)
        return field
    }
    
    func updateUIView(_ field: UITextView, context: Context) {
        field.text = text
        field.font = font
        field.textAlignment = alignment == .leading ? .left : .right
    }
}

private class CustomUITextField: UITextView, UITextViewDelegate {
    var onTextChanged: (String, [UploadContent]) -> Void = { newText, image in }
    var onFocusChanged: (Bool) -> Void = { focused in }
    
    func setOnTextChangedListener(onTextChanged: @escaping (String, [UploadContent]) -> Void) {
        self.onTextChanged = onTextChanged
    }
    
    func setOnFocusChangedListener(onFocusChanged: @escaping (Bool) -> Void) {
        self.onFocusChanged = onFocusChanged
    }

    func textView(_ textView: UITextView, editMenuForTextIn range: NSRange, suggestedActions: [UIMenuElement]) -> UIMenu? {
        if !UIPasteboard.general.hasImages { return UIMenu(children: suggestedActions)}
        return UIMenu(children: suggestedActions.map { elem in
            if let elem = elem as? UIMenu {
                var actions = elem.children
                // Replacing Paste action since it allows to paste animated images too
                let pasteIndex = elem.children.firstIndex { elem in elem.debugDescription.contains("Action: paste:")}
                if let pasteIndex = pasteIndex {
                    let paste = actions[pasteIndex]
                    actions.remove(at: pasteIndex)
                    let newPaste = UIAction(title: paste.title, image: paste.image) { action in
                        var images: [UploadContent] = []
                        var totalImages = 0
                        var processed = 0
                        UIPasteboard.general.itemProviders.forEach { p in
                            if p.hasItemConformingToTypeIdentifier(UTType.data.identifier) {
                                totalImages += 1
                                p.loadFileRepresentation(forTypeIdentifier: UTType.data.identifier) { url, error in
                                    processed += 1
                                    if let url = url, let image = UploadContent.loadFromURL(url: url) {
                                        images.append(image)
                                        DispatchQueue.main.sync {
                                            self.onTextChanged(textView.text, images)
                                        }
                                    }
                                    // No images were added, just paste a text then
                                    if processed == totalImages && images.isEmpty {
                                        textView.paste(UIPasteboard.general.string)
                                    }
                                }
                            }
                        }
                    }
                    actions.insert(newPaste, at: 0)
                }
                return UIMenu(title: elem.title, subtitle: elem.subtitle, image: elem.image, identifier: elem.identifier, options: elem.options, children: actions)
            } else {
                return elem
            }
        })
    }

    func textViewDidChange(_ textView: UITextView) {
        if textView.markedTextRange == nil {
            var images: [UploadContent] = []
            var rangeDiff = 0
            let newAttributedText = NSMutableAttributedString(attributedString: textView.attributedText)
            textView.attributedText.enumerateAttribute(
                NSAttributedString.Key.attachment,
                in: NSRange(location: 0, length: textView.attributedText.length),
                options: [],
                using: { value, range, _ in
                    if let attachment = (value as? NSTextAttachment)?.fileWrapper?.regularFileContents {
                        do {
                            images.append(.animatedImage(image: try UIImage(gifData: attachment)))
                        } catch {
                            if let img = (value as? NSTextAttachment)?.image {
                                images.append(.simpleImage(image: img))
                            }
                        }
                        newAttributedText.replaceCharacters(in: NSMakeRange(range.location - rangeDiff, range.length), with: "")
                        rangeDiff += range.length
                    }
                }
            )
            if textView.attributedText != newAttributedText {
                textView.attributedText = newAttributedText
            }
            onTextChanged(textView.text, images)
        }
    }
    
    func textViewDidBeginEditing(_ textView: UITextView) {
        onFocusChanged(true)
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        onFocusChanged(false)
    }
}

struct NativeTextEditor_Previews: PreviewProvider{
    static var previews: some View {
        return NativeTextEditor(
            text: Binding.constant("Hello, world!"),
            disableEditing: Binding.constant(false),
            height: 100,
            font: UIFont.preferredFont(forTextStyle: .body),
            focused: Binding.constant(false),
            alignment: TextAlignment.leading,
            onImagesAdded: { _ in }
        )
    }
}
