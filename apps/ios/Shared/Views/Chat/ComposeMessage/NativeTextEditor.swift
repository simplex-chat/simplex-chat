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

private let defaultAttributes: [NSAttributedString.Key : Any] = [
    .font: UIFont.preferredFont(forTextStyle: .body),
    .foregroundColor: UIColor.label
]

struct NativeTextEditor: UIViewRepresentable {
    @Binding var text: String
    @Binding var disableEditing: Bool
    @Binding var height: CGFloat
    @Binding var focused: Bool
    let onImagesAdded: ([UploadContent]) -> Void
    
    private let minHeight: CGFloat = 37

    private let defaultHeight: CGFloat = {
        let field = CustomUITextField(height: Binding.constant(0))
        field.textContainerInset = UIEdgeInsets(top: 8, left: 5, bottom: 6, right: 4)
        return min(max(field.sizeThatFits(CGSizeMake(field.frame.size.width, CGFloat.greatestFiniteMagnitude)).height, 37), 360).rounded(.down)
    }()
    
    func makeUIView(context: Context) -> UITextView {
        let field = CustomUITextField(height: _height)
        field.backgroundColor = .clear
        field.attributedText = NSAttributedString(
            string: "",
            attributes: [
                .font: UIFont.preferredFont(forTextStyle: .body),
                .foregroundColor: UIColor.label
            ]
        )
        field.textAlignment = alignment(text)
        field.autocapitalizationType = .sentences
// TODO: Reintegrate with attributed string
//        field.setOnTextChangedListener { newText, images in
//            if !disableEditing {
//                text = newText
//                field.textAlignment = alignment(text)
//                updateFont(field)
//                // Speed up the process of updating layout, reduce jumping content on screen
//                updateHeight(field)
//                self.height = field.frame.size.height
//            } else {
//                field.text = text
//            }
//            if !images.isEmpty {
//                onImagesAdded(images)
//            }
//        }
        field.setOnFocusChangedListener { focused = $0 }
        field.delegate = field
        field.textContainerInset = UIEdgeInsets(top: 8, left: 5, bottom: 6, right: 4)
        updateFont(field)
        updateHeight(field)
        return field
    }
    
    func updateUIView(_ field: UITextView, context: Context) {
        if field.markedTextRange == nil && field.text != text {
// TODO: Reintegrate with attributed string
//            field.text = text
            field.textAlignment = alignment(text)
            updateFont(field)
            updateHeight(field)
        }
    }

    private func updateHeight(_ field: UITextView) {
        let maxHeight = min(360, field.font!.lineHeight * 12)
        // When having emoji in text view and then removing it, sizeThatFits shows previous size (too big for empty text view), so using work around with default size
        let newHeight = field.text == ""
        ? defaultHeight
        : min(max(field.sizeThatFits(CGSizeMake(field.frame.size.width, CGFloat.greatestFiniteMagnitude)).height, minHeight), maxHeight).rounded(.down)

        if field.frame.size.height != newHeight {
            field.frame.size = CGSizeMake(field.frame.size.width, newHeight)
            (field as! CustomUITextField).invalidateIntrinsicContentHeight(newHeight)
        }
    }

    private func updateFont(_ field: UITextView) {
        let newFont = isShortEmoji(field.text)
        ? (field.text.count < 4 ? largeEmojiUIFont : mediumEmojiUIFont)
        : UIFont.preferredFont(forTextStyle: .body)
        if field.font != newFont {
            field.font = newFont
        }
    }
}

private func alignment(_ text: String) -> NSTextAlignment {
    isRightToLeft(text) ? .right : .left
}

private class CustomUITextField: UITextView, UITextViewDelegate {
    var height: Binding<CGFloat>
    var newHeight: CGFloat = 0
    var onTextChanged: (String, [UploadContent]) -> Void = { newText, image in }
    var onFocusChanged: (Bool) -> Void = { focused in }
    
    init(height: Binding<CGFloat>) {
        self.height = height
        super.init(frame: .zero, textContainer: nil)
    }

    required init?(coder: NSCoder) {
        fatalError("Not implemented")
    }

    // This func here needed because using frame.size.height in intrinsicContentSize while loading a screen with text (for example. when you have a draft),
    // produces incorrect height because at that point intrinsicContentSize has old value of frame.size.height even if it was set to new value right before the call
    // (who knows why...)
    func invalidateIntrinsicContentHeight(_ newHeight: CGFloat) {
        self.newHeight = newHeight
        invalidateIntrinsicContentSize()
    }

    override var intrinsicContentSize: CGSize {
        if height.wrappedValue != newHeight {
            DispatchQueue.main.asyncAfter(deadline: .now(), execute: { self.height.wrappedValue = self.newHeight })
        }
        return CGSizeMake(0, newHeight)
    }

    func setOnTextChangedListener(onTextChanged: @escaping (String, [UploadContent]) -> Void) {
        self.onTextChanged = onTextChanged
    }

    func setOnFocusChangedListener(onFocusChanged: @escaping (Bool) -> Void) {
        self.onFocusChanged = onFocusChanged
    }

    func textView(_ textView: UITextView, editMenuForTextIn range: NSRange, suggestedActions: [UIMenuElement]) -> UIMenu? {
        let suggestedActions = [formatMenu] + suggestedActions
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
// TODO: Reintegrate with attributed string
//        if textView.markedTextRange == nil {
//            var images: [UploadContent] = []
//            var rangeDiff = 0
//            let newAttributedText = NSMutableAttributedString(attributedString: textView.attributedText)
//            textView.attributedText.enumerateAttribute(
//                NSAttributedString.Key.attachment,
//                in: NSRange(location: 0, length: textView.attributedText.length),
//                options: [],
//                using: { value, range, _ in
//                    if let attachment = (value as? NSTextAttachment)?.fileWrapper?.regularFileContents {
//                        do {
//                            images.append(.animatedImage(image: try UIImage(gifData: attachment)))
//                        } catch {
//                            if let img = (value as? NSTextAttachment)?.image {
//                                images.append(.simpleImage(image: img))
//                            }
//                        }
//                        newAttributedText.replaceCharacters(in: NSMakeRange(range.location - rangeDiff, range.length), with: "")
//                        rangeDiff += range.length
//                    }
//                }
//            )
//            if textView.attributedText != newAttributedText {
//                textView.attributedText = newAttributedText
//            }
//            onTextChanged(textView.text, images)
//        }
    }
    
    func textViewDidBeginEditing(_ textView: UITextView) {
        onFocusChanged(true)
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        onFocusChanged(false)
    }

    // MARK: Formatting
    
    /// Toggles an attribute in the currently selected text range
    /// - Parameters:
    ///   - attribute: Value of the attribute to be enabled or disabled
    ///   - key: Key for which to apply the attribute
    ///   - detect: Block which detects, if the attribute is already present within the selection
    func toggle(
        _ attribute: Any,
        for key: NSAttributedString.Key,
        detect: (Any) -> Bool?
    ) {
        var detected = false
        textStorage.enumerateAttribute(key, in: selectedRange) { value, _, stop in
            if let value, detect(value) == true {
                detected = true
                stop.pointee = true
            }
        }

        if key == .backgroundColor {
            textStorage.removeAttribute(.backgroundColor, range: selectedRange)
        } else {
            textStorage.setAttributes(defaultAttributes, range: selectedRange)
        }

        if !detected {
            textStorage.addAttribute(key, value: attribute, range: selectedRange)
        }
    }

    private var formatMenu: UIMenu {

        func systemImage(_ systemName: String, color: UIColor? = nil) -> UIImage? {
            if let uiImage = UIImage(systemName: systemName) {
                if let color {
                    uiImage.withTintColor(color, renderingMode: .alwaysOriginal)
                } else {
                    uiImage
                }
            } else { nil }
        }

        var bodySize: CGFloat { UIFont.preferredFont(forTextStyle: .body).pointSize }

        
        let colors: [UIColor] = [.red, .green, .blue, .yellow, .cyan, .magenta]

        return UIMenu(
            title: "Format",
            children: [
                UIAction(image: systemImage("bold")) { _ in
                    self.toggle(UIFont.boldSystemFont(ofSize: bodySize), for: .font) { value in
                        (value as? UIFont)?.fontDescriptor.symbolicTraits.contains(.traitBold)
                    }
                },
                UIAction(image: systemImage("italic")) { _ in
                    self.toggle(UIFont.italicSystemFont(ofSize: bodySize), for: .font) { value in
                        (value as? UIFont)?.fontDescriptor.symbolicTraits.contains(.traitItalic)
                    }
                },
                UIAction(image: systemImage("strikethrough")) { _ in
                    self.toggle(NSUnderlineStyle.single.rawValue, for: .strikethroughStyle) { value in
                        (value as? Int).map { $0 == NSUnderlineStyle.single.rawValue }
                    }
                },
                UIAction(title: "Mono") { _ in
                    self.toggle(UIFont.monospacedSystemFont(ofSize: bodySize, weight: .regular), for: .font) { value in
                        (value as? UIFont)?.fontDescriptor.symbolicTraits.contains(.traitMonoSpace)
                    }
                },
                UIMenu(
                    title: "Color",
                    children: colors
                        .map { color in
                            UIAction(image: systemImage("circle.fill", color: color)) { _ in
                                self.toggle(color, for: .foregroundColor) { value in
                                    (value as? UIColor).map { $0 == color }
                                }
                            }
                        }
                ),
                UIAction(title: "Secret") { _ in
                    self.toggle(UIColor.clear, for: .foregroundColor) { value in
                        (value as? UIColor).map { $0 == .clear }
                    }
                    self.toggle(UIColor.secondarySystemFill, for: .backgroundColor) { value in
                        (value as? UIColor).map { $0 == .secondarySystemFill }
                    }
                }
            ]
        )
    }
}

struct NativeTextEditor_Previews: PreviewProvider{
    static var previews: some View {
        NativeTextEditor(
            text: Binding.constant("Hello, world!"),
            disableEditing: Binding.constant(false),
            height: Binding.constant(100),
            focused: Binding.constant(false),
            onImagesAdded: { _ in }
        )
        .fixedSize(horizontal: false, vertical: true)
    }
}

func encodeMarkdown(_ string: String) -> NSAttributedString {
    fatalError()
}
