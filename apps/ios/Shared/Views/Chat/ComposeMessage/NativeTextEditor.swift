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
    @Binding var height: CGFloat
    @Binding var focused: Bool
    @Binding var lastUnfocusedDate: Date
    @Binding var placeholder: String?
    @Binding var selectedRange: NSRange
    let onImagesAdded: ([UploadContent]) -> Void
    
    static let minHeight: CGFloat = 39

    func makeUIView(context: Context) -> CustomUITextField {
        let field = CustomUITextField(parent: self, height: _height)
        field.backgroundColor = .clear
        field.text = text
        field.textAlignment = alignment(text)
        field.autocapitalizationType = .sentences
        field.setOnTextChangedListener { newText, images in
            if !disableEditing {
                text = newText
                field.textAlignment = alignment(text)
                field.updateFont()
                // Speed up the process of updating layout, reduce jumping content on screen
                field.updateHeight()
            } else {
                field.text = text
            }
            if !images.isEmpty {
                onImagesAdded(images)
            }
        }
        field.setOnFocusChangedListener {
            focused = $0
            if !focused {
                lastUnfocusedDate = .now
            }
        }
        field.delegate = field
        field.textContainerInset = UIEdgeInsets(top: 8, left: 5, bottom: 6, right: 4)
        field.setPlaceholderView()
        field.updateFont()
        field.updateHeight(updateBindingNow: false)
        return field
    }
    
    func updateUIView(_ field: CustomUITextField, context: Context) {
        if field.markedTextRange == nil && field.text != text {
            field.text = text
            field.textAlignment = alignment(text)
            field.updateFont()
            field.updateHeight(updateBindingNow: false)
        }
        if field.placeholder != placeholder {
            field.placeholder = placeholder
        }
        if field.selectedRange != selectedRange {
            field.selectedRange = selectedRange
        }
    }
}

private func alignment(_ text: String) -> NSTextAlignment {
    isRightToLeft(text) ? .right : .left
}

class CustomUITextField: UITextView, UITextViewDelegate {
    var parent: NativeTextEditor?
    var height: Binding<CGFloat>
    var newHeight: CGFloat = 0
    var onTextChanged: (String, [UploadContent]) -> Void = { newText, image in }
    var onFocusChanged: (Bool) -> Void = { focused in }

    private let placeholderLabel: UILabel = UILabel()

    init(parent: NativeTextEditor?, height: Binding<CGFloat>) {
        self.parent = parent
        self.height = height
        super.init(frame: .zero, textContainer: nil)
    }

    var placeholder: String? {
        get { placeholderLabel.text }
        set { placeholderLabel.text = newValue }
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

    func updateHeight(updateBindingNow: Bool = true) {
        let maxHeight = min(360, font!.lineHeight * 12)
        let newHeight = min(max(sizeThatFits(CGSizeMake(frame.size.width, CGFloat.greatestFiniteMagnitude)).height, NativeTextEditor.minHeight), maxHeight).rounded(.down)

        if self.newHeight != newHeight {
            frame.size = CGSizeMake(frame.size.width, newHeight)
            invalidateIntrinsicContentHeight(newHeight)
            if updateBindingNow {
                self.height.wrappedValue = newHeight
            } else {
                DispatchQueue.main.async {
                    self.height.wrappedValue = newHeight
                }
            }
        }
    }

    func updateFont() {
        let newFont = isShortEmoji(text)
        ? (text.count < 4 ? largeEmojiUIFont : mediumEmojiUIFont)
        : UIFont.preferredFont(forTextStyle: .body)
        if font != newFont {
            font = newFont
            // force apply new font because it has problem with doing it when the field had two emojis
            if text.count == 0 {
                text = " "
                text = ""
            }
        }
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        updateHeight()
    }

    override var intrinsicContentSize: CGSize {
        CGSizeMake(0, newHeight)
    }

    func setOnTextChangedListener(onTextChanged: @escaping (String, [UploadContent]) -> Void) {
        self.onTextChanged = onTextChanged
    }
    
    func setPlaceholderView() {
        placeholderLabel.textColor = .lightGray
        placeholderLabel.font = UIFont.preferredFont(forTextStyle: .body)
        placeholderLabel.isHidden = !text.isEmpty
        placeholderLabel.translatesAutoresizingMaskIntoConstraints = false
        addSubview(placeholderLabel)
        
        NSLayoutConstraint.activate([
            placeholderLabel.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 7),
            placeholderLabel.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -7),
            placeholderLabel.topAnchor.constraint(equalTo: topAnchor, constant: 8)
        ])
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
        placeholderLabel.isHidden = !text.isEmpty
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
        updateSelectedRange(textView)
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        onFocusChanged(false)
        updateSelectedRange(textView)
    }
    
    func textViewDidChangeSelection(_ textView: UITextView) {
        updateSelectedRange(textView)
    }
    
    private func updateSelectedRange(_ textView: UITextView) {
        if parent?.selectedRange != textView.selectedRange {
            parent?.selectedRange = textView.selectedRange
        }
    }
}

struct NativeTextEditor_Previews: PreviewProvider{
    static var previews: some View {
        NativeTextEditor(
            text: Binding.constant("Hello, world!"),
            disableEditing: Binding.constant(false),
            height: Binding.constant(100),
            focused: Binding.constant(false),
            lastUnfocusedDate: Binding.constant(.now),
            placeholder: Binding.constant("Placeholder"),
            selectedRange: Binding.constant(NSRange(location: 0, length: 0)),
            onImagesAdded: { _ in }
        )
        .fixedSize(horizontal: false, vertical: true)
    }
}
