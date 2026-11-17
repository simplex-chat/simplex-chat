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
            field.placeholder = text.isEmpty ? placeholder : ""
        }
        if field.placeholder != placeholder {
            field.placeholder = text.isEmpty ? placeholder : ""
        }
        if field.selectedRange != selectedRange {
            field.selectedRange = selectedRange
        }
// This block causes delays in closing keyboard when navigating from chat view to chat list.
// It is also a candidate for iOS 26.1 freeze.
// This was added in commit below to open keyboard programmatically via a passed binding but this approach is not reliable.
// https://github.com/simplex-chat/simplex-chat/pull/6003/commits/cb666de51375623451a5e80dcf59449adc7d2a5f
//        if focused && !field.isFocused {
//            DispatchQueue.main.async {
//                if !field.isFocused {
//                    field.becomeFirstResponder()
//                }
//            }
//        }
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
        var actions = suggestedActions

        if UIPasteboard.general.hasImages {
            actions = actions.map { elem in
                if let elem = elem as? UIMenu {
                    var menuActions = elem.children
                    // Replacing Paste action since it allows to paste animated images too
                    let pasteIndex = elem.children.firstIndex { elem in elem.debugDescription.contains("Action: paste:")}
                    if let pasteIndex = pasteIndex {
                        let paste = menuActions[pasteIndex]
                        menuActions.remove(at: pasteIndex)
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
                        menuActions.insert(newPaste, at: 0)
                    }
                    return UIMenu(title: elem.title, subtitle: elem.subtitle, image: elem.image, identifier: elem.identifier, options: elem.options, children: menuActions)
                } else {
                    return elem
                }
            }
        }

        if range.length > 0 {
            let menu = formatMenu(for: range, in: textView)
            var inserted = false

            actions = actions.map { elem in
                guard !inserted, let subMenu = elem as? UIMenu else { return elem }
                var subActions = subMenu.children
                if let targetIndex = subActions.lastIndex(where: { $0.debugDescription.contains("paste:") || $0.debugDescription.contains("copy:") }) {
                    subActions.insert(menu, at: targetIndex + 1)
                    inserted = true
                    return UIMenu(title: subMenu.title, subtitle: subMenu.subtitle, image: subMenu.image, identifier: subMenu.identifier, options: subMenu.options, children: subActions)
                }
                return elem
            }

            if !inserted {
                if let targetIndex = actions.lastIndex(where: { $0.debugDescription.contains("paste:") || $0.debugDescription.contains("copy:") }) {
                    actions.insert(menu, at: targetIndex + 1)
                } else {
                    actions.append(menu)
                }
            }
        }

        return UIMenu(children: actions)
    }

    private func formatMenu(for range: NSRange, in textView: UITextView) -> UIMenu {
        let boldAction = UIAction(
            title: NSLocalizedString("Bold", comment: "Text formatting bold"),
            image: UIImage(systemName: "bold")
        ) { [weak self, weak textView] _ in
            guard let self, let textView else { return }
            self.applyFormatting(prefix: "**", suffix: "**", range: range, in: textView)
        }

        let italicAction = UIAction(
            title: NSLocalizedString("Italic", comment: "Text formatting italic"),
            image: UIImage(systemName: "italic")
        ) { [weak self, weak textView] _ in
            guard let self, let textView else { return }
            self.applyFormatting(prefix: "_", suffix: "_", range: range, in: textView)
        }

        let strikethroughAction = UIAction(
            title: NSLocalizedString("Strikethrough", comment: "Text formatting strikethrough"),
            image: UIImage(systemName: "strikethrough")
        ) { [weak self, weak textView] _ in
            guard let self, let textView else { return }
            self.applyFormatting(prefix: "~", suffix: "~", range: range, in: textView)
        }

        let codeAction = UIAction(
            title: NSLocalizedString("Monospace", comment: "Text formatting monospace code"),
            image: UIImage(systemName: "curlybraces")
        ) { [weak self, weak textView] _ in
            guard let self, let textView else { return }
            self.applyFormatting(prefix: "`", suffix: "`", range: range, in: textView)
        }

        let spoilerAction = UIAction(
            title: NSLocalizedString("Spoiler", comment: "Text formatting secret spoiler"),
            image: UIImage(systemName: "eye.slash")
        ) { [weak self, weak textView] _ in
            guard let self, let textView else { return }
            self.applyFormatting(prefix: "#", suffix: "#", range: range, in: textView)
        }

        return UIMenu(
            title: NSLocalizedString("Format", comment: "Text formatting menu title"),
            image: UIImage(systemName: "textformat"),
            children: [boldAction, italicAction, strikethroughAction, codeAction, spoilerAction]
        )
    }

    private func applyFormatting(prefix: String, suffix: String, range: NSRange, in textView: UITextView) {
        let currentText = textView.text as NSString
        let targetRange = (textView.selectedRange.length > 0 && textView.selectedRange.location != NSNotFound) ? textView.selectedRange : range
        guard targetRange.location != NSNotFound,
              targetRange.location + targetRange.length <= currentText.length else { return }

        let fullString = textView.text ?? ""
        guard let textRange = Range(targetRange, in: fullString) else { return }

        let selectedSubstring = fullString[textRange]
        let trimmedLeading = selectedSubstring.prefix(while: { $0.isWhitespace || $0.isNewline })
        let rest = selectedSubstring.dropFirst(trimmedLeading.count)
        let trimmedTrailing = rest.suffix(while: { $0.isWhitespace || $0.isNewline })
        let core = rest.dropLast(trimmedTrailing.count)

        guard !core.isEmpty else { return }

        let formattedCore = "\(prefix)\(core)\(suffix)"
        let replacement = "\(trimmedLeading)\(formattedCore)\(trimmedTrailing)"

        if let startPos = textView.position(from: textView.beginningOfDocument, offset: targetRange.location),
           let endPos = textView.position(from: startPos, offset: targetRange.length),
           let inputSelectedRange = textView.textRange(from: startPos, to: endPos) {
            textView.replace(inputSelectedRange, withText: replacement)
        } else {
            textView.text = currentText.replacingCharacters(in: targetRange, with: replacement)
        }

        let newCursorLocation = targetRange.location + replacement.utf16.count
        let newRange = NSRange(location: newCursorLocation, length: 0)
        textView.selectedRange = newRange
        if parent?.selectedRange != newRange {
            parent?.selectedRange = newRange
        }

        textViewDidChange(textView)
        updateHeight()
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
            lastUnfocusedDate: Binding.constant(previewDate),
            placeholder: Binding.constant("Placeholder"),
            selectedRange: Binding.constant(NSRange(location: 0, length: 0)),
            onImagesAdded: { _ in }
        )
        .fixedSize(horizontal: false, vertical: true)
    }
}
