//
//  NativeTextEditor.swift
//  SimpleX (iOS)
//
//  Created by Avently on 15.12.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct NativeTextEditor: UIViewRepresentable {
    @Binding var text: String
    let height: CGFloat
    let font: UIFont
    @FocusState.Binding var focused: Bool
    let alignment: TextAlignment
    let onImageAdded: (UIImage) -> Void
    
    func makeUIView(context: Context) -> UITextView {
        let field = CustomUITextField()
        field.allowsEditingTextAttributes = true
        field.text = text
        field.font = font
        field.textAlignment = alignment == .leading ? .left : .right
        field.autocapitalizationType = .sentences
        field.setOnTextChangedListener { newText, image in
            text = newText
            if let image = image {
                onImageAdded(image)
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
    var onTextChanged: (String, UIImage?) -> Void = { newText, image in }
    var onFocusChanged: (Bool) -> Void = { focused in }
    
    func setOnTextChangedListener(onTextChanged: @escaping (String, UIImage?) -> Void) {
        self.onTextChanged = onTextChanged
    }
    
    func setOnFocusChangedListener(onFocusChanged: @escaping (Bool) -> Void) {
        self.onFocusChanged = onFocusChanged
    }
    
    func textViewDidChange(_ textView: UITextView) {
        var image: UIImage? = nil
        textView.attributedText.enumerateAttribute(
            NSAttributedString.Key.attachment,
            in: NSRange(location: 0, length: textView.attributedText.length),
            options: [],
            using: { value, range, _ in
                if let attachment = (value as? NSTextAttachment)?.image {
                    image = attachment
                    let newText = NSMutableAttributedString(attributedString: textView.attributedText)
                    newText.replaceCharacters(in: range, with: "")
                    textView.attributedText = newText
                }
            }
        )
        onTextChanged(textView.text, image)
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
        @FocusState var keyboardVisible: Bool
        return NativeTextEditor(
            text: Binding.constant("Hello, world!"),
            height: 100,
            font: UIFont.preferredFont(forTextStyle: .body),
            focused: $keyboardVisible,
            alignment: TextAlignment.leading,
            onImageAdded: { _ in }
        )
    }
}
