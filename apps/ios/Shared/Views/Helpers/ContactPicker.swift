//
//  ContactPicker.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 01.05.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import ContactsUI

public struct ContactPicker: UIViewControllerRepresentable {
    @Binding var isPresented: Bool
    var onSelectContacts: ((_: [CNContact]) -> Void)?
    var onCancel: (() -> Void)?
    @State private var viewModel = ContactPickerViewModel()

    public func makeUIViewController(context: UIViewControllerRepresentableContext<ContactPicker>) -> ContactPicker.UIViewControllerType {
        let dummy = _DummyViewController()
        viewModel.dummy = dummy
        return dummy
    }

    public func updateUIViewController(_ uiViewController: _DummyViewController, context: UIViewControllerRepresentableContext<ContactPicker>) {
        guard viewModel.dummy != nil else {
            return
        }

        let ableToPresent = viewModel.dummy.presentedViewController == nil || viewModel.dummy.presentedViewController?.isBeingDismissed == true
        let ableToDismiss = viewModel.vc != nil

        if isPresented && viewModel.vc == nil && ableToPresent {
            let picker = CNContactPickerViewController()
            picker.predicateForEnablingContact = NSPredicate(format: "emailAddresses.@count > 0")
            picker.predicateForSelectionOfContact = NSPredicate(format: "emailAddresses.@count > 0")
            picker.delegate = context.coordinator
            viewModel.vc = picker
            viewModel.dummy.present(picker, animated: true)
        } else if !isPresented && ableToDismiss {
            viewModel.dummy.dismiss(animated: true)
            self.viewModel.vc = nil
        }
    }

    public func makeCoordinator() -> CNContactPickerDelegate {
        return ContactSelectionCoordinator(self)
    }

    public final class ContactSelectionCoordinator: NSObject, CNContactPickerDelegate {
        var parent : ContactPicker

        init(_ parent: ContactPicker){
            self.parent = parent
        }

        public func contactPickerDidCancel(_ picker: CNContactPickerViewController) {
            parent.isPresented = false
            parent.onCancel?()
        }

        public func contactPicker(_ picker: CNContactPickerViewController, didSelect contacts: [CNContact]) {
            parent.isPresented = false
            parent.onSelectContacts?(contacts)
        }
    }
}

class ContactPickerViewModel {
    var dummy: _DummyViewController!
    var vc: CNContactPickerViewController?
}

public class _DummyViewController: UIViewController {}
