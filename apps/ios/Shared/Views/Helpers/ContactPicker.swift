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
    @Binding var showPicker: Bool
    @State private var viewModel = ContactPickerViewModel()
    public var predicateForEnablingContact: NSPredicate?
    public var onSelectContact: ((_: CNContact) -> Void)?
    public var onSelectContacts: ((_: [CNContact]) -> Void)?
    public var onCancel: (() -> Void)?

    public init(
        showPicker: Binding<Bool>,
        predicateForEnablingContact: NSPredicate? = nil,
        onSelectContact: ((_: CNContact) -> Void)? = nil,
        onSelectContacts: ((_: [CNContact]) -> Void)? = nil,
        onCancel: (() -> Void)? = nil
    ) {
        self._showPicker = showPicker
        self.predicateForEnablingContact = predicateForEnablingContact
        self.onSelectContact = onSelectContact
        self.onSelectContacts = onSelectContacts
        self.onCancel = onCancel
    }

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

        if showPicker && viewModel.vc == nil && ableToPresent {
            let pickerVC = CNContactPickerViewController()
            pickerVC.predicateForEnablingContact = self.predicateForEnablingContact
            pickerVC.delegate = context.coordinator
            viewModel.vc = pickerVC
            viewModel.dummy.present(pickerVC, animated: true)
        } else if !showPicker && ableToDismiss {
            viewModel.dummy.dismiss(animated: true)
            self.viewModel.vc = nil
        }
    }

    public func makeCoordinator() -> ContactPickerCoordinator {
        if self.onSelectContacts != nil {
            return MultipleSelectionCoordinator(self)
        } else {
            return SingleSelectionCoordinator(self)
        }
    }

    public final class SingleSelectionCoordinator: NSObject, ContactPickerCoordinator {
        var parent : ContactPicker

        init(_ parent: ContactPicker){
            self.parent = parent
        }

        public func contactPickerDidCancel(_ picker: CNContactPickerViewController) {
            parent.showPicker = false
            parent.onCancel?()
        }

        public func contactPicker(_ picker: CNContactPickerViewController, didSelect contact: CNContact) {
            parent.showPicker = false
            parent.onSelectContact?(contact)
        }
    }

    public final class MultipleSelectionCoordinator: NSObject, ContactPickerCoordinator {
        var parent : ContactPicker

        init(_ parent: ContactPicker){
            self.parent = parent
        }

        public func contactPickerDidCancel(_ picker: CNContactPickerViewController) {
            parent.showPicker = false
            parent.onCancel?()
        }

        public func contactPicker(_ picker: CNContactPickerViewController, didSelect contacts: [CNContact]) {
            parent.showPicker = false
            parent.onSelectContacts?(contacts)
        }
    }
}

class ContactPickerViewModel {
    var dummy: _DummyViewController!
    var vc: CNContactPickerViewController?
}

public protocol ContactPickerCoordinator: CNContactPickerDelegate {}

public class _DummyViewController: UIViewController {}
