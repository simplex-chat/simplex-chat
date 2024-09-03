import SwiftUI

extension View {
    @ViewBuilder
    func sheetWithDetents<Content: View>(
        isPresented: Binding<Bool>,
        detents: [UISheetPresentationController.Detent] = [.medium(), .large()],
        @ViewBuilder content: @escaping () -> Content
    ) -> some View {
        if #available(iOS 16, *) {
            sheet(isPresented: isPresented, content: content)
                .presentationDetents(
                    Set(detents.compactMap { presentationDetent(detent:$0) })
                )
        } else {
            ZStack {
                SheetPresenter(
                    isPresented: isPresented,
                    detents: detents,
                    content: content()
                )
                self
            }
        }
    }
}

@available(iOS 16.0, *)
private func presentationDetent(detent: UISheetPresentationController.Detent) -> PresentationDetent? {
    let map: [UISheetPresentationController.Detent.Identifier: PresentationDetent] = [
        .medium: .medium,
        .large: .large
    ]
    return map[detent.identifier]
}

/// An transparent view which is added to SwfttUI view hierarchy an presents a UIKit sheet with detents
private struct SheetPresenter<Content: View>: UIViewRepresentable {
    @Binding var isPresented: Bool
    let detents: [UISheetPresentationController.Detent]
    let content: Content

    func makeUIView(context: Context) -> UIView { UIView() }

    func updateUIView(_ uiView: UIView, context: Context) {
        let hc = UIHostingController(rootView: content)
        if let spc = hc.presentationController as? UISheetPresentationController {
            spc.detents = detents
            spc.prefersGrabberVisible = true
            spc.prefersScrollingExpandsWhenScrolledToEdge = false
            spc.largestUndimmedDetentIdentifier = .medium
        }
        hc.presentationController?.delegate = context.coordinator
        if let root = uiView.window?.rootViewController {
            if isPresented {
                if root.presentedViewController != nil {
                    // Simultaneous sheets are not allowed - dismiss the previous one
                    root.dismiss(animated: true) { root.present(hc, animated: true) }
                } else {
                    root.present(hc, animated: true)
                }
            } else {
                root.dismiss(animated: true)
            }
        }
    }

    func makeCoordinator() -> Coordinator { Coordinator(isPresented: $isPresented) }

    class Coordinator: NSObject, UISheetPresentationControllerDelegate {
        @Binding var isPresented: Bool

        init(isPresented: Binding<Bool>) { self._isPresented = isPresented }

        func presentationControllerDidDismiss(_ presentationController: UIPresentationController) {
            isPresented = false
        }
    }
}

