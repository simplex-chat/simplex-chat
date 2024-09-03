import SwiftUI

extension View {
    @ViewBuilder
    func sheetWithDetents<Item: Identifiable, Content: View>(
        item: Binding<Item?>,
        detents: [SheetDetent],
        @ViewBuilder content: @escaping (Item) -> Content
    ) -> some View {
        if #available(iOS 16, *) {
            sheet(item: item) { item in
                content(item).presentationDetents(
                    Set(detents.map { $0.presentationDetent })
                )
            }
        } else {
            ZStack {
                SheetPresenter(
                    item: item,
                    detents: detents.map { $0.controllerDetent },
                    content: content
                )
                self
            }
        }
    }
}

enum SheetDetent {
    case medium
    case large
    case height(Double)
    case fraction(Double)

    @available(iOS 16.0, *)
    var presentationDetent: PresentationDetent {
        switch self {
        case .medium: .medium
        case .large: .large
        case let .height(value): .height(value)
        case let .fraction(value): .fraction(value)
        }
    }

    var controllerDetent: UISheetPresentationController.Detent {
        switch self {
        case .medium: .medium()
        case .large: .large()
        case let .height(h): h > 500 ? .large() : .medium()
        case let .fraction(f): f > 0.5 ? .large() : .medium()
        }
    }
}

/// Prevents re-presenting the same sheet
private var lastSheetHash: Int?

/// An transparent view which is added to SwiftUI
/// view hierarchy and presents a UIKit sheet with detents
private struct SheetPresenter<Item: Identifiable, Content: View>: UIViewRepresentable {
    @Binding var item: Item?
    @State var lastPresented: Item.ID?

    let detents: [UISheetPresentationController.Detent]
    @ViewBuilder let content: (Item) -> Content

    func makeUIView(context: Context) -> UIView { UIView() }

    func updateUIView(_ uiView: UIView, context: Context) {
        // Prevent
        guard let root = uiView.window?.rootViewController else { return }
        guard item?.id.hashValue != lastSheetHash else { return }
        lastSheetHash = item?.id.hashValue
        if let item {
            let hc = UIHostingController(rootView: content(item))
            if let spc = hc.presentationController as? UISheetPresentationController {
                spc.detents = detents
                spc.prefersGrabberVisible = true
                spc.prefersScrollingExpandsWhenScrolledToEdge = false
                spc.largestUndimmedDetentIdentifier = .medium
            }
            hc.presentationController?.delegate = context.coordinator
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

    func makeCoordinator() -> Coordinator { Coordinator(item: $item) }

    class Coordinator: NSObject, UISheetPresentationControllerDelegate {
        @Binding var item: Item?

        init(item: Binding<Item?>) { _item = item }

        func presentationControllerDidDismiss(_ presentationController: UIPresentationController) {
            item = nil
        }
    }
}

