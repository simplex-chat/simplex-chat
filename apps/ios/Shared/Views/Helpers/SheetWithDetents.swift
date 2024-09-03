import SwiftUI

extension View {
    @ViewBuilder
    func sheet<Item: Identifiable, Content: View>(
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
                    detents: detents.map { $0.controllerDetent },
                    item: item,
                    content: content
                )
                self
            }
        }
    }
}

/// iOS 15 Compatible detents.
/// Defines the fallback thresholds,
/// since fractional and constant height sheets are unavailable.
enum SheetDetent {
    case medium
    case large
    case height(Double)
    case fraction(Double)

    @available(iOS 16.0, *)
    fileprivate var presentationDetent: PresentationDetent {
        switch self {
        case .medium: .medium
        case .large: .large
        case let .height(value): .height(value)
        case let .fraction(value): .fraction(value)
        }
    }

    fileprivate var controllerDetent: UISheetPresentationController.Detent {
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
    let detents: [UISheetPresentationController.Detent]
    @Binding var item: Item?
    @ViewBuilder let content: (Item) -> Content

    func makeUIView(context: Context) -> UIView { UIView() }

    func updateUIView(_ uiView: UIView, context: Context) {
        guard let root = uiView.window?.rootViewController else { return }
        guard item?.id.hashValue != lastSheetHash else { return }
        lastSheetHash = item?.id.hashValue
        if let item {
            let hc = UIHostingController(rootView: content(item))
            if let spc = hc.presentationController as? UISheetPresentationController {
                spc.detents = detents
                spc.prefersGrabberVisible = detents.count > 1
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

        func presentationControllerDidDismiss(_: UIPresentationController) { item = nil }
    }
}

