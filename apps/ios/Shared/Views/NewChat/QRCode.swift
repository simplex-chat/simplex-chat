//
//  QRCode.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CoreImage.CIFilterBuiltins

struct MutableQRCode: View {
    @Binding var uri: String
    var withLogo: Bool = true
    var tintColor = UIColor(red: 0.023, green: 0.176, blue: 0.337, alpha: 1)

    var body: some View {
        QRCode(uri: uri, withLogo: withLogo, tintColor: tintColor)
            .id("simplex-qrcode-view-for-\(uri)")
    }
}

struct SimpleXLinkQRCode: View {
    let uri: String
    var withLogo: Bool = true
    var tintColor = UIColor(red: 0.023, green: 0.176, blue: 0.337, alpha: 1)

    var body: some View {
        QRCode(uri: simplexChatLink(uri), withLogo: withLogo, tintColor: tintColor)
    }
}

func simplexChatLink(_ uri: String) -> String {
    uri.starts(with: "simplex:/")
    ? uri.replacingOccurrences(of: "simplex:/", with: "https://simplex.chat/")
    : uri
}

struct QRCode: View {
    let uri: String
    var withLogo: Bool = true
    var tintColor = UIColor(red: 0.023, green: 0.176, blue: 0.337, alpha: 1)
    @State private var image: UIImage? = nil
    @State private var makeScreenshotFunc: () -> Void = {}

    var body: some View {
        ZStack {
            if let image = image {
                qrCodeImage(image)
            }
            GeometryReader { geo in
                ZStack {
                    if withLogo {
                        let w = geo.size.width
                        Image("icon-light")
                        .resizable()
                        .scaledToFit()
                        .frame(width: w * 0.16, height: w * 0.16)
                        .frame(width: w * 0.165, height: w * 0.165)
                        .background(.white)
                        .clipShape(Circle())
                    }
                }
                .onAppear {
                    makeScreenshotFunc = {
                        let size = CGSizeMake(1024 / UIScreen.main.scale, 1024 / UIScreen.main.scale)
                        showShareSheet(items: [makeScreenshot(geo.frame(in: .local).origin, size)])
                    }
                }
                .frame(width: geo.size.width, height: geo.size.height)
            }
        }
        .onTapGesture(perform: makeScreenshotFunc)
        .onAppear {
            image = image ?? generateImage(uri, tintColor: tintColor)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
}

private func qrCodeImage(_ image: UIImage) -> some View {
    Image(uiImage: image)
        .resizable()
        .interpolation(.none)
        .aspectRatio(1, contentMode: .fit)
        .textSelection(.enabled)
}

private func generateImage(_ uri: String, tintColor: UIColor) -> UIImage? {
    let context = CIContext()
    let filter = CIFilter.qrCodeGenerator()
    filter.message = Data(uri.utf8)
    if let outputImage = filter.outputImage,
       let cgImage = context.createCGImage(outputImage, from: outputImage.extent) {
        return UIImage(cgImage: cgImage).replaceColor(UIColor.black, tintColor)
    }
    return nil
}

extension View {
    func makeScreenshot(_ origin: CGPoint? = nil, _ targetSize: CGSize? = nil) -> UIImage {
        let controller = UIHostingController(rootView: self.edgesIgnoringSafeArea(.all))
        let targetSize = targetSize ?? controller.view.intrinsicContentSize
        let view = controller.view
        view?.bounds = CGRect(origin: origin ?? .zero, size: targetSize)
        view?.backgroundColor = .clear
        let renderer = UIGraphicsImageRenderer(size: targetSize)
        return renderer.image { _ in
            view?.drawHierarchy(in: controller.view.bounds, afterScreenUpdates: true)
        }
    }
}

struct QRCode_Previews: PreviewProvider {
    static var previews: some View {
        QRCode(uri: "https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FFe5ICmvrm4wkrr6X1LTMii-lhBqLeB76%23MCowBQYDK2VuAyEAdhZZsHpuaAk3Hh1q0uNb_6hGTpuwBIrsp2z9U2T0oC0%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAcz6jJk71InuxA0bOX7OUhddfB8Ov7xwQIlIDeXBRZaOntUU4brU5Y3rBzroZBdQJi0FKdtt_D7I%3D%2CMEIwBQYDK2VvAzkA-hDvk1duBi1hlOr08VWSI-Ou4JNNSQjseY69QyKm7Kgg1zZjbpGfyBqSZ2eqys6xtoV4ZtoQUXQ%3D")
    }
}
