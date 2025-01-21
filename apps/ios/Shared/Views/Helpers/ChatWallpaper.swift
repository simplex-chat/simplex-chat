//
//  ChatWallpaper.swift
//  SimpleX (iOS)
//
//  Created by Avently on 14.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat

struct ChatViewBackground: ViewModifier {
    @Environment(\.colorScheme) var colorScheme
    var image: Image
    var imageType: WallpaperType
    var background: Color
    var tint: Color

    func body(content: Content) -> some View {
        // Workaround a problem (SwiftUI bug?) when wallpaper is not updated when user changes global theme in iOS settings from dark to light and vice versa
        if colorScheme == .light {
            back(content)
        } else {
            back(content)
        }
    }

    func back(_ content: Content) -> some View {
        content.background(
            Canvas { context, size in
                var image = context.resolve(image)
                let rect = CGRectMake(0, 0, size.width, size.height)
                func repeatDraw(_ imageScale: CGFloat) {
                    // Prevent range bounds crash and dividing by zero
                    if size.height == 0 || size.width == 0 || image.size.height == 0 || image.size.width == 0 { return }
                    image.shading = .color(tint)
                    let scale = imageScale * 2.5 // scale wallpaper for iOS
                    for h in 0 ... Int(size.height / image.size.height / scale) {
                        for w in 0 ... Int(size.width / image.size.width / scale) {
                            let rect = CGRectMake(CGFloat(w) * image.size.width * scale, CGFloat(h) * image.size.height * scale, image.size.width * scale, image.size.height * scale)
                            context.draw(image, in: rect, style: FillStyle())
                        }
                    }
                }
                context.fill(Path(rect), with: .color(background))
                switch imageType {
                case let WallpaperType.preset(filename, scale):
                    repeatDraw(CGFloat((scale ?? 1) * (PresetWallpaper.from(filename)?.scale ?? 1)))
                case let WallpaperType.image(_, scale, scaleType):
                    let scaleType = scaleType ?? WallpaperScaleType.fill
                    switch scaleType {
                    case WallpaperScaleType.repeat: repeatDraw(CGFloat(scale ?? 1))
                    case WallpaperScaleType.fill: fallthrough
                    case WallpaperScaleType.fit:
                        let scale = scaleType.computeScaleFactor(image.size, size)
                        let scaledWidth = (image.size.width * scale.0)
                        let scaledHeight = (image.size.height * scale.1)
                        context.draw(image, in: CGRectMake(((size.width - scaledWidth) / 2), ((size.height - scaledHeight) / 2), scaledWidth, scaledHeight), style: FillStyle())
                        if case WallpaperScaleType.fit = scaleType {
                            if scaledWidth < size.width {
                                // has black lines at left and right sides
                                var x = (size.width - scaledWidth) / 2
                                while x > 0 {
                                    context.draw(image, in: CGRectMake((x - scaledWidth), ((size.height - scaledHeight) / 2), scaledWidth, scaledHeight), style: FillStyle())
                                    x -= scaledWidth
                                }
                                x = size.width - (size.width - scaledWidth) / 2
                                while x < size.width {
                                    context.draw(image, in: CGRectMake(x, ((size.height - scaledHeight) / 2), scaledWidth, scaledHeight), style: FillStyle())

                                    x += scaledWidth
                                }
                            } else {
                                // has black lines at top and bottom sides
                                var y = (size.height - scaledHeight) / 2
                                while y > 0 {
                                    context.draw(image, in: CGRectMake(((size.width - scaledWidth) / 2), (y - scaledHeight), scaledWidth, scaledHeight), style: FillStyle())
                                    y -= scaledHeight
                                }
                                y = size.height - (size.height - scaledHeight) / 2
                                while y < size.height {
                                    context.draw(image, in: CGRectMake(((size.width - scaledWidth) / 2), y, scaledWidth, scaledHeight), style: FillStyle())
                                    y += scaledHeight
                                }
                            }
                        }
                        context.fill(Path(rect), with: .color(tint))
                    }
                case WallpaperType.empty: ()
                }
            }
        ).ignoresSafeArea(.all)
    }
}

extension PresetWallpaper {
    public func toType(_ base: DefaultTheme, _ scale: Float? = nil) -> WallpaperType {
        let scale = if let scale {
            scale
        } else if let type = ChatModel.shared.currentUser?.uiThemes?.preferredMode(base.mode == DefaultThemeMode.dark)?.wallpaper?.toAppWallpaper().type, type.sameType(WallpaperType.preset(filename, nil)) {
            type.scale
        } else if let scale = themeOverridesDefault.get().first(where: { $0.wallpaper != nil && $0.wallpaper!.preset == filename && $0.base == base })?.wallpaper?.scale {
            scale
        } else {
            Float(1.0)
        }
        return WallpaperType.preset(
            filename,
            scale
        )
    }
}
