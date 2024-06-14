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
    @EnvironmentObject var theme: AppTheme
    var image: Image
    var imageType: WallpaperType
    var background: Color
    var tint: Color

    func body(content: Content) -> some View {
        content.background(
            Canvas { context, size in
                var image = context.resolve(image)
                image.shading = .color(tint)
                let rect = CGRectMake(0, 0, size.width, size.height)
                func repeatDraw(_ imageScale: CGFloat) {
                    let scale = imageScale
                    for h in 0 ... Int(size.height / image.size.height / scale) {
                        for w in 0 ... Int(size.width / image.size.width / scale) {
                            let rect = CGRectMake(CGFloat(w) * image.size.width * scale, CGFloat(h) * image.size.height * scale, image.size.width * scale, image.size.height * scale)
                            context.draw(image, in: rect, style: FillStyle())
                        }
                    }
                }
                context.fill(Path(rect), with: .color(background))
                switch imageType {
                case let WallpaperType.Preset(filename, scale): repeatDraw(CGFloat((scale ?? 1) * (PresetWallpaper.from(filename)?.scale ?? 1)))
                case let WallpaperType.Image(_, scale, scaleType):
                    let scaleType = scaleType ?? WallpaperScaleType.fill
                    switch scaleType {
                    case WallpaperScaleType.repeat: repeatDraw(CGFloat(scale ?? 1))
                    case WallpaperScaleType.fill: fallthrough
                    case WallpaperScaleType.fit:
                        //                        let scale = scaleType.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
                        //                        let scaledWidth = (image.width * scale.scaleX).roundToInt()
                        //                        let scaledHeight = (image.height * scale.scaleY).roundToInt()
                        //                        drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
                        //                        if (scaleType == WallpaperScaleType.FIT) {
                        //                            if (scaledWidth < size.width) {
                        //                                // has black lines at left and right sides
                        //                                var x = (size.width - scaledWidth) / 2
                        //                                while (x > 0) {
                        //                                    drawImage(image, dstOffset = IntOffset(x = (x - scaledWidth).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
                        //                                    x -= scaledWidth
                        //                                }
                        //                                x = size.width - (size.width - scaledWidth) / 2
                        //                                while (x < size.width) {
                        //                                    drawImage(image, dstOffset = IntOffset(x = x.roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
                        //                                    x += scaledWidth
                        //                                }
                        //                            } else {
                        //                                // has black lines at top and bottom sides
                        //                                var y = (size.height - scaledHeight) / 2
                        //                                while (y > 0) {
                        //                                    drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = (y - scaledHeight).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
                        //                                    y -= scaledHeight
                        //                                }
                        //                                y = size.height - (size.height - scaledHeight) / 2
                        //                                while (y < size.height) {
                        //                                    drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = y.roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
                        //                                    y += scaledHeight
                        //                                }
                        //                            }
                        //                        }
                        context.fill(Path(rect), with: .color(tint))
                    }
                case WallpaperType.Empty: ()
                }
            }
        )
    }
}

extension PresetWallpaper {
    public func toType(_ base: DefaultTheme, _ scale: Float? = nil) -> WallpaperType {
        WallpaperType.Preset(
            filename,
            scale ?? themeOverridesDefault.get().first { $0.wallpaper != nil && $0.wallpaper!.preset == filename && $0.base == base }?.wallpaper?.scale ?? 1
        )
    }
}
