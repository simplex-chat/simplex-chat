//
//  ChatWallpaper.swift
//  SimpleX (iOS)
//
//  Created by Avently on 06.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat

public enum PresetWallpaper {
    case cats
    case flowers
    case hearts
    case kids
    case school
    case travel

    var res: UIImage {
        UIImage(named: "wallpaper_\(filename)")!
    }

    var filename: String {
        switch self {
        case .cats: "cats"
        case .flowers: "flowers"
        case .hearts: "hearts"
        case .kids: "kids"
        case .school: "school"
        case .travel: "travel"
        }
    }

    var scale: Float {
        switch self {
        case .cats: 0.63
        case .flowers: 0.53
        case .hearts: 0.59
        case .kids: 0.53
        case .school: 0.53
        case .travel: 0.68
        }
    }

    var background: [DefaultTheme: Color] {
        switch self {
        case .cats: wallpaperBackgrounds(light: "#ffF8F6EA")
        case .flowers: wallpaperBackgrounds(light: "#ffE2FFE4")
        case .hearts: wallpaperBackgrounds(light: "#ffFDECEC")
        case .kids: wallpaperBackgrounds(light: "#ffdbfdfb")
        case .school: wallpaperBackgrounds(light: "#ffE7F5FF")
        case .travel: wallpaperBackgrounds(light: "#fff9eeff")
        }
    }

    var tint: [DefaultTheme: Color] {
        switch self {
        case .cats: [
            DefaultTheme.LIGHT: "#ffefdca6".colorFromReadableHex(),
            DefaultTheme.DARK: "#ff4b3b0e".colorFromReadableHex(),
            DefaultTheme.SIMPLEX: "#ff51400f".colorFromReadableHex(),
            DefaultTheme.BLACK: "#ff4b3b0e".colorFromReadableHex()
        ]
        case .flowers: [
            DefaultTheme.LIGHT: "#ff9CEA59".colorFromReadableHex(),
            DefaultTheme.DARK: "#ff31560D".colorFromReadableHex(),
            DefaultTheme.SIMPLEX: "#ff36600f".colorFromReadableHex(),
            DefaultTheme.BLACK: "#ff31560D".colorFromReadableHex()
        ]
        case .hearts: [
            DefaultTheme.LIGHT: "#fffde0e0".colorFromReadableHex(),
            DefaultTheme.DARK: "#ff3c0f0f".colorFromReadableHex(),
            DefaultTheme.SIMPLEX: "#ff411010".colorFromReadableHex(),
            DefaultTheme.BLACK: "#ff3C0F0F".colorFromReadableHex()
        ]
        case .kids: [
            DefaultTheme.LIGHT: "#ffadeffc".colorFromReadableHex(),
            DefaultTheme.DARK: "#ff16404B".colorFromReadableHex(),
            DefaultTheme.SIMPLEX: "#ff184753".colorFromReadableHex(),
            DefaultTheme.BLACK: "#ff16404B".colorFromReadableHex()
        ]
        case .school: [
            DefaultTheme.LIGHT: "#ffCEEBFF".colorFromReadableHex(),
            DefaultTheme.DARK: "#ff0F293B".colorFromReadableHex(),
            DefaultTheme.SIMPLEX: "#ff112f43".colorFromReadableHex(),
            DefaultTheme.BLACK: "#ff0F293B".colorFromReadableHex()
        ]
        case .travel: [
            DefaultTheme.LIGHT: "#ffeedbfe".colorFromReadableHex(),
            DefaultTheme.DARK: "#ff311E48".colorFromReadableHex(),
            DefaultTheme.SIMPLEX: "#ff35204e".colorFromReadableHex(),
            DefaultTheme.BLACK: "#ff311E48".colorFromReadableHex()
        ]
        }
    }

    var colors: [DefaultTheme: ThemeColors] {
        switch self {
        case .cats: [
            DefaultTheme.LIGHT: ThemeColors.from(
                sentMessage: "#fffffaed",
                sentQuote: "#fffaf0d6",
                receivedMessage: "#ffF8F7F4",
                receivedQuote: "#ffefede9"
            ),
            DefaultTheme.DARK: ThemeColors.from(
                sentMessage: "#ff2f2919",
                sentQuote: "#ff473a1d",
                receivedMessage: "#ff272624",
                receivedQuote: "#ff373633"
            ),
            DefaultTheme.SIMPLEX: ThemeColors.from(
                sentMessage: "#ff41371b",
                sentQuote: "#ff654f1c",
                receivedMessage: "#ff272624",
                receivedQuote: "#ff373633"
            ),
            DefaultTheme.BLACK: ThemeColors.from(
                sentMessage: "#ff41371b",
                sentQuote: "#ff654f1c",
                receivedMessage: "#ff1f1e1b",
                receivedQuote: "#ff2f2d27"
            )
        ]
        case .flowers: [
            DefaultTheme.LIGHT: ThemeColors.from(
                sentMessage: "#fff1ffe5",
                sentQuote: "#ffdcf9c4",
                receivedMessage: "#ffF4F8F2",
                receivedQuote: "#ffe7ece7"
            ),
            DefaultTheme.DARK: ThemeColors.from(
                sentMessage: "#ff163521",
                sentQuote: "#ff1B5330",
                receivedMessage: "#ff242523",
                receivedQuote: "#ff353733"
            ),
            DefaultTheme.SIMPLEX: ThemeColors.from(
                sentMessage: "#ff184739",
                sentQuote: "#ff1F6F4B",
                receivedMessage: "#ff242523",
                receivedQuote: "#ff353733"
            ),
            DefaultTheme.BLACK: ThemeColors.from(
                sentMessage: "#ff184739",
                sentQuote: "#ff1F6F4B",
                receivedMessage: "#ff1c1f1a",
                receivedQuote: "#ff282b25"
            )
        ]
        case .hearts: [
            DefaultTheme.LIGHT: ThemeColors.from(
                sentMessage: "#fffff4f4",
                sentQuote: "#ffffdfdf",
                receivedMessage: "#fff8f6f6",
                receivedQuote: "#ffefebeb"
            ),
            DefaultTheme.DARK: ThemeColors.from(
                sentMessage: "#ff301515",
                sentQuote: "#ff4C1818",
                receivedMessage: "#ff242121",
                receivedQuote: "#ff3b3535"
            ),
            DefaultTheme.SIMPLEX: ThemeColors.from(
                sentMessage: "#ff491A28",
                sentQuote: "#ff761F29",
                receivedMessage: "#ff242121",
                receivedQuote: "#ff3b3535"
            ),
            DefaultTheme.BLACK: ThemeColors.from(
                sentMessage: "#ff491A28",
                sentQuote: "#ff761F29",
                receivedMessage: "#ff1f1b1b",
                receivedQuote: "#ff2e2626"
            )
        ]
        case .kids: [
            DefaultTheme.LIGHT: ThemeColors.from(
                sentMessage: "#ffeafeff",
                sentQuote: "#ffcbf4f7",
                receivedMessage: "#fff3fafa",
                receivedQuote: "#ffe4efef"
            ),
            DefaultTheme.DARK: ThemeColors.from(
                sentMessage: "#ff16302F",
                sentQuote: "#ff1a4a49",
                receivedMessage: "#ff252626",
                receivedQuote: "#ff373A39"
            ),
            DefaultTheme.SIMPLEX: ThemeColors.from(
                sentMessage: "#ff1a4745",
                sentQuote: "#ff1d6b69",
                receivedMessage: "#ff252626",
                receivedQuote: "#ff373a39"
            ),
            DefaultTheme.BLACK: ThemeColors.from(
                sentMessage: "#ff1a4745",
                sentQuote: "#ff1d6b69",
                receivedMessage: "#ff1e1f1f",
                receivedQuote: "#ff262b29"
            )
        ]
        case .school: [
            DefaultTheme.LIGHT: ThemeColors.from(
                sentMessage: "#ffeef9ff",
                sentQuote: "#ffD6EDFA",
                receivedMessage: "#ffF3F5F9",
                receivedQuote: "#ffe4e8ee"
            ),
            DefaultTheme.DARK: ThemeColors.from(
                sentMessage: "#ff172833",
                sentQuote: "#ff1C3E4F",
                receivedMessage: "#ff26282c",
                receivedQuote: "#ff393c40"
            ),
            DefaultTheme.SIMPLEX: ThemeColors.from(
                sentMessage: "#ff1A3C5D",
                sentQuote: "#ff235b80",
                receivedMessage: "#ff26282c",
                receivedQuote: "#ff393c40"
            ),
            DefaultTheme.BLACK: ThemeColors.from(
                sentMessage: "#ff1A3C5D",
                sentQuote: "#ff235b80",
                receivedMessage: "#ff1d1e22",
                receivedQuote: "#ff292b2f"
            )
        ]
        case .travel: [
            DefaultTheme.LIGHT: ThemeColors.from(
                sentMessage: "#fffcf6ff",
                sentQuote: "#fff2e0fc",
                receivedMessage: "#ffF6F4F7",
                receivedQuote: "#ffede9ee"
            ),
            DefaultTheme.DARK: ThemeColors.from(
                sentMessage: "#ff33263B",
                sentQuote: "#ff53385E",
                receivedMessage: "#ff272528",
                receivedQuote: "#ff3B373E"
            ),
            DefaultTheme.SIMPLEX: ThemeColors.from(
                sentMessage: "#ff3C255D",
                sentQuote: "#ff623485",
                receivedMessage: "#ff26273B",
                receivedQuote: "#ff3A394F"
            ),
            DefaultTheme.BLACK: ThemeColors.from(
                sentMessage: "#ff3C255D",
                sentQuote: "#ff623485",
                receivedMessage: "#ff231f23",
                receivedQuote: "#ff2c2931"
            )
        ]
        }
    }

    func toType(_ base: DefaultTheme, _ scale: Float? = nil) -> WallpaperType {
        WallpaperType.Preset(
            filename,
            scale ?? themeOverridesDefault.get().first { $0.wallpaper != nil && $0.wallpaper!.preset == filename && $0.base == base }?.wallpaper?.scale ?? 1
        )
    }

    static func from(_ filename: String) -> PresetWallpaper? {
        switch filename {
        case PresetWallpaper.cats.filename: PresetWallpaper.cats
        case PresetWallpaper.flowers.filename: PresetWallpaper.flowers
        case PresetWallpaper.hearts.filename: PresetWallpaper.hearts
        case PresetWallpaper.kids.filename: PresetWallpaper.kids
        case PresetWallpaper.school.filename: PresetWallpaper.school
        case PresetWallpaper.travel.filename: PresetWallpaper.travel
        default: nil
        }
    }
}

func wallpaperBackgrounds(light: String) -> [DefaultTheme : Color] {
    [
        DefaultTheme.LIGHT: light.colorFromReadableHex(),
        DefaultTheme.DARK: "#ff121212".colorFromReadableHex(),
        DefaultTheme.SIMPLEX: "#ff111528".colorFromReadableHex(),
        DefaultTheme.BLACK: "#ff070707".colorFromReadableHex()
    ]
}

public enum WallpaperScaleType/*(val contentScale: ContentScale)*/: Codable {
    case fill/* (ContentScale.Crop)*/
    case fit/* (ContentScale.Fit)*/
    case `repeat`/* (ContentScale.Fit)*/

    var text: String {
        switch self {
        case .fill: "Fill"
        case .fit: "Fit"
        case .repeat: "Repeat"
        }
    }
}

public enum WallpaperType {
    var image: SwiftUI.Image? {
        if let uiImage {
            return SwiftUI.Image(uiImage: uiImage)
        }
        return nil
    }

    var uiImage: UIImage? {
        let filename: String
        switch self {
        case let .Preset(f, _): filename = f
        case let .Image(f, _, _): filename = f
        default: return nil
        }
        if filename == "" { return nil }
        if let image = WallpaperType.cachedImages[filename] {
            return image
        } else {
            let res: UIImage?
            if case let .Preset(filename, _) = self {
                res = (PresetWallpaper.from(filename) ?? PresetWallpaper.cats).res
            } else {
                do {
                    // LALAL REMOVE
                    res = nil
                    // In case of unintentional image deletion don't crash the app
                    //File(getWallpaperFilePath(filename)).inputStream().use { loadImageBitmap(it) }
                } catch let e {
                    logger.error("Error while loading wallpaper file: \(e)")
                    res = nil
                }
            }
            if let res {
                WallpaperType.cachedImages[filename] = res
            }
            return res
        }
    }

    func sameType(_ other: WallpaperType?) -> Bool {
        if case let .Preset(filename, _) = self, case let .Preset(otherFilename, _) = other { filename == otherFilename }
        else if case .Image = self, case .Image = other { true }
        else if case .Empty = self, case .Empty = other { true }
        else { false }
    }

    func samePreset(other: PresetWallpaper?) -> Bool { if case let .Preset(filename, _) = self, filename == other?.filename { true } else { false } }

    case Preset(_ filename: String, _ scale: Float?)

    case Image(_ filename: String, _ scale: Float?, _ scaleType: WallpaperScaleType?)

    case Empty

    func defaultBackgroundColor(_ theme: DefaultTheme, _ themeBackground: Color) -> Color {
        if case let .Preset(filename, _) = self {
            (PresetWallpaper.from(filename) ?? PresetWallpaper.cats).background[theme]!
        } else {
            themeBackground
        }
    }

    func defaultTintColor(_ theme: DefaultTheme) -> Color {
        if case let .Preset(filename, _) = self {
            (PresetWallpaper.from(filename) ?? PresetWallpaper.cats).tint[theme]!
        } else if case let .Image(_, _, scaleType) = self, scaleType == WallpaperScaleType.repeat {
            Color.clear
        } else {
            Color.clear
        }
    }

    static var cachedImages: [String: UIImage] = [:]

    static func from(_ wallpaper: ThemeWallpaper?) -> WallpaperType? {
        if wallpaper == nil {
            return nil
        } else if let preset = wallpaper?.preset {
            return WallpaperType.Preset(preset, wallpaper?.scale)
        } else if let imageFile = wallpaper?.imageFile {
            return WallpaperType.Image(imageFile, wallpaper?.scale, wallpaper?.scaleType)
        } else {
            return WallpaperType.Empty
        }
    }
}


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

