//
//  ChatWallpaper.swift
//  SimpleX (iOS)
//
//  Created by Avently on 06.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

// Spec: spec/services/theme.md#PresetWallpaper
public enum PresetWallpaper: CaseIterable {
    case cats
    case flowers
    case hearts
    case kids
    case school
    case travel

    var res: UIImage {
        UIImage(named: "wallpaper_\(filename)")!
    }

    public var filename: String {
        switch self {
        case .cats: "cats"
        case .flowers: "flowers"
        case .hearts: "hearts"
        case .kids: "kids"
        case .school: "school"
        case .travel: "travel"
        }
    }

    public var scale: Float {
        switch self {
        case .cats: 0.63
        case .flowers: 0.53
        case .hearts: 0.59
        case .kids: 0.53
        case .school: 0.53
        case .travel: 0.68
        }
    }

    public var background: [DefaultTheme: Color] {
        switch self {
        case .cats: [
            DefaultTheme.LIGHT: oklch(0.9714242, 0.01596467, 98.99223),
            DefaultTheme.DARK: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.SIMPLEX: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.BLACK: oklch(0.1285578, 0, 0)
        ]
        case .flowers: [
            DefaultTheme.LIGHT: oklch(0.9718878, 0.04671557, 147.1246),
            DefaultTheme.DARK: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.SIMPLEX: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.BLACK: oklch(0.1285578, 0, 0)
        ]
        case .hearts: [
            DefaultTheme.LIGHT: oklch(0.9565624, 0.01848713, 17.48077),
            DefaultTheme.DARK: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.SIMPLEX: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.BLACK: oklch(0.1285578, 0, 0)
        ]
        case .kids: [
            DefaultTheme.LIGHT: oklch(0.9693045, 0.03516977, 192.2433),
            DefaultTheme.DARK: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.SIMPLEX: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.BLACK: oklch(0.1285578, 0, 0)
        ]
        case .school: [
            DefaultTheme.LIGHT: oklch(0.9626785, 0.02004578, 238.6614),
            DefaultTheme.DARK: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.SIMPLEX: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.BLACK: oklch(0.1285578, 0, 0)
        ]
        case .travel: [
            DefaultTheme.LIGHT: oklch(0.9626377, 0.0253131, 313.9639),
            DefaultTheme.DARK: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.SIMPLEX: oklch(0.2024453, 0.03849037, 273.4875),
            DefaultTheme.BLACK: oklch(0.1285578, 0, 0)
        ]
        }
    }

    public var tint: [DefaultTheme: Color] {
        switch self {
        case .cats: [
            DefaultTheme.LIGHT: oklch(0.897064, 0.07281305, 90.95935),
            DefaultTheme.DARK: oklch(0.3603656, 0.0643012, 88.54155),
            DefaultTheme.SIMPLEX: oklch(0.3797781, 0.06842897, 88.88896),
            DefaultTheme.BLACK: oklch(0.3603656, 0.0643012, 88.54155)
        ]
        case .flowers: [
            DefaultTheme.LIGHT: oklch(0.8574244, 0.1932141, 133.0531),
            DefaultTheme.DARK: oklch(0.3183068, 0.07771506, 136.6477),
            DefaultTheme.SIMPLEX: oklch(0.3384358, 0.08523031, 136.3517),
            DefaultTheme.BLACK: oklch(0.3183068, 0.07771506, 136.6477)
        ]
        case .hearts: [
            DefaultTheme.LIGHT: oklch(0.7986557, 0.1086374, 14.80189),
            DefaultTheme.DARK: oklch(0.25332, 0.06915632, 12.58639),
            DefaultTheme.SIMPLEX: oklch(0.267816, 0.07407809, 16.28852),
            DefaultTheme.BLACK: oklch(0.25332, 0.06915632, 12.58639)
        ]
        case .kids: [
            DefaultTheme.LIGHT: oklch(0.8721581, 0.1030755, 189.3001),
            DefaultTheme.DARK: oklch(0.3107921, 0.04607738, 186.1605),
            DefaultTheme.SIMPLEX: oklch(0.3306122, 0.05137846, 185.1468),
            DefaultTheme.BLACK: oklch(0.3107921, 0.04607738, 186.1605)
        ]
        case .school: [
            DefaultTheme.LIGHT: oklch(0.9252349, 0.04096641, 238.0518),
            DefaultTheme.DARK: oklch(0.2700986, 0.04630937, 241.5568),
            DefaultTheme.SIMPLEX: oklch(0.2929108, 0.05102392, 240.8139),
            DefaultTheme.BLACK: oklch(0.2700986, 0.04630937, 241.5568)
        ]
        case .travel: [
            DefaultTheme.LIGHT: oklch(0.9174161, 0.05105522, 309.6281),
            DefaultTheme.DARK: oklch(0.2817417, 0.07665313, 302.6645),
            DefaultTheme.SIMPLEX: oklch(0.2948376, 0.08277514, 302.7197),
            DefaultTheme.BLACK: oklch(0.2817417, 0.07665313, 302.6645)
        ]
        }
    }

    public var colors: [DefaultTheme: ResolvedColors] {
        switch self {
        case .cats: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9854474, 0.01790464, 89.3544),
                sentQuote: oklch(0.9562038, 0.0357691, 89.44265),
                receivedMessage: oklch(0.9760699, 0.004115805, 91.44609),
                receivedQuote: oklch(0.9465333, 0.005762915, 84.56661)
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2827141, 0.02844628, 89.80136),
                sentQuote: oklch(0.3550253, 0.04770112, 85.80835),
                receivedMessage: oklch(0.2689313, 0.003935312, 84.58291),
                receivedQuote: oklch(0.332832, 0.005361989, 91.54412)
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3402031, 0.04537511, 90.2498),
                sentQuote: oklch(0.4398707, 0.0737883, 85.23908),
                receivedMessage: oklch(0.2689313, 0.003935312, 84.58291),
                receivedQuote: oklch(0.332832, 0.005361989, 91.54412)
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3402031, 0.04537511, 90.2498),
                sentQuote: oklch(0.4398707, 0.0737883, 85.23908),
                receivedMessage: oklch(0.2349937, 0.005828091, 91.60813),
                receivedQuote: oklch(0.2971596, 0.01092985, 91.6846)
            )
        ]
        case .flowers: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9795341, 0.03522444, 133.5366),
                sentQuote: oklch(0.9336828, 0.07752936, 133.1505),
                receivedMessage: oklch(0.9694833, 0.007316795, 132.4149),
                receivedQuote: oklch(0.9357576, 0.01475573, 132.4773)
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2829779, 0.05664382, 137.7101),
                sentQuote: oklch(0.3779453, 0.09538202, 137.6479),
                receivedMessage: oklch(0.269555, 0.006171017, 134.9414),
                receivedQuote: oklch(0.3480199, 0.007943601, 137.802)
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3464215, 0.08494502, 141.0868),
                sentQuote: oklch(0.4739489, 0.1235496, 140.5453),
                receivedMessage: oklch(0.269555, 0.006171017, 134.9414),
                receivedQuote: oklch(0.3480199, 0.007943601, 137.802)
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3464215, 0.08494502, 141.0868),
                sentQuote: oklch(0.4739489, 0.1235496, 140.5453),
                receivedMessage: oklch(0.231738, 0.006406116, 134.9656),
                receivedQuote: oklch(0.304409, 0.009754443, 132.6025)
            )
        ]
        case .hearts: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9543917, 0.02302169, 10.95497),
                sentQuote: oklch(0.9097407, 0.04732327, 11.58726),
                receivedMessage: oklch(0.9716819, 0.002138744, 17.19447),
                receivedQuote: oklch(0.9446311, 0.002154032, 17.19577)
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2586266, 0.03317007, 16.50844),
                sentQuote: oklch(0.336993, 0.05366197, 11.41689),
                receivedMessage: oklch(0.2681172, 0.004799012, 355.0992),
                receivedQuote: oklch(0.3440906, 0.004499214, 354.9628)
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3319502, 0.07553982, 5.77679),
                sentQuote: oklch(0.3914789, 0.1003936, 7.041932),
                receivedMessage: oklch(0.2681172, 0.004799012, 355.0992),
                receivedQuote: oklch(0.3440906, 0.004499214, 354.9628)
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3319502, 0.07553982, 5.77679),
                sentQuote: oklch(0.3914789, 0.1003936, 7.041932),
                receivedMessage: oklch(0.2387909, 0.004945965, 355.1783),
                receivedQuote: oklch(0.3006853, 0.004658511, 355.0313)
            )
        ]
        case .kids: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9816451, 0.02600448, 193.8135),
                sentQuote: oklch(0.9476761, 0.05463824, 193.449),
                receivedMessage: oklch(0.9743321, 0.006487189, 185.2751),
                receivedQuote: oklch(0.9380073, 0.0109707, 182.8632)
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2964424, 0.03780104, 189.8327),
                sentQuote: oklch(0.3897975, 0.05223659, 187.8233),
                receivedMessage: oklch(0.2696843, 0.004349819, 196.8988),
                receivedQuote: oklch(0.346296, 0.008286395, 184.8154)
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3665172, 0.05344185, 180.3469),
                sentQuote: oklch(0.486998, 0.07291723, 181.2082),
                receivedMessage: oklch(0.2696843, 0.004349819, 196.8988),
                receivedQuote: oklch(0.346296, 0.008286395, 184.8154)
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3665172, 0.05344185, 180.3469),
                sentQuote: oklch(0.486998, 0.07291723, 181.2082),
                receivedMessage: oklch(0.2318771, 0.004503036, 196.8468),
                receivedQuote: oklch(0.2999201, 0.007224831, 182.5153)
            )
        ]
        case .school: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9756479, 0.01416295, 231.2013),
                sentQuote: oklch(0.9331527, 0.03006113, 232.4212),
                receivedMessage: oklch(0.9697657, 0.005748723, 264.5325),
                receivedQuote: oklch(0.9296755, 0.00918803, 258.3366)
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.267226, 0.03061943, 237.8609),
                sentQuote: oklch(0.3464064, 0.04943852, 232.4005),
                receivedMessage: oklch(0.2764251, 0.007910622, 264.4375),
                receivedQuote: oklch(0.3548081, 0.008034593, 255.5451)
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3481476, 0.07023845, 249.9259),
                sentQuote: oklch(0.4520089, 0.08394516, 241.1934),
                receivedMessage: oklch(0.2764251, 0.007910622, 264.4375),
                receivedQuote: oklch(0.3548081, 0.008034593, 255.5451)
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3481476, 0.07023845, 249.9259),
                sentQuote: oklch(0.4520089, 0.08394516, 241.1934),
                receivedMessage: oklch(0.2356588, 0.007789041, 274.6063),
                receivedQuote: oklch(0.2886546, 0.007823012, 264.445)
            )
        ]
        case .travel: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9803204, 0.01342671, 314.7601),
                sentQuote: oklch(0.9294779, 0.04197705, 313.6968),
                receivedMessage: oklch(0.9695303, 0.004487354, 314.8044),
                receivedQuote: oklch(0.9385522, 0.007899312, 319.4466)
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2929984, 0.04120036, 312.1162),
                sentQuote: oklch(0.3876602, 0.07087001, 315.7654),
                receivedMessage: oklch(0.2678179, 0.006190444, 314.7144),
                receivedQuote: oklch(0.3435397, 0.01317027, 310.9424)
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3234681, 0.09690244, 299.9634),
                sentQuote: oklch(0.4226042, 0.1341495, 307.8573),
                receivedMessage: oklch(0.2812692, 0.03669397, 281.5485),
                receivedQuote: oklch(0.355058, 0.03791292, 286.3773)
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3234681, 0.09690244, 299.9634),
                sentQuote: oklch(0.4226042, 0.1341495, 307.8573),
                receivedMessage: oklch(0.2454222, 0.009540156, 325.8636),
                receivedQuote: oklch(0.2874049, 0.0149843, 302.5009)
            )
        ]
        }
    }

    public static func from(_ filename: String) -> PresetWallpaper? {
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

public enum WallpaperScaleType: String, Codable, CaseIterable {
    case fill
    case fit
    case `repeat`

    public var text: String {
        switch self {
        case .fill: "Fill"
        case .fit: "Fit"
        case .repeat: "Repeat"
        }
    }

    public func computeScaleFactor(_ srcSize: CGSize, _ dstSize: CGSize) -> (CGFloat, CGFloat) {
        switch self {
        case .fill:
            let widthScale = dstSize.width / srcSize.width
            let heightScale = dstSize.height / srcSize.height
            return (max(widthScale, heightScale), max(widthScale, heightScale))
        case .fit: fallthrough
        case .repeat:
            let widthScale = dstSize.width / srcSize.width
            let heightScale = dstSize.height / srcSize.height
            return (min(widthScale, heightScale), min(widthScale, heightScale))
        }
    }
}

// Spec: spec/services/theme.md#WallpaperType
public enum WallpaperType: Equatable {
    public var image: SwiftUI.Image? {
        if let uiImage {
            return SwiftUI.Image(uiImage: uiImage)
        }
        return nil
    }

    public var uiImage: UIImage? {
        let filename: String
        switch self {
        case let .preset(f, _): filename = f
        case let .image(f, _, _): filename = f
        default: return nil
        }
        if filename == "" { return nil }
        if let image = WallpaperType.cachedImages[filename] {
            return image
        } else {
            let res: UIImage?
            if case let .preset(filename, _) = self {
                res = (PresetWallpaper.from(filename) ?? PresetWallpaper.cats).res
            } else {
                // In case of unintentional image deletion don't crash the app
                res = UIImage(contentsOfFile: getWallpaperFilePath(filename).path)
            }
            if let res {
                WallpaperType.cachedImages[filename] = res
            }
            return res
        }
    }

    public func sameType(_ other: WallpaperType?) -> Bool {
        if case let .preset(filename, _) = self, case let .preset(otherFilename, _) = other { filename == otherFilename }
        else if case .image = self, case .image = other { true }
        else if case .empty = self, case .empty = other { true }
        else { false }
    }

    public var isPreset: Bool { switch self { case .preset: true; default: false } }

    public var isImage: Bool { switch self { case .image: true; default: false } }

    public var isEmpty: Bool { switch self { case .empty: true; default: false } }

    public var scale: Float { 
        switch self {
        case let .preset(_, scale): scale ?? 1
        case let .image(_, scale, _): scale ?? 1
        case .empty: 1
        }
    }

    public func samePreset(other: PresetWallpaper?) -> Bool { if case let .preset(filename, _) = self, filename == other?.filename { true } else { false } }

    case preset(_ filename: String, _ scale: Float?)

    case image(_ filename: String, _ scale: Float?, _ scaleType: WallpaperScaleType?)

    case empty

    public func defaultBackgroundColor(_ theme: DefaultTheme, _ themeBackground: Color) -> Color {
        if case let .preset(filename, _) = self {
            (PresetWallpaper.from(filename) ?? PresetWallpaper.cats).background[theme]!
        } else {
            themeBackground
        }
    }

    public func defaultTintColor(_ theme: DefaultTheme) -> Color {
        if case let .preset(filename, _) = self {
            (PresetWallpaper.from(filename) ?? PresetWallpaper.cats).tint[theme]!
        } else if case let .image(_, _, scaleType) = self, scaleType == WallpaperScaleType.repeat {
            Color.clear
        } else {
            Color.clear
        }
    }

    public static var cachedImages: [String: UIImage] = [:]

    public static func from(_ wallpaper: ThemeWallpaper?) -> WallpaperType? {
        if wallpaper == nil {
            return nil
        } else if let preset = wallpaper?.preset {
            return WallpaperType.preset(preset, wallpaper?.scale)
        } else if let imageFile = wallpaper?.imageFile {
            return WallpaperType.image(imageFile, wallpaper?.scale, wallpaper?.scaleType)
        } else {
            return WallpaperType.empty
        }
    }
}
