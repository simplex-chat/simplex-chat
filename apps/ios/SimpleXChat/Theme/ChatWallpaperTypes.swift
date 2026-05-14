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
        case .cats: wallpaperBackgrounds(light: oklch(0.9714242, 0.01596467, 98.99223))
        case .flowers: wallpaperBackgrounds(light: oklch(0.9718878, 0.04671557, 147.1246))
        case .hearts: wallpaperBackgrounds(light: oklch(0.9565624, 0.01848713, 17.48077))
        case .kids: wallpaperBackgrounds(light: oklch(0.9693045, 0.03516977, 192.2433))
        case .school: wallpaperBackgrounds(light: oklch(0.9626785, 0.02004578, 238.6614))
        case .travel: wallpaperBackgrounds(light: oklch(0.9626377, 0.0253131, 313.9639))
        }
    }

    public var tint: [DefaultTheme: Color] {
        switch self {
        case .cats: [
            DefaultTheme.LIGHT: oklch(0.897064, 0.07281305, 90.95935), // #ffefdca6
            DefaultTheme.DARK: oklch(0.3603656, 0.0643012, 88.54155), // #ff4b3b0e
            DefaultTheme.SIMPLEX: oklch(0.3797781, 0.06842897, 88.88896), // #ff51400f
            DefaultTheme.BLACK: oklch(0.3603656, 0.0643012, 88.54155) // #ff4b3b0e
        ]
        case .flowers: [
            DefaultTheme.LIGHT: oklch(0.8574244, 0.1932141, 133.0531), // #ff9CEA59
            DefaultTheme.DARK: oklch(0.409874, 0.1074549, 133.4271), // #ff31560D
            DefaultTheme.SIMPLEX: oklch(0.4415422, 0.1170956, 133.8571), // #ff36600f
            DefaultTheme.BLACK: oklch(0.409874, 0.1074549, 133.4271) // #ff31560D
        ]
        case .hearts: [
            DefaultTheme.LIGHT: oklch(0.9304586, 0.03207239, 17.7425), // #fffde0e0
            DefaultTheme.DARK: oklch(0.2458526, 0.07098409, 23.94782), // #ff3C0F0F
            DefaultTheme.SIMPLEX: oklch(0.2574974, 0.07614605, 24.19117), // #ff411010
            DefaultTheme.BLACK: oklch(0.2458526, 0.07098409, 23.94782) // #ff3C0F0F
        ]
        case .kids: [
            DefaultTheme.LIGHT: oklch(0.9123625, 0.06815507, 211.1344), // #ffadeffc
            DefaultTheme.DARK: oklch(0.3473769, 0.04958945, 218.0525), // #ff16404B
            DefaultTheme.SIMPLEX: oklch(0.3716418, 0.05389406, 217.7104), // #ff184753
            DefaultTheme.BLACK: oklch(0.3473769, 0.04958945, 218.0525) // #ff16404B
        ]
        case .school: [
            DefaultTheme.LIGHT: oklch(0.9252349, 0.04096641, 238.0518), // #ffCEEBFF
            DefaultTheme.DARK: oklch(0.2700986, 0.04630937, 241.5568), // #ff0F293B
            DefaultTheme.SIMPLEX: oklch(0.2929108, 0.05102392, 240.8139), // #ff112f43
            DefaultTheme.BLACK: oklch(0.2700986, 0.04630937, 241.5568) // #ff0F293B
        ]
        case .travel: [
            DefaultTheme.LIGHT: oklch(0.9174161, 0.05105522, 309.6281), // #ffeedbfe
            DefaultTheme.DARK: oklch(0.2817417, 0.07665313, 302.6645), // #ff311E48
            DefaultTheme.SIMPLEX: oklch(0.2948376, 0.08277514, 302.7197), // #ff35204e
            DefaultTheme.BLACK: oklch(0.2817417, 0.07665313, 302.6645) // #ff311E48
        ]
        }
    }

    public var colors: [DefaultTheme: ResolvedColors] {
        switch self {
        case .cats: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9854474, 0.01790464, 89.3544), // #fffffaed
                sentQuote: oklch(0.9562038, 0.0357691, 89.44265), // #fffaf0d6
                receivedMessage: oklch(0.9760699, 0.004115805, 91.44609), // #ffF8F7F4
                receivedQuote: oklch(0.9465333, 0.005762915, 84.56661) // #ffefede9
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2827141, 0.02844628, 89.80136), // #ff2f2919
                sentQuote: oklch(0.3550253, 0.04770112, 85.80835), // #ff473a1d
                receivedMessage: oklch(0.2689313, 0.003935312, 84.58291), // #ff272624
                receivedQuote: oklch(0.332832, 0.005361989, 91.54412) // #ff373633
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3402031, 0.04537511, 90.2498), // #ff41371b
                sentQuote: oklch(0.4398707, 0.0737883, 85.23908), // #ff654f1c
                receivedMessage: oklch(0.2689313, 0.003935312, 84.58291), // #ff272624
                receivedQuote: oklch(0.332832, 0.005361989, 91.54412) // #ff373633
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3402031, 0.04537511, 90.2498), // #ff41371b
                sentQuote: oklch(0.4398707, 0.0737883, 85.23908), // #ff654f1c
                receivedMessage: oklch(0.2349937, 0.005828091, 91.60813), // #ff1f1e1b
                receivedQuote: oklch(0.2971596, 0.01092985, 91.6846) // #ff2f2d27
            )
        ]
        case .flowers: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9827452, 0.03710413, 130.3627), // #fff1ffe5
                sentQuote: oklch(0.9477894, 0.07588911, 131.1257), // #ffdcf9c4
                receivedMessage: oklch(0.9744452, 0.008958742, 134.8726), // #ffF4F8F2
                receivedQuote: oklch(0.9378814, 0.008518542, 145.5074) // #ffe7ece7
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2986395, 0.05211595, 153.5889), // #ff163521
                sentQuote: oklch(0.3954021, 0.08319059, 152.8037), // #ff1B5330
                receivedMessage: oklch(0.2626721, 0.003936427, 128.6285), // #ff242523
                receivedQuote: oklch(0.3334174, 0.007411477, 128.7105) // #ff353733
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3611755, 0.05678164, 170.3752), // #ff184739
                sentQuote: oklch(0.484029, 0.09629127, 159.5568), // #ff1F6F4B
                receivedMessage: oklch(0.2626721, 0.003936427, 128.6285), // #ff242523
                receivedQuote: oklch(0.3334174, 0.007411477, 128.7105) // #ff353733
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3611755, 0.05678164, 170.3752),
                sentQuote: oklch(0.484029, 0.09629127, 159.5568), // #ff1F6F4B
                receivedMessage: oklch(0.2342548, 0.01039849, 132.6996), // #ff1c1f1a
                receivedQuote: oklch(0.2838948, 0.01154375, 128.9221) // #ff282b25
            )
        ]
        case .hearts: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9757184, 0.01184164, 17.35934), // #fffff4f4
                sentQuote: oklch(0.9300344, 0.0354728, 17.80723), // #ffffdfdf
                receivedMessage: oklch(0.9746758, 0.002137086, 17.19433), // #fff8f6f6
                receivedQuote: oklch(0.9431687, 0.004317648, 17.23361) // #ffefebeb
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2353791, 0.04398437, 20.94719), // #ff301515
                sentQuote: oklch(0.2920391, 0.07914221, 23.35544), // #ff4C1818
                receivedMessage: oklch(0.2510736, 0.004554155, 17.46058), // #ff242121
                receivedQuote: oklch(0.3352158, 0.008515606, 17.58481) // #ff3b3535
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.2941874, 0.07322977, 4.102547), // #ff491A28
                sentQuote: oklch(0.3831088, 0.1201278, 18.61089), // #ff761F29
                receivedMessage: oklch(0.2510736, 0.004554155, 17.46058), // #ff242121
                receivedQuote: oklch(0.3352158, 0.008515606, 17.58481) // #ff3b3535
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.2941874, 0.07322977, 4.102547), // #ff491A28
                sentQuote: oklch(0.3831088, 0.1201278, 18.61089), // #ff761F29
                receivedMessage: oklch(0.2267386, 0.00626924, 17.6236), // #ff1f1b1b
                receivedQuote: oklch(0.2776199, 0.012034, 17.89987) // #ff2e2626
            )
        ]
        case .kids: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9827091, 0.02093746, 200.4479), // #ffeafeff
                sentQuote: oklch(0.9392156, 0.04239295, 201.9221), // #ffcbf4f7
                receivedMessage: oklch(0.9798523, 0.007408877, 197.0357), // #fff3fafa
                receivedQuote: oklch(0.9438775, 0.0117012, 196.9581)
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2881511, 0.03214503, 192.2759), // #ff16302F
                sentQuote: oklch(0.3764664, 0.05129536, 193.292), // #ff1a4a49
                receivedMessage: oklch(0.2675764, 0.001466786, 197.0692), // #ff252626
                receivedQuote: oklch(0.3451987, 0.004436687, 174.2088) // #ff373a39
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3662882, 0.04909204, 191.2229), // #ff1a4745
                sentQuote: oklch(0.4817563, 0.07299667, 192.4874), // #ff1d6b69
                receivedMessage: oklch(0.2675764, 0.001466786, 197.0692), // #ff252626
                receivedQuote: oklch(0.3451987, 0.004436687, 174.2088) // #ff373a39
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3662882, 0.04909204, 191.2229), // #ff1a4745
                sentQuote: oklch(0.4817563, 0.07299667, 192.4874), // #ff1d6b69
                receivedMessage: oklch(0.2382215, 0.001508911, 197.0555), // #ff1e1f1f
                receivedQuote: oklch(0.2833724, 0.007955636, 169.798) // #ff262b29
            )
        ]
        case .school: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9756479, 0.01416295, 231.2013), // #ffeef9ff
                sentQuote: oklch(0.9331527, 0.03006113, 232.4212), // #ffD6EDFA
                receivedMessage: oklch(0.9697657, 0.005748723, 264.5325), // #ffF3F5F9
                receivedQuote: oklch(0.9296755, 0.00918803, 258.3366) // #ffe4e8ee
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.267226, 0.03061943, 237.8609), // #ff172833
                sentQuote: oklch(0.3464064, 0.04943852, 232.4005), // #ff1C3E4F
                receivedMessage: oklch(0.2764251, 0.007910622, 264.4375), // #ff26282c
                receivedQuote: oklch(0.3548081, 0.008034593, 255.5451) // #ff393c40
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3481476, 0.07023845, 249.9259), // #ff1A3C5D
                sentQuote: oklch(0.4520089, 0.08394516, 241.1934), // #ff235b80
                receivedMessage: oklch(0.2764251, 0.007910622, 264.4375), // #ff26282c
                receivedQuote: oklch(0.3548081, 0.008034593, 255.5451) // #ff393c40
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3481476, 0.07023845, 249.9259), // #ff1A3C5D
                sentQuote: oklch(0.4520089, 0.08394516, 241.1934), // #ff235b80
                receivedMessage: oklch(0.2356588, 0.007789041, 274.6063), // #ff1d1e22
                receivedQuote: oklch(0.2886546, 0.007823012, 264.445) // #ff292b2f
            )
        ]
        case .travel: [
            DefaultTheme.LIGHT: ResolvedColors(
                sentMessage: oklch(0.9803204, 0.01342671, 314.7601), // #fffcf6ff
                sentQuote: oklch(0.9294779, 0.04197705, 313.6968), // #fff2e0fc
                receivedMessage: oklch(0.9695303, 0.004487354, 314.8044), // #ffF6F4F7
                receivedQuote: oklch(0.9385522, 0.007899312, 319.4466) // #ffede9ee
            ),
            DefaultTheme.DARK: ResolvedColors(
                sentMessage: oklch(0.2929984, 0.04120036, 312.1162), // #ff33263B
                sentQuote: oklch(0.3876602, 0.07087001, 315.7654), // #ff53385E
                receivedMessage: oklch(0.2678179, 0.006190444, 314.7144), // #ff272528
                receivedQuote: oklch(0.3435397, 0.01317027, 310.9424) // #ff3B373E
            ),
            DefaultTheme.SIMPLEX: ResolvedColors(
                sentMessage: oklch(0.3234681, 0.09690244, 299.9634), // #ff3C255D
                sentQuote: oklch(0.4226042, 0.1341495, 307.8573), // #ff623485
                receivedMessage: oklch(0.2812692, 0.03669397, 281.5485), // #ff26273B
                receivedQuote: oklch(0.355058, 0.03791292, 286.3773) // #ff3A394F
            ),
            DefaultTheme.BLACK: ResolvedColors(
                sentMessage: oklch(0.3234681, 0.09690244, 299.9634), // #ff3C255D
                sentQuote: oklch(0.4226042, 0.1341495, 307.8573), // #ff623485
                receivedMessage: oklch(0.2454222, 0.009540156, 325.8636), // #ff231f23
                receivedQuote: oklch(0.2874049, 0.0149843, 302.5009) // #ff2c2931
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

func wallpaperBackgrounds(light: Color) -> [DefaultTheme : Color] {
    [
        DefaultTheme.LIGHT: light,
        DefaultTheme.DARK: oklch(0.1822037, 0, 0), // #ff121212
        DefaultTheme.SIMPLEX: oklch(0.2024453, 0.03849037, 273.4875), // #ff111528
        DefaultTheme.BLACK: oklch(0.1285578, 0, 0) // #ff070707
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
