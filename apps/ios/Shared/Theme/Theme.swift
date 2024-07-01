//
//  Theme.swift
//  SimpleX (iOS)
//
//  Created by Avently on 14.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat

var CurrentColors: ThemeManager.ActiveTheme = ThemeManager.currentColors(nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())

var MenuTextColor: Color { if isInDarkTheme() { AppTheme.shared.colors.onBackground.opacity(0.8) } else { Color.black } }
var NoteFolderIconColor: Color { AppTheme.shared.appColors.primaryVariant2 }

func isInDarkTheme() -> Bool { !CurrentColors.colors.isLight }

class AppTheme: ObservableObject, Equatable {
    static let shared = AppTheme(name: CurrentColors.name, base: CurrentColors.base, colors: CurrentColors.colors, appColors: CurrentColors.appColors, wallpaper: CurrentColors.wallpaper)

    var name: String
    var base: DefaultTheme
    @ObservedObject var colors: Colors
    @ObservedObject var appColors: AppColors
    @ObservedObject var wallpaper: AppWallpaper

    init(name: String, base: DefaultTheme, colors: Colors, appColors: AppColors, wallpaper: AppWallpaper) {
        self.name = name
        self.base = base
        self.colors = colors
        self.appColors = appColors
        self.wallpaper = wallpaper
    }

    static func == (lhs: AppTheme, rhs: AppTheme) -> Bool {
        lhs.name == rhs.name &&
        lhs.colors == rhs.colors &&
        lhs.appColors == rhs.appColors &&
        lhs.wallpaper == rhs.wallpaper
    }

    func updateFromCurrentColors() {
        objectWillChange.send()
        name = CurrentColors.name
        base = CurrentColors.base
        colors.updateColorsFrom(CurrentColors.colors)
        appColors.updateColorsFrom(CurrentColors.appColors)
        wallpaper.updateWallpaperFrom(CurrentColors.wallpaper)
    }
}

struct ThemedBackground: ViewModifier {
    @EnvironmentObject var theme: AppTheme
    var grouped: Bool = false

    func body(content: Content) -> some View {
        content
            .background(
                theme.base == DefaultTheme.SIMPLEX
                ? LinearGradient(
                    colors: [
                        grouped
                        ? theme.colors.background.lighter(0.4).asGroupedBackground(theme.base.mode)
                        : theme.colors.background.lighter(0.4),
                        grouped
                        ? theme.colors.background.darker(0.4).asGroupedBackground(theme.base.mode)
                        : theme.colors.background.darker(0.4)
                    ],
                    startPoint: .topLeading,
                    endPoint: .bottomTrailing
                )
                : LinearGradient(
                    colors: [],
                    startPoint: .topLeading,
                    endPoint: .bottomTrailing
                )
            )
            .background(
                theme.base == DefaultTheme.SIMPLEX
                ? Color.clear
                : grouped
                ? theme.colors.background.asGroupedBackground(theme.base.mode)
                : theme.colors.background
            )
    }
}

var systemInDarkThemeCurrently: Bool {
    return UITraitCollection.current.userInterfaceStyle == .dark
}

func reactOnDarkThemeChanges() {
    if currentThemeDefault.get() == DefaultTheme.SYSTEM_THEME_NAME && CurrentColors.colors.isLight == systemInDarkThemeCurrently {
        // Change active colors from light to dark and back based on system theme
        ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
    }
}

extension ThemeWallpaper {
    public func importFromString() -> ThemeWallpaper {
        if preset == nil, let image {
            // Need to save image from string and to save its path
            if let data = Data(base64Encoded: dropImagePrefix(image)),
               let parsed = UIImage(data: data),
               let filename = saveWallpaperFile(image: parsed) {
                var copy = self
                copy.image = nil
                copy.imageFile = filename
                return copy
            } else {
                return ThemeWallpaper()
            }
        } else {
            return self
        }
    }

    func withFilledWallpaperBase64() -> ThemeWallpaper {
        let aw = toAppWallpaper()
        let type = aw.type
        let preset: String? = if case let WallpaperType.preset(filename, _) = type { filename } else { nil }
        let scale: Float? = if case let WallpaperType.preset(_, scale) = type { scale } else { if case let WallpaperType.image(_, scale, _) = type { scale } else { 1.0 } }
        let scaleType: WallpaperScaleType? = if case let WallpaperType.image(_, _, scaleType) = type { scaleType } else { nil }
        let image: String? = if case WallpaperType.image = type, let image = type.uiImage { resizeImageToStrSize(image, maxDataSize: 5_000_000) } else { nil }
        return ThemeWallpaper (
            preset: preset,
            scale: scale,
            scaleType: scaleType,
            background: aw.background?.toReadableHex(),
            tint: aw.tint?.toReadableHex(),
            image: image,
            imageFile: nil
        )
    }
}

extension ThemeModeOverride {
    func removeSameColors(_ base: DefaultTheme, colorsToCompare tc: ThemeColors) -> ThemeModeOverride {
        let wallpaperType = WallpaperType.from(wallpaper) ?? WallpaperType.empty
        let w: ThemeWallpaper
        switch wallpaperType {
        case let WallpaperType.preset(filename, scale):
            let p = PresetWallpaper.from(filename)
            w = ThemeWallpaper(
                preset: filename,
                scale: scale ?? wallpaper?.scale,
                scaleType: nil,
                background: p?.background[base]?.toReadableHex(),
                tint: p?.tint[base]?.toReadableHex(),
                image: nil,
                imageFile: nil
            )
        case WallpaperType.image:
            w = ThemeWallpaper(
                preset: nil,
                scale: nil,
                scaleType: WallpaperScaleType.fill,
                background: Color.clear.toReadableHex(),
                tint: Color.clear.toReadableHex(),
                image: nil,
                imageFile: nil
            )
        default:
            w = ThemeWallpaper()
        }
        let wallpaper: ThemeWallpaper? = if let wallpaper {
            ThemeWallpaper(
                preset: wallpaper.preset,
                scale: wallpaper.scale != w.scale ? wallpaper.scale : nil,
                scaleType: wallpaper.scaleType != w.scaleType ? wallpaper.scaleType : nil,
                background: wallpaper.background != w.background ? wallpaper.background : nil,
                tint: wallpaper.tint != w.tint ? wallpaper.tint : nil,
                image: wallpaper.image,
                imageFile: wallpaper.imageFile
            )
        } else {
            nil
        }
        return ThemeModeOverride(
            mode: self.mode,
            colors: ThemeColors(
                primary: colors.primary != tc.primary ? colors.primary : nil,
                primaryVariant: colors.primaryVariant != tc.primaryVariant ? colors.primaryVariant : nil,
                secondary: colors.secondary != tc.secondary ? colors.secondary : nil,
                secondaryVariant: colors.secondaryVariant != tc.secondaryVariant ? colors.secondaryVariant : nil,
                background: colors.background != tc.background ? colors.background : nil,
                surface: colors.surface != tc.surface ? colors.surface : nil,
                title: colors.title != tc.title ? colors.title : nil,
                primaryVariant2: colors.primaryVariant2 != tc.primaryVariant2 ? colors.primary : nil,
                sentMessage: colors.sentMessage != tc.sentMessage ? colors.sentMessage : nil,
                sentQuote: colors.sentQuote != tc.sentQuote ? colors.sentQuote : nil,
                receivedMessage: colors.receivedMessage != tc.receivedMessage ? colors.receivedMessage : nil,
                receivedQuote: colors.receivedQuote != tc.receivedQuote ? colors.receivedQuote : nil
            ),
            wallpaper: wallpaper
        )
    }
}
