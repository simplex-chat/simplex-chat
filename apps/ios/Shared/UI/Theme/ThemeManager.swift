//
//  ThemeManager.swift
//  SimpleX (iOS)
//
//  Created by Avently on 03.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat

public class ThemeManager {
    public struct ActiveTheme {
        let name: String
        let base: DefaultTheme
        let colors: Colors
        let appColors: AppColors
        var wallpaper: AppWallpaper = AppWallpaper(background: nil, tint: nil, type: .Empty)
    }

    private static func systemDarkThemeColors() -> (Colors, DefaultTheme) {
        switch systemDarkThemeDefault.get() {
        case DefaultTheme.DARK.themeName: (DarkColorPalette, DefaultTheme.DARK)
        case DefaultTheme.SIMPLEX.themeName: (SimplexColorPalette, DefaultTheme.SIMPLEX)
        case DefaultTheme.BLACK.themeName: (BlackColorPalette, DefaultTheme.BLACK)
        default: (SimplexColorPalette, DefaultTheme.SIMPLEX)
        }
    }

    private static func nonSystemThemeName() -> String {
        let themeName = currentThemeDefault.get()
        return if themeName != DefaultTheme.SYSTEM_THEME_NAME {
            themeName
        } else {
            systemInDarkThemeCurrently ? systemDarkThemeDefault.get() : DefaultTheme.LIGHT.themeName
        }
    }

    static func defaultActiveTheme(_ appSettingsTheme: [ThemeOverrides]) -> ThemeOverrides? {
        let nonSystemThemeName = nonSystemThemeName()
        let defaultThemeId = currentThemeIdsDefault.get()[nonSystemThemeName]
        return appSettingsTheme.getTheme(defaultThemeId)
    }

    static func defaultActiveTheme(_ perUserTheme: ThemeModeOverrides?, _ appSettingsTheme: [ThemeOverrides]) -> ThemeModeOverride {
        let perUserTheme = !CurrentColors.colors.isLight ? perUserTheme?.dark : perUserTheme?.light
        if let perUserTheme {
            return perUserTheme
        }
        let defaultTheme = defaultActiveTheme(appSettingsTheme)
        return ThemeModeOverride(colors: defaultTheme?.colors ?? ThemeColors(), wallpaper: defaultTheme?.wallpaper)
    }

    static func currentColors(_ themeOverridesForType: WallpaperType?, _ perChatTheme: ThemeModeOverride?, _ perUserTheme: ThemeModeOverrides?, _ appSettingsTheme: [ThemeOverrides]) -> ActiveTheme {
        let themeName = currentThemeDefault.get()
        let nonSystemThemeName = nonSystemThemeName()
        let defaultTheme = defaultActiveTheme(appSettingsTheme)

        let baseTheme = switch nonSystemThemeName {
        case DefaultTheme.LIGHT.themeName: ActiveTheme(name: DefaultTheme.LIGHT.themeName, base: DefaultTheme.LIGHT, colors: LightColorPalette.clone(), appColors: LightColorPaletteApp.clone(), wallpaper: AppWallpaper(background: nil, tint: nil, type: PresetWallpaper.school.toType(DefaultTheme.LIGHT)))
        case DefaultTheme.DARK.themeName: ActiveTheme(name: DefaultTheme.DARK.themeName, base: DefaultTheme.DARK, colors: DarkColorPalette.clone(), appColors: DarkColorPaletteApp.clone(), wallpaper: AppWallpaper(background: nil, tint: nil, type: PresetWallpaper.school.toType(DefaultTheme.DARK)))
        case DefaultTheme.SIMPLEX.themeName: ActiveTheme(name: DefaultTheme.SIMPLEX.themeName, base: DefaultTheme.SIMPLEX, colors: SimplexColorPalette.clone(), appColors: SimplexColorPaletteApp.clone(), wallpaper: AppWallpaper(background: nil, tint: nil, type: PresetWallpaper.school.toType(DefaultTheme.SIMPLEX)))
        case DefaultTheme.BLACK.themeName: ActiveTheme(name: DefaultTheme.BLACK.themeName, base: DefaultTheme.BLACK, colors: BlackColorPalette.clone(), appColors: BlackColorPaletteApp.clone(), wallpaper: AppWallpaper(background: nil, tint: nil, type: PresetWallpaper.school.toType(DefaultTheme.BLACK)))
        default: ActiveTheme(name: DefaultTheme.LIGHT.themeName, base: DefaultTheme.LIGHT, colors: LightColorPalette.clone(), appColors: LightColorPaletteApp.clone(), wallpaper: AppWallpaper(background: nil, tint: nil, type: PresetWallpaper.school.toType(DefaultTheme.LIGHT)))
        }

        let perUserTheme = baseTheme.colors.isLight ? perUserTheme?.light : perUserTheme?.dark
        let theme = appSettingsTheme.sameTheme(themeOverridesForType ?? perChatTheme?.type ?? perUserTheme?.type ?? defaultTheme?.wallpaper?.toAppWallpaper().type, nonSystemThemeName) ?? defaultTheme

        if theme == nil && perUserTheme == nil && perChatTheme == nil && themeOverridesForType == nil {
            return ActiveTheme(name: themeName, base: baseTheme.base, colors: baseTheme.colors, appColors: baseTheme.appColors, wallpaper: baseTheme.wallpaper)
        }
        let presetWallpaperTheme: ThemeColors? = if let wallpaper = perChatTheme?.wallpaper {
            if let preset = wallpaper.preset { PresetWallpaper.from(preset)?.colors[baseTheme.base] } else { nil }
        } else if let wallpaper = perUserTheme?.wallpaper {
            if let preset = wallpaper.preset { PresetWallpaper.from(preset)?.colors[baseTheme.base] } else { nil }
        } else {
            if let preset = theme?.wallpaper?.preset { PresetWallpaper.from(preset)?.colors[baseTheme.base] } else { nil }
        }

        let themeOrEmpty = theme ?? ThemeOverrides(base: baseTheme.base)
        let colors = themeOrEmpty.toColors(themeOrEmpty.base, perChatTheme?.colors, perUserTheme?.colors, presetWallpaperTheme)
        return ActiveTheme(
            name: themeName,
            base: baseTheme.base,
            colors: colors,
            appColors: themeOrEmpty.toAppColors(themeOrEmpty.base, perChatTheme?.colors, perChatTheme?.type, perUserTheme?.colors, perUserTheme?.type, presetWallpaperTheme),
            wallpaper: themeOrEmpty.toAppWallpaper(themeOverridesForType, perChatTheme, perUserTheme, colors.background)
        )
    }

    static func currentThemeOverridesForExport(_ perChatTheme: ThemeModeOverride?, _ perUserTheme: ThemeModeOverrides?) -> ThemeOverrides {
        let current = currentColors(nil, perChatTheme, perUserTheme, themeOverridesDefault.get())
        let wType = current.wallpaper.type
        let wBackground = current.wallpaper.background
        let wTint = current.wallpaper.tint
        let w: ThemeWallpaper? = if case WallpaperType.Empty = wType { 
            nil
        } else {
            ThemeWallpaper.from(wType, wBackground?.toReadableHex(), wTint?.toReadableHex()).withFilledWallpaperBase64()
        }
        return ThemeOverrides(
            themeId: "",
            base: current.base,
            colors: ThemeColors.from(current.colors, current.appColors),
            wallpaper: w
        )
    }

    static func applyTheme(_ theme: String) {
        currentThemeDefault.set(theme)
        CurrentColors = currentColors(nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
        SceneDelegate.windowStatic?.tintColor = UIColor(CurrentColors.colors.primary)
        SceneDelegate.windowStatic?.backgroundColor = UIColor(CurrentColors.colors.background)
        SceneDelegate.windowStatic?.overrideUserInterfaceStyle = switch currentThemeDefault.get() {
        case DefaultTheme.LIGHT.themeName: .light
        case DefaultTheme.SYSTEM_THEME_NAME: .unspecified
        default: .dark
        }
    }

    static func changeDarkTheme(_ theme: String) {
        systemDarkThemeDefault.set(theme)
        CurrentColors = currentColors(nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
    }

    static func saveAndApplyThemeColor(_ baseTheme: DefaultTheme, _ name: ThemeColor, _ color: Color? = nil, _ pref: CodableDefault<[ThemeOverrides]>? = nil) {
        let nonSystemThemeName = baseTheme.themeName
        let pref = pref ?? themeOverridesDefault
        let overrides = pref.get()
        let themeId = currentThemeIdsDefault.get()[nonSystemThemeName]
        let prevValue = overrides.getTheme(themeId) ?? ThemeOverrides(base: baseTheme)
        pref.set(overrides.replace(prevValue.withUpdatedColor(name, color?.toReadableHex())))
        var themeIds = currentThemeIdsDefault.get()
        themeIds[nonSystemThemeName] = prevValue.themeId
        currentThemeIdsDefault.set(themeIds)
        CurrentColors = currentColors(nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
    }

    static func applyThemeColor(name: ThemeColor, color: Color? = nil, pref: Binding<ThemeModeOverride>) {
        pref.wrappedValue = pref.wrappedValue.withUpdatedColor(name, color?.toReadableHex())
    }

    static func saveAndApplyWallpaper(_ baseTheme: DefaultTheme, _ type: WallpaperType?, _ pref: CodableDefault<[ThemeOverrides]>?) {
        let nonSystemThemeName = baseTheme.themeName
        let pref = pref ?? themeOverridesDefault
        let overrides = pref.get()
        let theme = overrides.sameTheme(type, baseTheme.themeName)
        var prevValue = theme ?? ThemeOverrides(base: baseTheme)
        prevValue.wallpaper = if let type {
            if case WallpaperType.Empty = type {
                nil as ThemeWallpaper?
            } else {
                ThemeWallpaper.from(type, prevValue.wallpaper?.background, prevValue.wallpaper?.tint)
            }
        } else {
            nil
        }
        pref.set(overrides.replace(prevValue))
        var themeIds = currentThemeIdsDefault.get()
        themeIds[nonSystemThemeName] = prevValue.themeId
        currentThemeIdsDefault.set(themeIds)
        CurrentColors = currentColors( nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
    }

    static func copyFromSameThemeOverrides(_ type: WallpaperType?, _ lowerLevelOverride: ThemeModeOverride?, _ pref: Binding<ThemeModeOverride>) -> Bool {
        let overrides = themeOverridesDefault.get()
        let sameWallpaper: ThemeWallpaper? = if let wallpaper = lowerLevelOverride?.wallpaper, lowerLevelOverride?.type?.sameType(type) == true {
            wallpaper
        } else {
            overrides.sameTheme(type, CurrentColors.base.themeName)?.wallpaper
        }
        guard let sameWallpaper else {
            if let type {
                var w: ThemeWallpaper = ThemeWallpaper.from(type, nil, nil)
                w.scale = nil
                w.scaleType = nil
                pref.wrappedValue = ThemeModeOverride(wallpaper: w)
            } else {
                // Make an empty wallpaper to override any top level ones
                pref.wrappedValue = ThemeModeOverride(wallpaper: ThemeWallpaper())
            }
            return true
        }
        var type = sameWallpaper.toAppWallpaper().type
        if case let WallpaperType.Image(filename, scale, scaleType) = type, sameWallpaper.imageFile == filename {
            // same image file. Needs to be copied first in order to be able to remove the file once it's not needed anymore without affecting main theme override
            // LALAL
            let filename: String? = "LALAL"//saveWallpaperFile(File(getWallpaperFilePath(filename)).toURI())
            if let filename {
                type = WallpaperType.Image(filename, scale, scaleType)
            } else {
                logger.error("Error while copying wallpaper from global overrides to chat overrides")
                return false
            }
        }
        var prevValue = pref.wrappedValue
        var w = ThemeWallpaper.from(type, nil, nil)
        w.scale = nil
        w.scaleType = nil
        prevValue.colors = ThemeColors()
        prevValue.wallpaper = w
        pref.wrappedValue = prevValue
        return true
    }

    static func applyWallpaper(_ type: WallpaperType?, _ pref: Binding<ThemeModeOverride>) {
        var prevValue = pref.wrappedValue
        prevValue.wallpaper = if let type {
            ThemeWallpaper.from(type, prevValue.wallpaper?.background, prevValue.wallpaper?.tint)
        } else {
            nil
        }
        pref.wrappedValue = prevValue
    }

    static func saveAndApplyThemeOverrides(_ theme: ThemeOverrides, _ pref: CodableDefault<[ThemeOverrides]>? = nil) {
        let wallpaper = theme.wallpaper?.importFromString()
        let nonSystemThemeName = theme.base.themeName
        let pref: CodableDefault<[ThemeOverrides]> = pref ?? themeOverridesDefault
        let overrides = pref.get()
        var prevValue = overrides.getTheme(nil, wallpaper?.toAppWallpaper().type, theme.base) ?? ThemeOverrides(base: theme.base)
        if prevValue.wallpaper?.imageFile != nil {
            // LALAL
            //File(getWallpaperFilePath(prevValue.wallpaper.imageFile)).delete()
        }
        prevValue.base = theme.base
        prevValue.colors = theme.colors
        prevValue.wallpaper = wallpaper
        pref.set(overrides.replace(prevValue))
        currentThemeDefault.set(nonSystemThemeName)
        var currentThemeIds = currentThemeIdsDefault.get()
        currentThemeIds[nonSystemThemeName] = prevValue.themeId
        currentThemeIdsDefault.set(currentThemeIds)
        CurrentColors = currentColors(nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
    }

    static func resetAllThemeColors(_ pref: CodableDefault<[ThemeOverrides]>? = nil) {
        let nonSystemThemeName = nonSystemThemeName()
        let pref: CodableDefault<[ThemeOverrides]> = pref ?? themeOverridesDefault
        let overrides = pref.get()
        guard let themeId = currentThemeIdsDefault.get()[nonSystemThemeName],
              var prevValue = overrides.getTheme(themeId)
        else { return }
        prevValue.colors = ThemeColors()
        prevValue.wallpaper?.background = nil
        prevValue.wallpaper?.tint = nil
        pref.set(overrides.replace(prevValue))
        CurrentColors = currentColors(nil, nil, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
    }

    static func resetAllThemeColors(_ pref: Binding<ThemeModeOverride>) {
        var prevValue = pref.wrappedValue
        prevValue.colors = ThemeColors()
        prevValue.wallpaper?.background = nil
        prevValue.wallpaper?.tint = nil
        pref.wrappedValue = prevValue
    }

    static func removeTheme(_ themeId: String?) {
        var themes = themeOverridesDefault.get().map { $0 }
        themes.removeAll(where: { $0.themeId == themeId })
        themeOverridesDefault.set(themes)
    }
}

extension String {
    func colorFromReadableHex() -> Color {
        // https://stackoverflow.com/a/56874327
        let hex = self.trimmingCharacters(in: ["#", " "])
        var int: UInt64 = 0
        Scanner(string: hex).scanHexInt64(&int)
        let a, r, g, b: UInt64
        switch hex.count {
        case 3: // RGB (12-bit)
            (a, r, g, b) = (255, (int >> 8) * 17, (int >> 4 & 0xF) * 17, (int & 0xF) * 17)
        case 6: // RGB (24-bit)
            (a, r, g, b) = (255, int >> 16, int >> 8 & 0xFF, int & 0xFF)
        case 8: // ARGB (32-bit)
            (a, r, g, b) = (int >> 24, int >> 16 & 0xFF, int >> 8 & 0xFF, int & 0xFF)
        default:
            (a, r, g, b) = (1, 1, 1, 0)
        }

        return Color(
            .sRGB,
            red: Double(r) / 255,
            green: Double(g) / 255,
            blue:  Double(b) / 255,
            opacity: Double(a) / 255
        )
    }
}

extension Color {
    init(_ argb: Int64) {
        let a = Double((argb & 0xFF000000) >> 24) / 255.0
        let r = Double((argb & 0xFF0000) >> 16) / 255.0
        let g = Double((argb & 0xFF00) >> 8) / 255.0
        let b = Double((argb & 0xFF)) / 255.0
        self.init(.sRGB, red: r, green: g, blue: b, opacity: a)
    }

    init(_ r: Int, _ g: Int, _ b: Int, a: Int) {
        self.init(.sRGB, red: Double(r) / 255.0, green: Double(g) / 255.0, blue: Double(b) / 255.0, opacity: Double(a) / 255.0)
    }

    func toReadableHex() -> String {
        let uiColor: UIColor = .init(self)
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        uiColor.getRed(&r, green: &g, blue: &b, alpha: &a)
        return String(format: "#%02x%02x%02x%02x",
                      Int(a * 255),
                      Int(r * 255),
                      Int(g * 255),
                      Int(b * 255)
        )
    }

    func darker(_ factor: CGFloat = 0.1) -> Color {
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        UIColor(self).getRed(&r, green: &g, blue: &b, alpha: &a)
        return Color(.sRGB, red: max(r * (1 - factor), 0), green: max(g * (1 - factor), 0), blue: max(b * (1 - factor), 0), opacity: a)
    }

    func lighter(_ factor: CGFloat = 0.1) -> Color {
        var (r, g, b, a): (CGFloat, CGFloat, CGFloat, CGFloat) = (0, 0, 0, 0)
        UIColor(self).getRed(&r, green: &g, blue: &b, alpha: &a)
        return Color(.sRGB, red: min(r * (1 + factor), 1), green: min(g * (1 + factor), 1), blue: min(b * (1 + factor), 1), opacity: a)
    }

}
