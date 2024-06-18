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

class ThemeManager {
    struct ActiveTheme {
        let name: String
        let base: DefaultTheme
        let colors: Colors
        let appColors: AppColors
        var wallpaper: AppWallpaper = AppWallpaper(background: nil, tint: nil, type: .Empty)

        func toAppTheme() -> AppTheme {
            AppTheme(name: name, base: base, colors: colors, appColors: appColors, wallpaper: wallpaper)
        }
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
        return ThemeModeOverride(mode: CurrentColors.base.mode, colors: defaultTheme?.colors ?? ThemeColors(), wallpaper: defaultTheme?.wallpaper)
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
        applyTheme(currentThemeDefault.get())
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
                pref.wrappedValue = ThemeModeOverride(mode: CurrentColors.base.mode, wallpaper: w)
            } else {
                // Make an empty wallpaper to override any top level ones
                pref.wrappedValue = ThemeModeOverride(mode: CurrentColors.base.mode, wallpaper: ThemeWallpaper())
            }
            return true
        }
        var type = sameWallpaper.toAppWallpaper().type
        if case let WallpaperType.Image(filename, scale, scaleType) = type, sameWallpaper.imageFile == filename {
            // same image file. Needs to be copied first in order to be able to remove the file once it's not needed anymore without affecting main theme override
            if let filename = saveWallpaperFile(url: getWallpaperFilePath(filename)) {
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
        if let imageFile = prevValue.wallpaper?.imageFile {
            try? FileManager.default.removeItem(at: getWallpaperFilePath(imageFile))
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
