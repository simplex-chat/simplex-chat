//
//  ThemeModeEditor.swift
//  SimpleX (iOS)
//
//  Created by Avently on 20.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat

struct UserWallpaperEditor: View {
    @EnvironmentObject var theme: AppTheme
    var initialTheme: ThemeModeOverride
    @State var themeModeOverride: ThemeModeOverride
    @State var applyToMode: DefaultThemeMode?
    @State var showMore: Bool = false
    @Binding var globalThemeUsed: Bool
    var save: (DefaultThemeMode?, ThemeModeOverride?) async -> Void

    @State private var showImageImporter: Bool = false

    var body: some View {
        List {
            let wallpaperType = theme.wallpaper.type

            WallpaperPresetSelector(
                selectedWallpaper: wallpaperType,
                currentColors: { type in
                    // If applying for :
                    // - all themes: no overrides needed
                    // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
                    let perUserOverride: ThemeModeOverrides? = wallpaperType.sameType(type) ? ChatModel.shared.currentUser?.uiThemes : nil
                    return ThemeManager.currentColors(type, nil, perUserOverride, themeOverridesDefault.get())
                },
                onChooseType: onChooseType
            )
            .padding(.bottom, 10)
            .listRowInsets(.init())
            .listRowBackground(Color.clear)
            .modifier(WallpaperImporter(showImageImporter: $showImageImporter, onChooseImage: { image in
                if let filename = saveWallpaperFile(image: image) {
                    onTypeChange(WallpaperType.Image(filename, 1, WallpaperScaleType.fill))
                }
            }))

            WallpaperSetupView(
                wallpaperType: themeModeOverride.type,
                base: theme.base,
                initialWallpaper: theme.wallpaper,
                editColor: { name in editColor(name, theme) },
                onTypeChange: onTypeChange
            )

            Section {
                if !globalThemeUsed {
                    ResetToGlobalThemeButton(true, theme.colors.primary) {
                        themeModeOverride = ThemeManager.defaultActiveTheme(ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
                        globalThemeUsed = true
                        Task {
                            await save(applyToMode, nil)
                            await MainActor.run {
                                // Change accent color globally
                                ThemeManager.applyTheme(currentThemeDefault.get())
                            }
                        }
                    }
                }

                SetDefaultThemeButton(theme.colors.primary) {
                    globalThemeUsed = false
                    let lightBase = DefaultTheme.LIGHT
                    let darkBase = if theme.base != DefaultTheme.LIGHT { theme.base } else if systemDarkThemeDefault.get() == DefaultTheme.DARK.themeName { DefaultTheme.DARK } else if systemDarkThemeDefault.get() == DefaultTheme.BLACK.themeName { DefaultTheme.BLACK } else { DefaultTheme.SIMPLEX }
                    let mode = themeModeOverride.mode
                    Task {
                        // Saving for both modes in one place by changing mode once per save
                        if applyToMode == nil {
                            let oppositeMode = mode == DefaultThemeMode.light ? DefaultThemeMode.dark : DefaultThemeMode.light
                            await save(oppositeMode, ThemeModeOverride.withFilledAppDefaults(oppositeMode, oppositeMode == DefaultThemeMode.light ? lightBase : darkBase))
                        }
                        await MainActor.run {
                            themeModeOverride = ThemeModeOverride.withFilledAppDefaults(mode, mode == DefaultThemeMode.light ? lightBase : darkBase)
                        }
                        await save(themeModeOverride.mode, themeModeOverride)
                        await MainActor.run {
                            // Change accent color globally
                            ThemeManager.applyTheme(currentThemeDefault.get())
                        }
                    }
                }.onChange(of: initialTheme.mode) { mode in
                    themeModeOverride = initialTheme
                    if applyToMode != nil {
                        applyToMode = mode
                    }
                }
                .onChange(of: theme) { _ in
                    // Applies updated global theme if current one tracks global theme
                    if globalThemeUsed {
                        themeModeOverride = ThemeManager.defaultActiveTheme(ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
                        globalThemeUsed = true
                    }
                }
            }

            if showMore {
                let values = [
                        (nil, "All modes"),
                        (DefaultThemeMode.light, "Light mode"),
                        (DefaultThemeMode.dark, "Dark mode")
                ]
                Picker("Apply to", selection: $applyToMode) {
                    ForEach(values, id: \.0) { (_, text) in
                        Text(text)
                    }
                }
                .frame(height: 36)
                .onChange(of: applyToMode) { mode in
                    if let mode, mode != theme.base.mode {
                        let lightBase = DefaultTheme.LIGHT
                        let darkBase = if theme.base != DefaultTheme.LIGHT { theme.base } else if systemDarkThemeDefault.get() == DefaultTheme.DARK.themeName { DefaultTheme.DARK } else if systemDarkThemeDefault.get() == DefaultTheme.BLACK.themeName { DefaultTheme.BLACK } else { DefaultTheme.SIMPLEX }
                        ThemeManager.applyTheme(mode == DefaultThemeMode.light ? lightBase.themeName : darkBase.themeName)
                    }
                }

                CustomizeThemeColorsSection(editColor: { name in editColor(name, theme) })

                ImportExportThemeSection(perChat: nil, perUser: ChatModel.shared.currentUser?.uiThemes) { theme in
                    let res = ThemeModeOverride(mode: theme.base.mode, colors: theme.colors, wallpaper: theme.wallpaper?.importFromString()).removeSameColors(theme.base)
                    Task {
                        await MainActor.run {
                            themeModeOverride = res
                        }
                        await save(applyToMode, res)
                    }
                }
            } else {
                AdvancedSettingsButton(theme.colors.primary) { showMore = true }
            }
        }
    }

    private func onTypeCopyFromSameTheme(_ type: WallpaperType?) -> Bool {
        _ = ThemeManager.copyFromSameThemeOverrides(type, nil, $themeModeOverride)
        Task {
            await save(applyToMode, themeModeOverride)
        }
        globalThemeUsed = false
        return true
    }

    private func preApplyGlobalIfNeeded(_ type: WallpaperType?) {
        if globalThemeUsed {
            _ = onTypeCopyFromSameTheme(type)
        }
    }

    private func onTypeChange(_ type: WallpaperType?) {
        if globalThemeUsed {
            preApplyGlobalIfNeeded(type)
            // Saves copied static image instead of original from global theme
            ThemeManager.applyWallpaper(themeModeOverride.type, $themeModeOverride)
        } else {
            ThemeManager.applyWallpaper(type, $themeModeOverride)
        }
        Task {
            await save(applyToMode, themeModeOverride)
        }
    }

    private func currentColors(_ type: WallpaperType?) -> ThemeManager.ActiveTheme {
        // If applying for :
        // - all themes: no overrides needed
        // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
        let perUserOverride: ThemeModeOverrides? = theme.wallpaper.type.sameType(type) ? ChatModel.shared.currentUser?.uiThemes : nil
        return ThemeManager.currentColors(type, nil, perUserOverride, themeOverridesDefault.get())
    }

    private func onChooseType(_ type: WallpaperType?) {
        if let type, case WallpaperType.Image = type {
            if theme.wallpaper.type.isImage || currentColors(type).wallpaper.type.image == nil {
                showImageImporter = true
            } else {
                _ = onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)
            }
        } else if themeModeOverride.type != type || theme.wallpaper.type != type {
            _ = onTypeCopyFromSameTheme(type)
        } else {
            onTypeChange(type)
        }
    }

    private func editColor(_ name: ThemeColor, _ currentTheme: AppTheme) -> Binding<Color> {
        editColorBinding(
            name: name,
            wallpaperType: theme.wallpaper.type,
            wallpaperImage: theme.wallpaper.type.image,
            theme: currentTheme,
            onColorChange: { color in
                preApplyGlobalIfNeeded(themeModeOverride.type)
                ThemeManager.applyThemeColor(name: name, color: color, pref: $themeModeOverride)
                Task { await save(applyToMode, themeModeOverride) }
            })
    }
}

struct ChatWallpaperEditor: View {
    @EnvironmentObject var theme: AppTheme
    @State private var currentTheme: ThemeManager.ActiveTheme
    var initialTheme: ThemeModeOverride
    @State var themeModeOverride: ThemeModeOverride
    @State var applyToMode: DefaultThemeMode?
    @State var showMore: Bool = false
    @Binding var globalThemeUsed: Bool
    var save: (DefaultThemeMode?, ThemeModeOverride?) async -> Void

    @State private var showImageImporter: Bool = false

    init(initialTheme: ThemeModeOverride, themeModeOverride: ThemeModeOverride, applyToMode: DefaultThemeMode? = nil, globalThemeUsed: Binding<Bool>, save: @escaping (DefaultThemeMode?, ThemeModeOverride?) async -> Void) {
        let cur = ThemeManager.currentColors(nil, globalThemeUsed.wrappedValue ? nil : themeModeOverride, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
        self.currentTheme = cur
        self.initialTheme = initialTheme
        self.themeModeOverride = themeModeOverride
        self.applyToMode = applyToMode
        self._globalThemeUsed = globalThemeUsed
        self.save = save
    }

    var body: some View {
        List {
            WallpaperPresetSelector(
                selectedWallpaper: currentTheme.wallpaper.type,
                activeBackgroundColor: currentTheme.wallpaper.background,
                activeTintColor: currentTheme.wallpaper.tint,
                currentColors: currentColors,
                onChooseType: onChooseType
            )
            .padding(.bottom, 10)
            .listRowInsets(.init())
            .listRowBackground(Color.clear)
            .modifier(WallpaperImporter(showImageImporter: $showImageImporter, onChooseImage: { image in
                if let filename = saveWallpaperFile(image: image) {
                    onTypeChange(WallpaperType.Image(filename, 1, WallpaperScaleType.fill))
                }
            }))

            WallpaperSetupView(
                wallpaperType: themeModeOverride.type,
                base: currentTheme.base,
                initialWallpaper: currentTheme.wallpaper,
                editColor: editColor,
                onTypeChange: onTypeChange
            )

            Section {
                if !globalThemeUsed {
                    ResetToGlobalThemeButton(ChatModel.shared.currentUser?.uiThemes?.preferredMode(isInDarkTheme()) == nil, theme.colors.primary) {
                        themeModeOverride = ThemeManager.defaultActiveTheme(ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
                        globalThemeUsed = true
                        Task {
                            await save(applyToMode, nil)
                        }
                    }
                }

                SetDefaultThemeButton(theme.colors.primary) {
                    globalThemeUsed = false
                    let lightBase = DefaultTheme.LIGHT
                    let darkBase = if currentTheme.base != DefaultTheme.LIGHT { currentTheme.base } else if systemDarkThemeDefault.get() == DefaultTheme.DARK.themeName { DefaultTheme.DARK } else if systemDarkThemeDefault.get() == DefaultTheme.BLACK.themeName { DefaultTheme.BLACK } else { DefaultTheme.SIMPLEX }
                    let mode = themeModeOverride.mode
                    Task {
                        // Saving for both modes in one place by changing mode once per save
                        if applyToMode == nil {
                            let oppositeMode = mode == DefaultThemeMode.light ? DefaultThemeMode.dark : DefaultThemeMode.light
                            await save(oppositeMode, ThemeModeOverride.withFilledAppDefaults(oppositeMode, oppositeMode == DefaultThemeMode.light ? lightBase : darkBase))
                        }
                        await MainActor.run {
                            themeModeOverride = ThemeModeOverride.withFilledAppDefaults(mode, mode == DefaultThemeMode.light ? lightBase : darkBase)
                        }
                        await save(themeModeOverride.mode, themeModeOverride)
                    }
                }
                .onChange(of: initialTheme) { initial in
                    if initial.mode != themeModeOverride.mode {
                        themeModeOverride = initial
                        currentTheme = ThemeManager.currentColors(nil, globalThemeUsed ? nil : themeModeOverride, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
                        if applyToMode != nil {
                            applyToMode = initial.mode
                        }
                    }
                }
                .onChange(of: currentTheme) { _ in
                    // Applies updated global theme if current one tracks global theme
                    if globalThemeUsed {
                        themeModeOverride = ThemeManager.defaultActiveTheme(ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
                        globalThemeUsed = true
                    }
                }
                .onChange(of: themeModeOverride) { override in
                    currentTheme = ThemeManager.currentColors(nil, globalThemeUsed ? nil : override, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
                }
            }

            if showMore {
                let values = [
                    (nil, "All modes"),
                    (DefaultThemeMode.light, "Light mode"),
                    (DefaultThemeMode.dark, "Dark mode")
                ]
                Picker("Apply to", selection: $applyToMode) {
                    ForEach(values, id: \.0) { (_, text) in
                        Text(text)
                    }
                }
                .frame(height: 36)
                .onChange(of: applyToMode) { mode in
                    if let mode, mode != currentTheme.base.mode {
                        let lightBase = DefaultTheme.LIGHT
                        let darkBase = if currentTheme.base != DefaultTheme.LIGHT { currentTheme.base } else if systemDarkThemeDefault.get() == DefaultTheme.DARK.themeName { DefaultTheme.DARK } else if systemDarkThemeDefault.get() == DefaultTheme.BLACK.themeName { DefaultTheme.BLACK } else { DefaultTheme.SIMPLEX }
                        ThemeManager.applyTheme(mode == DefaultThemeMode.light ? lightBase.themeName : darkBase.themeName)
                    }
                }

                CustomizeThemeColorsSection(editColor: editColor)

                ImportExportThemeSection(perChat: themeModeOverride, perUser: ChatModel.shared.currentUser?.uiThemes) { theme in
                    let res = ThemeModeOverride(mode: theme.base.mode, colors: theme.colors, wallpaper: theme.wallpaper?.importFromString()).removeSameColors(theme.base)
                    Task {
                        await MainActor.run {
                            themeModeOverride = res
                        }
                        await save(applyToMode, res)
                    }
                }
            } else {
                AdvancedSettingsButton(theme.colors.primary) { showMore = true }
            }
        }
    }

    private func onTypeCopyFromSameTheme(_ type: WallpaperType?) -> Bool {
        let success = ThemeManager.copyFromSameThemeOverrides(type, ChatModel.shared.currentUser?.uiThemes?.preferredMode(!currentTheme.colors.isLight), $themeModeOverride)
        if success {
            Task {
                await save(applyToMode, themeModeOverride)
            }
            globalThemeUsed = false
        }
        return success
    }

    private func preApplyGlobalIfNeeded(_ type: WallpaperType?) {
        if globalThemeUsed {
            _ = onTypeCopyFromSameTheme(type)
        }
    }

    private func onTypeChange(_ type: WallpaperType?) {
        if globalThemeUsed {
            preApplyGlobalIfNeeded(type)
            // Saves copied static image instead of original from global theme
            ThemeManager.applyWallpaper(themeModeOverride.type, $themeModeOverride)
        } else {
            ThemeManager.applyWallpaper(type, $themeModeOverride)
        }
        Task {
            await save(applyToMode, themeModeOverride)
        }
    }

    private func currentColors(_ type: WallpaperType?) -> ThemeManager.ActiveTheme {
        // If applying for :
        // - all themes: no overrides needed
        // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
        let perChatOverride: ThemeModeOverride? = type?.sameType(themeModeOverride.type) == true ? themeModeOverride : nil
        return ThemeManager.currentColors(type, perChatOverride, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
    }

    private func onChooseType(_ type: WallpaperType?) {
        if let type, case WallpaperType.Image = type {
            if (themeModeOverride.type?.isImage == true && !globalThemeUsed) || currentColors(type).wallpaper.type.image == nil {
                showImageImporter = true
            } else if !onTypeCopyFromSameTheme(currentColors(type).wallpaper.type) {
                showImageImporter = true
            }
        } else if globalThemeUsed || themeModeOverride.type != type || themeModeOverride.type != type {
            _ = onTypeCopyFromSameTheme(type)
        } else {
            onTypeChange(type)
        }
    }

    private func editColor(_ name: ThemeColor) -> Binding<Color> {
        editColorBinding(
            name: name,
            wallpaperType: themeModeOverride.type,
            wallpaperImage: themeModeOverride.type?.image,
            theme: currentTheme.toAppTheme(),
            onColorChange: { color in
                preApplyGlobalIfNeeded(themeModeOverride.type)
                ThemeManager.applyThemeColor(name: name, color: color, pref: $themeModeOverride)
                currentTheme = ThemeManager.currentColors(nil, globalThemeUsed ? nil : themeModeOverride, ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
                Task { await save(applyToMode, themeModeOverride) }
            })
    }
}

private func ResetToGlobalThemeButton(_ app: Bool, _ primaryColor: Color, _ onClick: @escaping () -> Void) -> some View {
    Button {
        onClick()
    } label: {
        Text(app ? "Reset to app theme" : "Reset to user theme")
            .foregroundColor(primaryColor)
    }
}

private func SetDefaultThemeButton(_ primaryColor: Color, _ onClick: @escaping () -> Void) -> some View {
    Button {
        onClick()
    } label: {
        Text("Set default theme")
            .foregroundColor(primaryColor)
    }
}

private func AdvancedSettingsButton(_ primaryColor: Color, _ onClick: @escaping () -> Void) -> some View {
    Button {
        onClick()
    } label: {
        HStack {
            Image(systemName: "chevron.down")
            Text("Advanced settings")
        }.foregroundColor(primaryColor)
    }
}
