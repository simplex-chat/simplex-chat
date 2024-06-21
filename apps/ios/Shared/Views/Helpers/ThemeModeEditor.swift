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
                initialSentColor: theme.appColors.sentMessage,
                initialSentQuoteColor: theme.appColors.sentQuote,
                initialReceivedColor: theme.appColors.receivedMessage,
                initialReceivedQuoteColor: theme.appColors.receivedQuote,
                editColor: { name in
                    return editColor(name, theme)
                },
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
                        themeModeOverride = ThemeModeOverride.withFilledAppDefaults(mode, mode == DefaultThemeMode.light ? lightBase : darkBase)
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

//fun ModalData.ChatWallpaperEditor(
//    theme: ThemeModeOverride,
//    applyToMode: DefaultThemeMode?,
//    globalThemeUsed: MutableState<Boolean>,
//    save: suspend (applyToMode: DefaultThemeMode?, ThemeModeOverride?) -> Unit
//) {
//    ColumnWithScrollBar(
//        Modifier
//            .fillMaxSize()
//    ) {
//        val applyToMode = remember { stateGetOrPutNullable("applyToMode") { applyToMode } }
//        var showMore by remember { stateGetOrPut("showMore") { false } }
//        val themeModeOverride = remember { stateGetOrPut("themeModeOverride") { theme } }
//        val currentTheme by remember(themeModeOverride.value, CurrentColors.collectAsState().value) {
//            mutableStateOf(
//                ThemeManager.currentColors(null, if (themeModeOverride.value == ThemeModeOverride()) null else themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
//            )
//        }
//
//        AppBarTitle(stringResource(MR.strings.settings_section_title_chat_theme))
//
//        val onTypeCopyFromSameTheme: (WallpaperType?) -> Boolean = { type ->
//            if (type is WallpaperType.Image && chatModel.remoteHostId() != null) {
//                false
//            } else {
//                val success = ThemeManager.copyFromSameThemeOverrides(type, chatModel.currentUser.value?.uiThemes?.preferredMode(!CurrentColors.value.colors.isLight), themeModeOverride)
//                if (success) {
//                    withBGApi { save(applyToMode.value, themeModeOverride.value) }
//                    globalThemeUsed.value = false
//                }
//                success
//            }
//        }
//        val preApplyGlobalIfNeeded = { type: WallpaperType? ->
//            if (globalThemeUsed.value) {
//                onTypeCopyFromSameTheme(type)
//            }
//        }
//        val onTypeChange: (WallpaperType?) -> Unit = { type ->
//            if (globalThemeUsed.value) {
//                preApplyGlobalIfNeeded(type)
//                // Saves copied static image instead of original from global theme
//                ThemeManager.applyWallpaper(themeModeOverride.value.type, themeModeOverride)
//            } else {
//                ThemeManager.applyWallpaper(type, themeModeOverride)
//            }
//            withBGApi { save(applyToMode.value, themeModeOverride.value) }
//        }
//
//        val editColor: (ThemeColor) -> Unit = { name: ThemeColor ->
//            ModalManager.end.showModal {
//                val currentTheme by remember(themeModeOverride.value, CurrentColors.collectAsState().value) {
//                    mutableStateOf(
//                        ThemeManager.currentColors(null, themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
//                    )
//                }
//                val initialColor: Color = when (name) {
//                    ThemeColor.WALLPAPER_BACKGROUND -> currentTheme.wallpaper.background ?: Color.Transparent
//                    ThemeColor.WALLPAPER_TINT -> currentTheme.wallpaper.tint ?: Color.Transparent
//                    ThemeColor.PRIMARY -> currentTheme.colors.primary
//                    ThemeColor.PRIMARY_VARIANT -> currentTheme.colors.primaryVariant
//                    ThemeColor.SECONDARY -> currentTheme.colors.secondary
//                    ThemeColor.SECONDARY_VARIANT -> currentTheme.colors.secondaryVariant
//                    ThemeColor.BACKGROUND -> currentTheme.colors.background
//                    ThemeColor.SURFACE -> currentTheme.colors.surface
//                    ThemeColor.TITLE -> currentTheme.appColors.title
//                    ThemeColor.PRIMARY_VARIANT2 -> currentTheme.appColors.primaryVariant2
//                    ThemeColor.SENT_MESSAGE -> currentTheme.appColors.sentMessage
//                    ThemeColor.SENT_QUOTE -> currentTheme.appColors.sentQuote
//                    ThemeColor.RECEIVED_MESSAGE -> currentTheme.appColors.receivedMessage
//                    ThemeColor.RECEIVED_QUOTE -> currentTheme.appColors.receivedQuote
//                }
//                AppearanceScope.ColorEditor(
//                    name,
//                    initialColor,
//                    CurrentColors.collectAsState().value.base,
//                    themeModeOverride.value.type,
//                    themeModeOverride.value.type?.image,
//                    currentTheme.wallpaper.background,
//                    currentTheme.wallpaper.tint,
//                    currentColors = {
//                        ThemeManager.currentColors(null, themeModeOverride.value, chatModel.currentUser.value?.uiThemes, appPreferences.themeOverrides.get())
//                    },
//                    onColorChange = { color ->
//                        preApplyGlobalIfNeeded(themeModeOverride.value.type)
//                        ThemeManager.applyThemeColor(name, color, themeModeOverride)
//                        withBGApi { save(applyToMode.value, themeModeOverride.value) }
//                    }
//                )
//            }
//        }
//
//        val importWallpaperLauncher = rememberFileChooserLauncher(true) { to: URI? ->
//            if (to != null) {
//                val filename = saveWallpaperFile(to)
//                if (filename != null) {
//                    // Delete only non-user image
//                    if (!globalThemeUsed.value) {
//                        removeWallpaperFile((themeModeOverride.value.type as? WallpaperType.Image)?.filename)
//                    }
//                    globalThemeUsed.value = false
//                    onTypeChange(WallpaperType.Image(filename, 1f, WallpaperScaleType.FILL))
//                }
//            }
//        }
//
//        val currentColors = { type: WallpaperType? ->
//            ThemeManager.currentColors(type, if (type?.sameType(themeModeOverride.value.type) == true) themeModeOverride.value else null, chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
//        }
//
//        WallpaperPresetSelector(
//            selectedWallpaper = currentTheme.wallpaper.type,
//            activeBackgroundColor = currentTheme.wallpaper.background,
//            activeTintColor = currentTheme.wallpaper.tint,
//            baseTheme = CurrentColors.collectAsState().value.base,
//            currentColors = { type -> currentColors(type) },
//            onChooseType = { type ->
//                when {
//                    type is WallpaperType.Image && chatModel.remoteHostId() != null -> { /* do nothing */ }
//                    type is WallpaperType.Image && ((themeModeOverride.value.type is WallpaperType.Image && !globalThemeUsed.value) || currentColors(type).wallpaper.type.image == null) -> {
//                        withLongRunningApi { importWallpaperLauncher.launch("image/*") }
//                    }
//                    type is WallpaperType.Image -> {
//                        if (!onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)) {
//                            withLongRunningApi { importWallpaperLauncher.launch("image/*") }
//                        }
//                    }
//                    globalThemeUsed.value || themeModeOverride.value.type != type -> {
//                        onTypeCopyFromSameTheme(type)
//                    }
//                    else -> {
//                        onTypeChange(type)
//                    }
//                }
//            },
//        )
//
//        WallpaperSetupView(
//            themeModeOverride.value.type,
//            CurrentColors.collectAsState().value.base,
//            currentTheme.wallpaper,
//            currentTheme.appColors.sentMessage,
//            currentTheme.appColors.sentQuote,
//            currentTheme.appColors.receivedMessage,
//            currentTheme.appColors.receivedQuote,
//            editColor = editColor,
//            onTypeChange = onTypeChange,
//        )
//
//        SectionSpacer()
//
//        if (!globalThemeUsed.value) {
//            ResetToGlobalThemeButton(false) {
//                themeModeOverride.value = ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
//                globalThemeUsed.value = true
//                withBGApi { save(applyToMode.value, null) }
//            }
//        }
//
//        SetDefaultThemeButton {
//            globalThemeUsed.value = false
//            val lightBase = DefaultTheme.LIGHT
//            val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else if (appPrefs.systemDarkTheme.get() == DefaultTheme.BLACK.themeName) DefaultTheme.BLACK else DefaultTheme.SIMPLEX
//                                val mode = themeModeOverride.value.mode
//                                withBGApi {
//                // Saving for both modes in one place by changing mode once per save
//                if (applyToMode.value == null) {
//                    val oppositeMode = if (mode == DefaultThemeMode.LIGHT) DefaultThemeMode.DARK else DefaultThemeMode.LIGHT
//                                            save(oppositeMode, ThemeModeOverride.withFilledAppDefaults(oppositeMode, if (oppositeMode == DefaultThemeMode.LIGHT) lightBase else darkBase))
//                }
//                themeModeOverride.value = ThemeModeOverride.withFilledAppDefaults(mode, if (mode == DefaultThemeMode.LIGHT) lightBase else darkBase)
//                save(themeModeOverride.value.mode, themeModeOverride.value)
//            }
//        }
//
//        KeyChangeEffect(theme.mode) {
//            themeModeOverride.value = theme
//            if (applyToMode.value != null) {
//                applyToMode.value = theme.mode
//            }
//        }
//
//        // Applies updated global theme if current one tracks global theme
//        KeyChangeEffect(CurrentColors.collectAsState()) {
//            if (globalThemeUsed.value) {
//                themeModeOverride.value = ThemeManager.defaultActiveTheme(chatModel.currentUser.value?.uiThemes, appPrefs.themeOverrides.get())
//                globalThemeUsed.value = true
//            }
//        }
//
//        SectionSpacer()
//
//        if (showMore) {
//            val values by remember { mutableStateOf(
//                listOf(
//                    null to generalGetString(MR.strings.chat_theme_apply_to_all_modes),
//                    DefaultThemeMode.LIGHT to generalGetString(MR.strings.chat_theme_apply_to_light_mode),
//                    DefaultThemeMode.DARK to generalGetString(MR.strings.chat_theme_apply_to_dark_mode),
//                )
//            )
//            }
//            ExposedDropDownSettingRow(
//                generalGetString(MR.strings.chat_theme_apply_to_mode),
//                values,
//                applyToMode,
//                icon = null,
//                enabled = remember { mutableStateOf(true) },
//                onSelected = {
//                    applyToMode.value = it
//                    if (it != null && it != CurrentColors.value.base.mode) {
//                        val lightBase = DefaultTheme.LIGHT
//                        val darkBase = if (CurrentColors.value.base != DefaultTheme.LIGHT) CurrentColors.value.base else if (appPrefs.systemDarkTheme.get() == DefaultTheme.DARK.themeName) DefaultTheme.DARK else if (appPrefs.systemDarkTheme.get() == DefaultTheme.BLACK.themeName) DefaultTheme.BLACK else DefaultTheme.SIMPLEX
//                                            ThemeManager.applyTheme(if (it == DefaultThemeMode.LIGHT) lightBase.themeName else darkBase.themeName)
//                    }
//                }
//            )
//
//            SectionSpacer()
//
//            AppearanceScope.CustomizeThemeColorsSection(currentTheme, editColor = editColor)
//        } else {
//            AdvancedSettingsButton { showMore = true }
//        }
//
//        SectionBottomSpacer()
//    }
//}

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
