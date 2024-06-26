//
//  AppearanceSettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import Yams

//let interfaceStyles: [UIUserInterfaceStyle] = [.unspecified, .light, .dark]

let colorModesLocalized: [LocalizedStringKey] = ["System", "Light", "Dark"]
let colorModesNames: [DefaultThemeMode?] = [nil, DefaultThemeMode.light, DefaultThemeMode.dark]

let darkThemesLocalized: [LocalizedStringKey] = ["Dark", "SimpleX", "Black"]
let darkThemesNames: [String] = [DefaultTheme.DARK.themeName, DefaultTheme.SIMPLEX.themeName, DefaultTheme.BLACK.themeName]

let appSettingsURL = URL(string: UIApplication.openSettingsURLString)!

struct AppearanceSettings: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var sceneDelegate: SceneDelegate
    @EnvironmentObject var theme: AppTheme
    @State private var iconLightTapped = false
    @State private var iconDarkTapped = false
    //@State private var userInterfaceStyle = getUserInterfaceStyleDefault()
    @State private var colorMode: DefaultThemeMode? = {
        if currentThemeDefault.get() == DefaultTheme.SYSTEM_THEME_NAME { nil as DefaultThemeMode? } else { CurrentColors.base.mode }
    }()
    @State private var darkModeTheme: String = UserDefaults.standard.string(forKey: DEFAULT_SYSTEM_DARK_THEME) ?? DefaultTheme.SIMPLEX.themeName
    @State private var uiTintColor = getUIAccentColorDefault()
    @AppStorage(DEFAULT_PROFILE_IMAGE_CORNER_RADIUS) private var profileImageCornerRadius = defaultProfileImageCorner

    @State var themeUserDestination: (Int64, ThemeModeOverrides?)? = {
        if let currentUser = ChatModel.shared.currentUser, let uiThemes = currentUser.uiThemes, uiThemes.preferredMode(!CurrentColors.colors.isLight) != nil {
            (currentUser.userId, uiThemes)
        } else {
            nil
        }
    }()

    @State var perUserTheme: ThemeModeOverride = {
        ChatModel.shared.currentUser?.uiThemes?.preferredMode(!CurrentColors.colors.isLight) ?? ThemeModeOverride(mode: CurrentColors.base.mode)
    }()

    @State var showImageImporter: Bool = false
    @State var customizeThemeIsOpen: Bool = false

    var body: some View {
        VStack{
            List {
                Section(String("Language")) {
                    HStack {
                        Text(currentLanguage)
                        Spacer()
                        Button("Change") {
                            UIApplication.shared.open(appSettingsURL)
                        }
                    }
                }

                Section {
                    let baseTheme = theme.base
                    let wallpaperType = theme.wallpaper.type

                    ThemeDestinationPicker(themeUserDestination: $themeUserDestination, themeUserDest: themeUserDestination?.0, customizeThemeIsOpen: $customizeThemeIsOpen)

                    WallpaperPresetSelector(
                        selectedWallpaper: wallpaperType,
                        currentColors: currentColors,
                        onChooseType: onChooseType
                    )
                    .padding(.bottom, 10)
                    .listRowInsets(.init())
                    .listRowBackground(Color.clear)
                    .modifier(WallpaperImporter(showImageImporter: $showImageImporter, onChooseImage: { image in
                        if let filename = saveWallpaperFile(image: image) {
                            if themeUserDestination == nil, case let WallpaperType.Image(filename, _, _) = theme.wallpaper.type {
                                removeWallpaperFile(fileName: filename)
                            } else if let type = perUserTheme.type, case let WallpaperType.Image(filename, _, _) = type {
                                removeWallpaperFile(fileName: filename)
                            }
                            onTypeChange(WallpaperType.Image(filename, 1, WallpaperScaleType.fill))
                        }
                    }))

                    if case let WallpaperType.Image(filename, _, _) = wallpaperType, (themeUserDestination == nil || perUserTheme.wallpaper?.imageFile != nil) {
                        Button {
                            if themeUserDestination == nil {
                                let defaultActiveTheme = ThemeManager.defaultActiveTheme(themeOverridesDefault.get())
                                ThemeManager.saveAndApplyWallpaper(baseTheme, nil, themeOverridesDefault)
                                ThemeManager.removeTheme(defaultActiveTheme?.themeId)
                                removeWallpaperFile(fileName: filename)
                            } else {
                                removeUserThemeModeOverrides($themeUserDestination, $perUserTheme)
                            }
                            saveThemeToDatabase(themeUserDestination)
                        } label: {
                            Text("Remove image")
                                .foregroundColor(theme.colors.primary)
                        }
                        .listRowBackground(Color.clear)
                    }

                    Picker("Color mode", selection: $colorMode) {
                        ForEach(Array(colorModesNames.enumerated()), id: \.element) { index, mode in
                            Text(colorModesLocalized[index])
                        }
                    }
                    .frame(height: 36)
                    Picker("Dark mode colors", selection: $darkModeTheme) {
                        ForEach(Array(darkThemesNames.enumerated()), id: \.element) { index, darkTheme in
                            Text(darkThemesLocalized[index])
                        }
                    }
                    .frame(height: 36)

                    NavigationLink {
                        let userId = themeUserDestination?.0
                        if let userId {
                            UserWallpaperEditorSheet(userId: userId)
                                .onAppear {
                                    customizeThemeIsOpen = true
                                }
                        } else {
                            CustomizeThemeView(onChooseType: onChooseType)
                                .navigationTitle("Customize theme")
                                .modifier(ThemedBackground(grouped: true))
                                .onAppear {
                                    customizeThemeIsOpen = true
                                }
                        }
                    } label: {
                        Text("Customize theme")
                    }
                } header: {
                    Text("Themes")
                }
                .onChange(of: profileImageCornerRadius) { _ in
                    saveThemeToDatabase(nil)
                }
                .onChange(of: colorMode) { mode in
                    guard let mode else {
                        ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
                        return
                    }
                    if case DefaultThemeMode.light = mode {
                        ThemeManager.applyTheme(DefaultTheme.LIGHT.themeName)
                    } else if case DefaultThemeMode.dark = mode {
                        ThemeManager.applyTheme(systemDarkThemeDefault.get())
                    }
                }
                .onChange(of: darkModeTheme) { darkTheme in
                    ThemeManager.changeDarkTheme(darkTheme)
                    if currentThemeDefault.get() == DefaultTheme.SYSTEM_THEME_NAME {
                        ThemeManager.applyTheme(currentThemeDefault.get())
                    } else if currentThemeDefault.get() != DefaultTheme.LIGHT.themeName {
                        ThemeManager.applyTheme(systemDarkThemeDefault.get())
                    }
                }
                .onChange(of: uiTintColor) { _ in
                    sceneDelegate.window?.tintColor = UIColor(cgColor: uiTintColor)
                    setUIAccentColorDefault(uiTintColor)
                }

                Section("Profile images") {
                    HStack(spacing: 16) {
                        if let img = m.currentUser?.image, img != "" {
                            ProfileImage(imageStr: img, size: 60)
                        } else {
                            clipProfileImage(Image(colorScheme == .light ? "icon-dark" : "icon-light"), size: 60, radius: profileImageCornerRadius)
                        }

                        Slider(
                            value: $profileImageCornerRadius,
                            in: 0...50,
                            step: 2.5
                        )
                    }
                    .foregroundColor(.secondary)
                }

                Section("App icon") {
                    HStack {
                        updateAppIcon(image: "icon-light", icon: nil, tapped: $iconLightTapped)
                        Spacer().frame(width: 16)
                        updateAppIcon(image: "icon-dark", icon: "DarkAppIcon", tapped: $iconDarkTapped)
                    }
                }
            }
        }
        .onAppear {
            customizeThemeIsOpen = false
        }
    }

    private func updateThemeUserDestination() {
        if let dest = themeUserDestination {
            var (userId, themes) = dest
            themes = themes ?? ThemeModeOverrides()
            if case DefaultThemeMode.light = perUserTheme.mode {
                themes?.light = perUserTheme
            } else {
                themes?.dark = perUserTheme
            }
            themeUserDestination = (userId, themes)
        }
    }

    private func onTypeCopyFromSameTheme(_ type: WallpaperType?) -> Bool {
        if themeUserDestination == nil {
            ThemeManager.saveAndApplyWallpaper(theme.base, type, themeOverridesDefault)
        } else {
            var wallpaperFiles = Set([perUserTheme.wallpaper?.imageFile])
            _ = ThemeManager.copyFromSameThemeOverrides(type, nil, $perUserTheme)
            wallpaperFiles.remove(perUserTheme.wallpaper?.imageFile)
            wallpaperFiles.forEach(removeWallpaperFile)
            updateThemeUserDestination()
        }
        saveThemeToDatabase(themeUserDestination)
        return true
    }

    private func onTypeChange(_ type: WallpaperType?) {
        if themeUserDestination == nil {
            ThemeManager.saveAndApplyWallpaper(theme.base, type, themeOverridesDefault)
        } else {
            ThemeManager.applyWallpaper(type, $perUserTheme)
            updateThemeUserDestination()
        }
        saveThemeToDatabase(themeUserDestination)
    }

    private func currentColors(_ type: WallpaperType?) -> ThemeManager.ActiveTheme {
        // If applying for :
        // - all themes: no overrides needed
        // - specific user: only user overrides for currently selected theme are needed, because they will NOT be copied when other wallpaper is selected
        let perUserOverride: ThemeModeOverrides? = themeUserDestination == nil
        ? nil
        : theme.wallpaper.type.sameType(type)
        ? m.currentUser?.uiThemes
        : nil
        return ThemeManager.currentColors(type, nil, perUserOverride, themeOverridesDefault.get())
    }

    private func onChooseType(_ type: WallpaperType?) {
        // don't have image in parent or already selected wallpaper with custom image
        if let type, case WallpaperType.Image = type {
            if case WallpaperType.Image = theme.wallpaper.type, themeUserDestination?.1 != nil {
                showImageImporter = true
            } else if currentColors(type).wallpaper.type.image == nil {
                showImageImporter = true
            } else if currentColors(type).wallpaper.type.image != nil, case WallpaperType.Image = theme.wallpaper.type, themeUserDestination == nil {
                showImageImporter = true
            } else if themeUserDestination == nil {
                onTypeChange(currentColors(type).wallpaper.type)
            } else {
                _ = onTypeCopyFromSameTheme(currentColors(type).wallpaper.type)
            }
        } else if (themeUserDestination != nil && themeUserDestination?.1?.preferredMode(!CurrentColors.colors.isLight)?.type != type) || theme.wallpaper.type != type {
            _ = onTypeCopyFromSameTheme(type)
        } else {
            onTypeChange(type)
        }
    }

    private var currentLanguage: String {
        let lang = Bundle.main.preferredLocalizations.first ?? "en"
        return Locale.current.localizedString(forIdentifier: lang)?.localizedCapitalized ?? lang
    }

    private func updateAppIcon(image: String, icon: String?, tapped: Binding<Bool>) -> some View {
        Image(image)
            .resizable()
            .scaledToFit()
            .frame(width: 60, height: 60)
            .onTapGesture {
                UIApplication.shared.setAlternateIconName(icon) { err in
                    if let err = err {
                        logger.error("\(err.localizedDescription)")
                    }
                }
            }
            ._onButtonGesture { tapped.wrappedValue = $0 } perform: {}
            .overlay(tapped.wrappedValue ? Color.secondary : Color.clear)
            .cornerRadius(13.5)
    }
}

struct ChatThemePreview: View {
    @EnvironmentObject var theme: AppTheme
    var base: DefaultTheme
    var wallpaperType: WallpaperType?
    var backgroundColor: Color?
    var tintColor: Color?
    var withMessages: Bool = true

    var body: some View {
        let themeBackgroundColor = theme.colors.background
        let backgroundColor =  backgroundColor ?? wallpaperType?.defaultBackgroundColor(theme.base, theme.colors.background)
        let tintColor = tintColor ?? wallpaperType?.defaultTintColor(theme.base)
        let view = VStack {
            if withMessages {
                let alice = ChatItem.getSample(1, CIDirection.directRcv, Date.now, NSLocalizedString("Good afternoon!", comment: "message preview"))
                HStack {
                    ChatItemView(chat: Chat.sampleData, chatItem: alice, revealed: Binding.constant(false))
                    Spacer()
                }
                HStack {
                    Spacer()
                    ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, CIDirection.directSnd, Date.now, NSLocalizedString("Good morning!", comment: "message preview"), quotedItem: CIQuote.getSample(alice.id, alice.meta.itemTs, alice.content.text, chatDir: alice.chatDir)), revealed: Binding.constant(false))
                        .frame(alignment: .trailing)
                }
            } else {
                Rectangle().fill(.clear)
            }
        }
        .padding(10)
        .frame(maxWidth: .infinity)

        if let wallpaperType, let wallpaperImage = wallpaperType.image, let backgroundColor, let tintColor {
            view.modifier(ChatViewBackground(image: wallpaperImage, imageType: wallpaperType, background: backgroundColor, tint: tintColor))
        } else {
            view.background(themeBackgroundColor)
        }
    }
}

struct WallpaperPresetSelector: View {
    @EnvironmentObject var theme: AppTheme
    var selectedWallpaper: WallpaperType?
    var activeBackgroundColor: Color? = nil
    var activeTintColor: Color? = nil
    var currentColors: (WallpaperType?) -> ThemeManager.ActiveTheme
    var onChooseType: (WallpaperType?) -> Void
    let width: Double = 80
    let height: Double = 80
    let backgrounds = PresetWallpaper.allCases

    private let cornerRadius: Double = 22.5

    var baseTheme: DefaultTheme { theme.base }

    var body: some View {
        VStack {
            ChatThemePreview(
                base: theme.base,
                wallpaperType: selectedWallpaper,
                backgroundColor: activeBackgroundColor ?? theme.wallpaper.background,
                tintColor: activeTintColor ?? theme.wallpaper.tint
            )
            .environmentObject(currentColors(selectedWallpaper).toAppTheme())
            ScrollView(.horizontal, showsIndicators: false) {
                HStack {
                    BackgroundItem(nil)
                    ForEach(backgrounds, id: \.self) { background in
                        BackgroundItem(background)
                    }
                    OwnBackgroundItem(selectedWallpaper)
                }
            }
        }
    }

    func plus() -> some View {
        Image(systemName: "plus")
        .tint(theme.colors.primary)
        .frame(width: 25, height: 25)
    }

    func BackgroundItem(_ background: PresetWallpaper?) -> some View {
        let checked = (background == nil && (selectedWallpaper == nil || selectedWallpaper?.isEmpty == true)) || selectedWallpaper?.samePreset(other: background) == true
        let type = background?.toType(baseTheme, checked ? selectedWallpaper?.scale : nil)
        let overrides = currentColors(type).toAppTheme()
        return ZStack {
            if let type {
                ChatThemePreview(
                    base: baseTheme,
                    wallpaperType: type,
                    backgroundColor: checked ? activeBackgroundColor ?? overrides.wallpaper.background : overrides.wallpaper.background,
                    tintColor: checked ? activeTintColor ?? overrides.wallpaper.tint : overrides.wallpaper.tint,
                    withMessages: false
                )
                .environmentObject(overrides)
            } else {
                Rectangle().fill(overrides.colors.background)
            }
        }
        .frame(width: CGFloat(width), height: CGFloat(height))
        .clipShape(RoundedRectangle(cornerRadius: width / 100 * cornerRadius))
        .overlay(RoundedRectangle(cornerRadius: width / 100 * cornerRadius)
            .strokeBorder(checked ? overrides.colors.primary.opacity(0.8) : overrides.colors.onBackground.opacity(isInDarkTheme() ? 0.2 : 0.1), lineWidth: 1)
        )
        .onTapGesture {
            onChooseType(background?.toType(baseTheme))
        }
    }

    func OwnBackgroundItem(_ type: WallpaperType?) -> some View {
        let overrides = currentColors(WallpaperType.Image("", nil, nil))
        let appWallpaper = overrides.wallpaper
        let backgroundColor = appWallpaper.background
        let tintColor = appWallpaper.tint
        let wallpaperImage = appWallpaper.type.image
        let checked = if let type, case WallpaperType.Image = type, wallpaperImage != nil { true } else { false }
        let borderColor = if let type, case WallpaperType.Image = type { theme.colors.primary.opacity(0.8) } else { theme.colors.onBackground.opacity(0.1) }
        return ZStack {
            if checked || wallpaperImage != nil {
                ChatThemePreview(
                    base: baseTheme,
                    wallpaperType: checked ? type : appWallpaper.type,
                    backgroundColor: checked ? activeBackgroundColor ?? backgroundColor : backgroundColor,
                    tintColor: checked ? activeTintColor ?? tintColor : tintColor,
                    withMessages: false
                )
                .environmentObject(currentColors(type).toAppTheme())
            } else {
                plus()
            }
        }
        .frame(width: width, height: height)
        .clipShape(RoundedRectangle(cornerRadius: width / 100 * cornerRadius))
        .overlay(RoundedRectangle(cornerRadius: width / 100 * cornerRadius)
            .strokeBorder(borderColor, lineWidth: 1)
        )
        .onTapGesture {
            onChooseType(WallpaperType.Image("", nil, nil))
        }
      }
}

struct CustomizeThemeView: View {
    @EnvironmentObject var theme: AppTheme
    var onChooseType: (WallpaperType?) -> Void
    @State private var showFileImporter = false

    var body: some View {
        List {
            let wallpaperImage = theme.wallpaper.type.image
            let wallpaperType = theme.wallpaper.type
            let baseTheme = theme.base

            let editColor: (ThemeColor) -> Binding<Color> = { name in
                editColorBinding(
                    name: name,
                    wallpaperType: wallpaperType,
                    wallpaperImage: wallpaperImage,
                    theme: theme,
                    onColorChange: { color in
                        ThemeManager.saveAndApplyThemeColor(baseTheme, name, color)
                        saveThemeToDatabase(nil)
                    })
            }
            WallpaperPresetSelector(
                selectedWallpaper: wallpaperType,
                currentColors: { type in
                    ThemeManager.currentColors(type, nil, nil, themeOverridesDefault.get())
                },
                onChooseType: onChooseType
            )
            .listRowInsets(.init())
            .listRowBackground(Color.clear)

            if case let WallpaperType.Image(filename, _, _) = theme.wallpaper.type {
                Button {
                    let defaultActiveTheme = ThemeManager.defaultActiveTheme(themeOverridesDefault.get())
                    ThemeManager.saveAndApplyWallpaper(baseTheme, nil, themeOverridesDefault)
                    ThemeManager.removeTheme(defaultActiveTheme?.themeId)
                    removeWallpaperFile(fileName: filename)
                    saveThemeToDatabase(nil)
                } label: {
                    Text("Remove image")
                        .foregroundColor(theme.colors.primary)
                }
                .listRowBackground(Color.clear)
            }

            Section {
                WallpaperSetupView(
                    wallpaperType: wallpaperType,
                    base: baseTheme,
                    initialWallpaper: theme.wallpaper,
                    editColor: { name in
                        editColor(name)
                    },
                    onTypeChange: { type in
                        ThemeManager.saveAndApplyWallpaper(baseTheme, type, themeOverridesDefault)
                        saveThemeToDatabase(nil)
                    }
                )
            } header: {
                Text("Chat colors")
            }

            CustomizeThemeColorsSection(editColor: editColor)

            let currentOverrides = ThemeManager.defaultActiveTheme(themeOverridesDefault.get())
            let canResetColors = theme.base.hasChangedAnyColor(currentOverrides)
            if canResetColors {
                Button {
                    ThemeManager.resetAllThemeColors()
                    saveThemeToDatabase(nil)
                } label: {
                    Text("Reset colors").font(.callout).foregroundColor(theme.colors.primary)
                }
            }

            ImportExportThemeSection(perChat: nil, perUser: nil, save: { theme in
                ThemeManager.saveAndApplyThemeOverrides(theme)
                saveThemeToDatabase(nil)
            })
        }
        /// When changing app theme, user overrides are hidden. User overrides will be returned back after closing Appearance screen, see ThemeDestinationPicker()
        .interactiveDismissDisabled(true)
    }
}

struct ImportExportThemeSection: View {
    @EnvironmentObject var theme: AppTheme
    var perChat: ThemeModeOverride?
    var perUser: ThemeModeOverrides?
    var save: (ThemeOverrides) -> Void
    @State private var showFileImporter = false

    var body: some View {
        Section {
            Button {
                let overrides = ThemeManager.currentThemeOverridesForExport(perChat, perUser)
                do {
                    let encoded = try encodeThemeOverrides(overrides)
                    var lines = encoded.split(separator: "\n")
                    // Removing theme id without using custom serializer or data class
                    lines.remove(at: 0)
                    let theme = lines.joined(separator: "\n")
                    let tempUrl = getTempFilesDirectory().appendingPathComponent("simplex.theme")
                    try? FileManager.default.removeItem(at: tempUrl)
                    if FileManager.default.createFile(atPath: tempUrl.path, contents: theme.data(using: .utf8)) {
                        showShareSheet(items: [tempUrl])
                    }
                } catch {
                    AlertManager.shared.showAlertMsg(title: "Error", message: "Error exporting theme: \(error.localizedDescription)")
                }
            } label: {
                Text("Export theme").foregroundColor(theme.colors.primary)
            }
            Button {
                showFileImporter = true
            } label: {
                Text("Import theme").foregroundColor(theme.colors.primary)
            }
            .fileImporter(
                isPresented: $showFileImporter,
                allowedContentTypes: [.data/*.plainText*/],
                allowsMultipleSelection: false
            ) { result in
                if case let .success(files) = result, let fileURL = files.first {
                    do {
                        var fileSize: Int? = nil
                        if fileURL.startAccessingSecurityScopedResource() {
                            let resourceValues = try fileURL.resourceValues(forKeys: [.fileSizeKey])
                            fileSize = resourceValues.fileSize
                        }
                        if let fileSize = fileSize,
                           // Same as Android/desktop
                           fileSize <= 5_500_000 {
                            if let string = try? String(contentsOf: fileURL, encoding: .utf8), let theme: ThemeOverrides = decodeYAML("themeId: \(UUID().uuidString)\n" + string) {
                                save(theme)
                                logger.error("Saved theme from file")
                            } else {
                                logger.error("Error decoding theme file")
                            }
                            fileURL.stopAccessingSecurityScopedResource()
                        } else {
                            fileURL.stopAccessingSecurityScopedResource()
                            let prettyMaxFileSize = ByteCountFormatter.string(fromByteCount: 5_500_000, countStyle: .binary)
                            AlertManager.shared.showAlertMsg(
                                title: "Large file!",
                                message: "Currently maximum supported file size is \(prettyMaxFileSize)."
                            )
                        }
                    } catch {
                        logger.error("Appearance fileImporter error \(error.localizedDescription)")
                    }
                }
            }
        }
    }
}

struct UserWallpaperEditorSheet: View {
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var theme: AppTheme
    @State var userId: Int64
    @State private var globalThemeUsed: Bool = false

    @State private var themes = ChatModel.shared.currentUser?.uiThemes ?? ThemeModeOverrides()

    var body: some View {
        let preferred = themes.preferredMode(!theme.colors.isLight)
        let initialTheme = preferred ?? ThemeManager.defaultActiveTheme(ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
        UserWallpaperEditor(
            initialTheme: initialTheme,
            themeModeOverride: initialTheme,
            applyToMode: themes.light == themes.dark ? nil : initialTheme.mode,
            globalThemeUsed: $globalThemeUsed,
            save: { applyToMode, newTheme in
                if updateBackendDate + 1 < Date.now {
                    updateBackendDate = .now
                    await save(applyToMode, newTheme, themes, userId)
                } else {
                    updateBackendTask.cancel()
                    updateBackendTask = Task {
                        do {
                            try await Task.sleep(nanoseconds: 100_000000)
                            updateBackendDate = .now
                            await save(applyToMode, newTheme, themes, userId)
                        } catch {
                            // ignore
                        }
                    }
                }
            }
        )
        .navigationTitle("Profile theme")
        .modifier(ThemedBackground(grouped: true))
        .onAppear {
            globalThemeUsed = preferred == nil
        }
        .onChange(of: ChatModel.shared.currentUser?.userId) { _ in
            dismiss()
        }
    }

    private func save(
        _ applyToMode: DefaultThemeMode?,
        _ newTheme: ThemeModeOverride?,
        _ themes: ThemeModeOverrides?,
        _ userId: Int64
    ) async {
        let unchangedThemes: ThemeModeOverrides = themes ?? ThemeModeOverrides()
        var wallpaperFiles = Set([unchangedThemes.light?.wallpaper?.imageFile, unchangedThemes.dark?.wallpaper?.imageFile])
        var changedThemes: ThemeModeOverrides? = unchangedThemes
        let light: ThemeModeOverride? = if let newTheme {
            ThemeModeOverride(mode: DefaultThemeMode.light, colors: newTheme.colors, wallpaper: newTheme.wallpaper?.withFilledWallpaperPath())
        } else {
            nil
        }
        let dark: ThemeModeOverride? = if let newTheme {
            ThemeModeOverride(mode: DefaultThemeMode.dark, colors: newTheme.colors, wallpaper: newTheme.wallpaper?.withFilledWallpaperPath())
        } else {
            nil
        }

        if let applyToMode {
            switch applyToMode {
            case DefaultThemeMode.light:
                changedThemes?.light = light
            case DefaultThemeMode.dark:
                changedThemes?.dark = dark
            }
        } else {
            changedThemes?.light = light
            changedThemes?.dark = dark
        }
        if changedThemes?.light != nil || changedThemes?.dark != nil {
            let light = changedThemes?.light
            let dark = changedThemes?.dark
            let currentMode = CurrentColors.base.mode
            // same image file for both modes, copy image to make them as different files
            if var light, var dark, let lightWallpaper = light.wallpaper, let darkWallpaper = dark.wallpaper, let lightImageFile = lightWallpaper.imageFile, let darkImageFile = darkWallpaper.imageFile, lightWallpaper.imageFile == darkWallpaper.imageFile {
                let imageFile = if currentMode == DefaultThemeMode.light {
                    darkImageFile
                } else {
                    lightImageFile
                }
                let filePath = saveWallpaperFile(url: getWallpaperFilePath(imageFile))
                if currentMode == DefaultThemeMode.light {
                    dark.wallpaper?.imageFile = filePath
                    changedThemes = ThemeModeOverrides(light: changedThemes?.light, dark: dark)
                } else {
                    light.wallpaper?.imageFile = filePath
                    changedThemes = ThemeModeOverrides(light: light, dark: changedThemes?.dark)
                }
            }
        } else {
            changedThemes = nil
        }
        wallpaperFiles.remove(changedThemes?.light?.wallpaper?.imageFile)
        wallpaperFiles.remove(changedThemes?.dark?.wallpaper?.imageFile)
        wallpaperFiles.forEach(removeWallpaperFile)

        let oldThemes = ChatModel.shared.currentUser?.uiThemes
        let changedThemesConstant = changedThemes
        // Update before save to make it work seamless
        await MainActor.run {
            ChatModel.shared.updateCurrentUserUiThemes(uiThemes: changedThemesConstant)
        }
        if await !apiSetUserUIThemes(userId: userId, themes: changedThemesConstant) {
            await MainActor.run {
                // If failed to apply for some reason return the old themes
                ChatModel.shared.updateCurrentUserUiThemes(uiThemes: oldThemes)
            }
        }
    }
}

struct ThemeDestinationPicker: View {
    @EnvironmentObject var m: ChatModel
    @Binding var themeUserDestination: (Int64, ThemeModeOverrides?)?
    @State var themeUserDest: Int64?
    @Binding var customizeThemeIsOpen: Bool

    var body: some View {
        let values = [(nil, "All profiles")] + m.users.filter { $0.user.activeUser }.map { ($0.user.userId, $0.user.chatViewName)}

        if values.contains(where: { (userId, text) in userId == themeUserDestination?.0 }) {
            Picker("Apply to mode", selection: $themeUserDest) {
                ForEach(values, id: \.0) { (_, text) in
                    Text(text)
                }
            }
            .frame(height: 36)
            .onChange(of: themeUserDest) { userId in
                themeUserDest = userId
                if let userId {
                    themeUserDestination = (userId, m.users.first { $0.user.userId == userId }?.user.uiThemes)
                } else {
                    themeUserDestination = nil
                }
                if let userId, userId != m.currentUser?.userId {
                    changeActiveUser(userId, viewPwd: nil)
                }
            }
            .onChange(of: themeUserDestination == nil) { isNil in
                if isNil {
                    // Easiest way to hide per-user customization.
                    // Otherwise, it would be needed to make global variable and to use it everywhere for making a decision to include these overrides into active theme constructing or not
                    m.currentUser?.uiThemes = nil
                } else {
                    m.updateCurrentUserUiThemes(uiThemes: m.users.first(where: { $0.user.userId == m.currentUser?.userId })?.user.uiThemes)
                }
            }
            .onDisappear {
                // Skip when Appearance screen is not hidden yet
                if customizeThemeIsOpen { return }
                // Restore user overrides from stored list of users
                m.updateCurrentUserUiThemes(uiThemes: m.users.first(where: { $0.user.userId == m.currentUser?.userId })?.user.uiThemes)
                themeUserDestination = if let currentUser = m.currentUser, let uiThemes = currentUser.uiThemes {
                    (currentUser.userId, uiThemes)
                } else {
                    nil
                }
            }
        } else {
            EmptyView()
                .onAppear {
                    themeUserDestination = nil
                    themeUserDest = nil
                }
        }
    }
}

struct CustomizeThemeColorsSection: View {
    var editColor: (ThemeColor) -> Binding<Color>

    var body: some View {
        Section {
            picker(.PRIMARY, editColor)
            picker(.PRIMARY_VARIANT, editColor)
            picker(.SECONDARY, editColor)
            picker(.SECONDARY_VARIANT, editColor)
            picker(.BACKGROUND, editColor)
            picker(.SURFACE, editColor)
            picker(.TITLE, editColor)
            picker(.PRIMARY_VARIANT2, editColor)
        } header: {
            Text("Interface colors")
        }
    }
}

func editColorBinding(name: ThemeColor, wallpaperType: WallpaperType?, wallpaperImage: Image?, theme: AppTheme, onColorChange: @escaping (Color?) -> Void) -> Binding<Color> {
    Binding(get: {
        let baseTheme = theme.base
        let wallpaperBackgroundColor = theme.wallpaper.background ?? wallpaperType?.defaultBackgroundColor(baseTheme, theme.colors.background) ?? Color.clear
        let wallpaperTintColor = theme.wallpaper.tint ?? wallpaperType?.defaultTintColor(baseTheme) ?? Color.clear
        return switch name {
        case ThemeColor.WALLPAPER_BACKGROUND: wallpaperBackgroundColor
        case ThemeColor.WALLPAPER_TINT: wallpaperTintColor
        case ThemeColor.PRIMARY: theme.colors.primary
        case ThemeColor.PRIMARY_VARIANT: theme.colors.primaryVariant
        case ThemeColor.SECONDARY: theme.colors.secondary
        case ThemeColor.SECONDARY_VARIANT: theme.colors.secondaryVariant
        case ThemeColor.BACKGROUND: theme.colors.background
        case ThemeColor.SURFACE: theme.colors.surface
        case ThemeColor.TITLE: theme.appColors.title
        case ThemeColor.PRIMARY_VARIANT2: theme.appColors.primaryVariant2
        case ThemeColor.SENT_MESSAGE: theme.appColors.sentMessage
        case ThemeColor.SENT_QUOTE: theme.appColors.sentQuote
        case ThemeColor.RECEIVED_MESSAGE: theme.appColors.receivedMessage
        case ThemeColor.RECEIVED_QUOTE: theme.appColors.receivedQuote
        }
    }, set: onColorChange)
}

struct WallpaperSetupView: View {
    var wallpaperType: WallpaperType?
    var base: DefaultTheme
    var initialWallpaper: AppWallpaper?
    var editColor: (ThemeColor) -> Binding<Color>
    var onTypeChange: (WallpaperType?) -> Void

    var body: some View {
        if let wallpaperType, case let WallpaperType.Image(_, _, scaleType) = wallpaperType {
            let wallpaperScaleType = if let scaleType {
                scaleType
            } else if let initialWallpaper, case let WallpaperType.Image(_, _, scaleType) = initialWallpaper.type, let scaleType {
                scaleType
            } else {
                WallpaperScaleType.fill
            }
            WallpaperScaleTypeChooser(wallpaperScaleType: Binding.constant(wallpaperScaleType), wallpaperType: wallpaperType, onTypeChange: onTypeChange)
        }


        if let wallpaperType, wallpaperType.isPreset {
            WallpaperScaleChooser(wallpaperScale: Binding.constant(initialWallpaper?.type.scale ?? 1), wallpaperType: wallpaperType, onTypeChange: onTypeChange)
        } else if let wallpaperType, case let WallpaperType.Image(_, _, scaleType) = wallpaperType, scaleType == WallpaperScaleType.repeat {
            WallpaperScaleChooser(wallpaperScale: Binding.constant(initialWallpaper?.type.scale ?? 1), wallpaperType: wallpaperType, onTypeChange: onTypeChange)
        }

        if wallpaperType?.isPreset == true || wallpaperType?.isImage == true {
            picker(.WALLPAPER_BACKGROUND, editColor)
            picker(.WALLPAPER_TINT, editColor)
        }

        picker(.SENT_MESSAGE, editColor)
        picker(.SENT_QUOTE, editColor)
        picker(.RECEIVED_MESSAGE, editColor)
        picker(.RECEIVED_QUOTE, editColor)

    }

    private struct WallpaperScaleChooser: View {
        @Binding var wallpaperScale: Float
        var wallpaperType: WallpaperType?
        var onTypeChange: (WallpaperType?) -> Void

        var body: some View {
            HStack {
                Text("\(wallpaperScale)".prefix(4))
                    .frame(width: 40, height: 36, alignment: .leading)
                Slider(
                    value: Binding(get: { wallpaperScale }, set: { scale in
                        if let wallpaperType, case let WallpaperType.Preset(filename, _) = wallpaperType {
                            onTypeChange(WallpaperType.Preset(filename, Float("\(scale)".prefix(9))))
                        } else if let wallpaperType, case let WallpaperType.Image(filename, _, scaleType) = wallpaperType {
                            onTypeChange(WallpaperType.Image(filename, Float("\(scale)".prefix(9)), scaleType))
                        }
                    }),
                    in: 0.5...2,
                    step: 0.0000001
                )
                .frame(height: 36)
            }
        }
    }

    private struct WallpaperScaleTypeChooser: View {
        @Binding var wallpaperScaleType: WallpaperScaleType
        var wallpaperType: WallpaperType?
        var onTypeChange: (WallpaperType?) -> Void

        var body: some View {
            Picker("Scale", selection: Binding(get: { wallpaperScaleType }, set: { scaleType in
                if let wallpaperType, case let WallpaperType.Image(filename, scale, _) = wallpaperType {
                    onTypeChange(WallpaperType.Image(filename, scale, scaleType))
                }
            })) {
                ForEach(Array(WallpaperScaleType.allCases), id: \.self) { type in
                    Text(type.text)
                }
            }
            .frame(height: 36)
        }
    }
}

private struct picker: View {
    var name: ThemeColor
    @State var color: Color
    var editColor: (ThemeColor) -> Binding<Color>
    // Prevent a race between setting a color here and applying externally changed color to the binding
    @State private var lastColorUpdate: Date = .now

    init(_ name: ThemeColor, _ editColor: @escaping (ThemeColor) -> Binding<Color>) {
        self.name = name
        self.color = editColor(name).wrappedValue
        self.editColor = editColor
    }

    var body: some View {
        ColorPickerView(name: name, selection: $color)
            .onChange(of: color) { newColor in
                let editedColor = editColor(name)
                if editedColor.wrappedValue != newColor {
                    editedColor.wrappedValue = newColor
                    lastColorUpdate = .now
                }
            }
            .onChange(of: editColor(name).wrappedValue) { newValue in
                // Allows to update underlying color in the picker when color changed externally, for example, by reseting colors of a theme or changing the theme
                if lastColorUpdate < Date.now - 1 && newValue != color {
                    color = newValue
                }
            }
    }
}

struct ColorPickerView: View {
    var name: ThemeColor
    @State var selection: Binding<Color>

    var body: some View {
        let supportsOpacity = switch name {
        case .WALLPAPER_TINT: true
        case .SENT_MESSAGE: true
        case .SENT_QUOTE: true
        case .RECEIVED_MESSAGE: true
        case .RECEIVED_QUOTE: true
        default: UIColor(selection.wrappedValue).cgColor.alpha < 1
        }
        ColorPicker(title(name), selection: selection, supportsOpacity: supportsOpacity)

    }

    func title(_ name: ThemeColor) -> LocalizedStringKey {
        switch name {
        case ThemeColor.WALLPAPER_BACKGROUND: "Wallpaper background"
        case ThemeColor.WALLPAPER_TINT: "Wallpaper accent"
        case ThemeColor.PRIMARY: "Accent"
        case ThemeColor.PRIMARY_VARIANT: "Additional accent"
        case ThemeColor.SECONDARY: "Secondary"
        case ThemeColor.SECONDARY_VARIANT: "Additional secondary"
        case ThemeColor.BACKGROUND: "Background"
        case ThemeColor.SURFACE: "Menus & alerts"
        case ThemeColor.TITLE: "Title"
        case ThemeColor.PRIMARY_VARIANT2: "Additional accent 2"
        case ThemeColor.SENT_MESSAGE: "Sent message"
        case ThemeColor.SENT_QUOTE: "Sent reply"
        case ThemeColor.RECEIVED_MESSAGE: "Received message"
        case ThemeColor.RECEIVED_QUOTE: "Received reply"
        }
    }
}

struct WallpaperImporter: ViewModifier {
    @Binding var showImageImporter: Bool
    var onChooseImage: (UIImage) -> Void

    func body(content: Content) -> some View {
        content.sheet(isPresented: $showImageImporter) {
            // LALAL TODO: limit by 5 mb
            LibraryMediaListPicker(addMedia: { onChooseImage($0.uiImage) }, selectionLimit: 1, filter: .images, finishedPreprocessing: { }) { itemsSelected in
                await MainActor.run {
                    showImageImporter = false
                }
            }
        }
        //        content.fileImporter(
        //            isPresented: $showImageImporter,
        //            allowedContentTypes: [.image],
        //            allowsMultipleSelection: false
        //        ) { result in
        //            if case let .success(files) = result, let fileURL = files.first {
        //                do {
        //                    var fileSize: Int? = nil
        //                    if fileURL.startAccessingSecurityScopedResource() {
        //                        let resourceValues = try fileURL.resourceValues(forKeys: [.fileSizeKey])
        //                        fileSize = resourceValues.fileSize
        //                    }
        //                    fileURL.stopAccessingSecurityScopedResource()
        //                    if let fileSize = fileSize,
        //                       // Same as Android/desktop
        //                       fileSize <= 5_000_000,
        //                       let image = UIImage(contentsOfFile: fileURL.path){
        //                        onChooseImage(image)
        //                    } else {
        //                        let prettyMaxFileSize = ByteCountFormatter.string(fromByteCount: 5_500_000, countStyle: .binary)
        //                        AlertManager.shared.showAlertMsg(
        //                            title: "Large file!",
        //                            message: "Currently maximum supported file size is \(prettyMaxFileSize)."
        //                        )
        //                    }
        //                } catch {
        //                    logger.error("Appearance fileImporter error \(error.localizedDescription)")
        //                }
        //            }
        //        }
    }
}


func getUIAccentColorDefault() -> CGColor {
    let defs = UserDefaults.standard
    return CGColor(
        red: defs.double(forKey: DEFAULT_ACCENT_COLOR_RED),
        green: defs.double(forKey: DEFAULT_ACCENT_COLOR_GREEN),
        blue: defs.double(forKey: DEFAULT_ACCENT_COLOR_BLUE),
        alpha: 1
    )
}

func setUIAccentColorDefault(_ color: CGColor) {
    if let cs = color.components {
        let defs = UserDefaults.standard
        defs.set(cs[0], forKey: DEFAULT_ACCENT_COLOR_RED)
        defs.set(cs[1], forKey: DEFAULT_ACCENT_COLOR_GREEN)
        defs.set(cs[2], forKey: DEFAULT_ACCENT_COLOR_BLUE)
    }
}

private var updateBackendTask: Task = Task {}
private var updateBackendDate: Date = .now - 1

private func saveThemeToDatabase(_ themeUserDestination: (Int64, ThemeModeOverrides?)?) {
    let m = ChatModel.shared
    let oldThemes = m.currentUser?.uiThemes
    if let themeUserDestination {
        // Update before save to make it work seamless
        m.updateCurrentUserUiThemes(uiThemes: themeUserDestination.1)
    }
    updateBackendTask.cancel()
    updateBackendTask = Task {
        do {
            try await Task.sleep(nanoseconds: 300_000000)
        } catch {
            return
        }
        if themeUserDestination == nil {
            do {
                try apiSaveAppSettings(settings: AppSettings.current.prepareForExport())
            } catch {
                logger.error("Error saving settings: \(error)")
            }
        } else if let themeUserDestination, await !apiSetUserUIThemes(userId: themeUserDestination.0, themes: themeUserDestination.1) {
            // If failed to apply for some reason return the old themes
            m.updateCurrentUserUiThemes(uiThemes: oldThemes)
        }
    }
}

private func removeUserThemeModeOverrides(_ themeUserDestination: Binding<(Int64, ThemeModeOverrides?)?>, _ perUserTheme: Binding<ThemeModeOverride>) {
    guard let dest = themeUserDestination.wrappedValue else { return }
    perUserTheme.wrappedValue = ThemeModeOverride(mode: CurrentColors.base.mode)
    themeUserDestination.wrappedValue = (dest.0, nil)
    var wallpaperFilesToDelete: [String] = []
    if let type = ChatModel.shared.currentUser?.uiThemes?.light?.type, case let WallpaperType.Image(filename, _, _) = type {
        wallpaperFilesToDelete.append(filename)
    }
    if let type = ChatModel.shared.currentUser?.uiThemes?.dark?.type, case let WallpaperType.Image(filename, _, _) = type {
        wallpaperFilesToDelete.append(filename)
    }
    wallpaperFilesToDelete.forEach(removeWallpaperFile)
}

private func decodeYAML<T: Decodable>(_ string: String) -> T? {
    do {
        return try YAMLDecoder().decode(T.self, from: string)
    } catch {
        logger.error("Error decoding YAML: \(error)")
        return nil
    }
}

private func encodeThemeOverrides(_ value: ThemeOverrides) throws -> String {
    let encoder = YAMLEncoder()
    encoder.options = YAMLEncoder.Options(sequenceStyle: .block, mappingStyle: .block, newLineScalarStyle: .doubleQuoted)

    guard var node = try Yams.compose(yaml: try encoder.encode(value)) else {
        throw RuntimeError("Error while composing a node from object")
    }
    node["base"]?.scalar?.style = .doubleQuoted

    ThemeColors.CodingKeys.allCases.forEach { key in
        node["colors"]?[key.stringValue]?.scalar?.style = .doubleQuoted
    }

    ThemeWallpaper.CodingKeys.allCases.forEach { key in
        if case .scale = key {
            // let number be without quotes
        } else {
            node["wallpaper"]?[key.stringValue]?.scalar?.style = .doubleQuoted
        }
    }
    return try Yams.serialize(node: node)
}

//func getUserInterfaceStyleDefault() -> UIUserInterfaceStyle {
//    switch UserDefaults.standard.integer(forKey: DEFAULT_USER_INTERFACE_STYLE) {
//    case 1: return .light
//    case 2: return .dark
//    default: return .unspecified
//    }
//}
//
//func setUserInterfaceStyleDefault(_ style: UIUserInterfaceStyle) {
//    var v: Int
//    switch style {
//    case .unspecified: v = 0
//    case .light: v = 1
//    case .dark: v = 2
//    default: v = 0
//    }
//    UserDefaults.standard.set(v, forKey: DEFAULT_USER_INTERFACE_STYLE)
//}

struct AppearanceSettings_Previews: PreviewProvider {
    static var previews: some View {
        AppearanceSettings()
    }
}
