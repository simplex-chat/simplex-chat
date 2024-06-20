//
//  AppearanceSettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let defaultAccentColor = CGColor.init(red: 0, green: 0.533, blue: 1, alpha: 1)

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
    @State private var iconLightTapped = false
    @State private var iconDarkTapped = false
    //@State private var userInterfaceStyle = getUserInterfaceStyleDefault()
    @State private var colorMode: DefaultThemeMode? = {
        if currentThemeDefault.get() == DefaultTheme.SYSTEM_THEME_NAME { nil as DefaultThemeMode? } else { CurrentColors.base.mode }
    }()
    @State private var darkModeTheme: String = UserDefaults.standard.string(forKey: DEFAULT_SYSTEM_DARK_THEME) ?? DefaultTheme.SIMPLEX.themeName
    @State private var uiTintColor = getUIAccentColorDefault()
    @AppStorage(DEFAULT_PROFILE_IMAGE_CORNER_RADIUS) private var profileImageCornerRadius = defaultProfileImageCorner

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

                Section("App icon") {
                    HStack {
                        updateAppIcon(image: "icon-light", icon: nil, tapped: $iconLightTapped)
                        Spacer().frame(width: 16)
                        updateAppIcon(image: "icon-dark", icon: "DarkAppIcon", tapped: $iconDarkTapped)
                    }
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

                Section {
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
                    ColorPicker("Accent color", selection: $uiTintColor, supportsOpacity: false)
                } header: {
                    Text("Colors")
                } footer: {
                    Button {
                        uiTintColor = defaultAccentColor
                        setUIAccentColorDefault(defaultAccentColor)
                    } label: {
                        Text("Reset colors").font(.callout)
                    }
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
            }
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
