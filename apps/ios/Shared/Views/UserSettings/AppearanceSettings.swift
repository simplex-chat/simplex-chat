//
//  AppearanceSettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

let defaultAccentColor = CGColor.init(red: 0, green: 0.533, blue: 1, alpha: 1)

let interfaceStyles: [UIUserInterfaceStyle] = [.unspecified, .light, .dark]

let interfaceStyleNames: [LocalizedStringKey] = ["System", "Light", "Dark"]

struct AppearanceSettings: View {
    @EnvironmentObject var sceneDelegate: SceneDelegate
    @State private var iconLightTapped = false
    @State private var iconDarkTapped = false
    @State private var userInterfaceStyle = getUserInterfaceStyleDefault()
    @State private var uiTintColor = getUIAccentColorDefault()

    var body: some View {
        VStack{
            List {
                Section("App icon") {
                    HStack {
                        updateAppIcon(image: "icon-light", icon: nil, tapped: $iconLightTapped)
                        Spacer().frame(width: 16)
                        updateAppIcon(image: "icon-dark", icon: "DarkAppIcon", tapped: $iconDarkTapped)
                    }
                }

                Section {
                    Picker("Theme", selection: $userInterfaceStyle) {
                        ForEach(interfaceStyles, id: \.self) { style in
                            Text(interfaceStyleNames[interfaceStyles.firstIndex(of: style) ?? 0])
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
                .onChange(of: userInterfaceStyle) { _ in
                    sceneDelegate.window?.overrideUserInterfaceStyle = userInterfaceStyle
                    setUserInterfaceStyleDefault(userInterfaceStyle)
                }
                .onChange(of: uiTintColor) { _ in
                    sceneDelegate.window?.tintColor = UIColor(cgColor: uiTintColor)
                    setUIAccentColorDefault(uiTintColor)
                }
            }
        }
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
            .cornerRadius(20)
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

func getUserInterfaceStyleDefault() -> UIUserInterfaceStyle {
    switch UserDefaults.standard.integer(forKey: DEFAULT_USER_INTERFACE_STYLE) {
    case 1: return .light
    case 2: return .dark
    default: return .unspecified
    }
}

func setUserInterfaceStyleDefault(_ style: UIUserInterfaceStyle) {
    var v: Int
    switch style {
    case .unspecified: v = 0
    case .light: v = 1
    case .dark: v = 2
    default: v = 0
    }
    UserDefaults.standard.set(v, forKey: DEFAULT_USER_INTERFACE_STYLE)
}

struct AppearanceSettings_Previews: PreviewProvider {
    static var previews: some View {
        AppearanceSettings()
    }
}
