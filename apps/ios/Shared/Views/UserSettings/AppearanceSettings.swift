//
//  AppearanceSettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/08/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct AppearanceSettings: View {
    @State private var iconLightTapped = false
    @State private var iconDarkTapped = false

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

struct AppearanceSettings_Previews: PreviewProvider {
    static var previews: some View {
        AppearanceSettings()
    }
}
