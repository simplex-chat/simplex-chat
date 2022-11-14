//
//  PreferencesView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 13/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct PreferencesView: View {
    @State var allowFullDeletion = FeatureAllowed.yes
    @State var allowVoice = FeatureAllowed.yes

    var body: some View {
        VStack {
            List {
                featureSection(.fullDelete, $allowFullDeletion)
                featureSection(.voice, $allowVoice)

                Section {
                    HStack {
                        Text("Reset")
                        Spacer()
                        Text("Save")
                    }
                    .foregroundColor(.accentColor)
                    .disabled(true)
                }
                .listRowBackground(Color.clear)
            }
        }
    }

    private func featureSection(_ feature: Feature, _ allowFeature: Binding<FeatureAllowed>) -> some View {
        Section {
            settingsRow(feature.icon) {
                Picker(feature.text, selection: allowFeature) {
                    ForEach(FeatureAllowed.values) { allow in
                        Text(allow.text)
                    }
                }
                .frame(height: 36)
            }
        } footer: {
            Text(feature.allowDescription(allowFeature.wrappedValue))
                .frame(height: 36, alignment: .topLeading)
        }
    }
}

struct PreferencesView_Previews: PreviewProvider {
    static var previews: some View {
        PreferencesView()
    }
}
