//
//  ContactPreferencesView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 13/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactPreferencesView: View {
    @State var allowFullDeletion = ContactFeatureAllowed.yes
    @State var allowVoice = ContactFeatureAllowed.yes
    @State var prefs = ContactUserPreferences(
        fullDelete: ContactUserPreference(
            enabled: FeatureEnabled(forUser: true, forContact: true),
            userPreference: .user(preference: Preference(allow: .yes)),
            contactPreference: Preference(allow: .no)
        ),
        voice: ContactUserPreference(
            enabled: FeatureEnabled(forUser: true, forContact: true),
            userPreference: .user(preference: Preference(allow: .yes)),
            contactPreference: Preference(allow: .no)
        )
    )

    var body: some View {
        VStack {
            List {
                featureSection(.fullDelete, .yes, prefs.fullDelete, $allowFullDeletion)
                featureSection(.voice, .yes, prefs.voice, $allowVoice)

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

    private func featureSection(_ feature: Feature, _ userDefault: FeatureAllowed, _ pref: ContactUserPreference, _ allowFeature: Binding<ContactFeatureAllowed>) -> some View {
        let enabled = FeatureEnabled.enabled(
            user: Preference(allow: allowFeature.wrappedValue.allowed),
            contact: pref.contactPreference
        )
        return Section {
            Picker("You allow", selection: allowFeature) {
                ForEach(ContactFeatureAllowed.values(userDefault)) { allow in
                    Text(allow.text)
                }
            }
            .frame(height: 36)
            HStack {
                Text("Contact allows")
                Spacer()
                Text(pref.contactPreference.allow.text)
            }
        } header: {
            HStack {
                Image(systemName: "\(feature.icon).fill")
                    .foregroundColor(enabled.forUser ? .green : enabled.forContact ? .yellow : .red)
                Text(feature.text)
            }
        } footer: {
            Text(feature.enabledDescription(enabled))
            .frame(height: 36, alignment: .topLeading)
        }
    }
}

struct ContactPreferencesView_Previews: PreviewProvider {
    static var previews: some View {
        ContactPreferencesView()
    }
}
