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
    @EnvironmentObject var chatModel: ChatModel
    @State var profile: LocalProfile
    @State var preferences: FullPreferences
    @State var currentPreferences: FullPreferences

    var body: some View {
        VStack {
            List {
                featureSection(.fullDelete, $preferences.fullDelete.allow)
                featureSection(.voice, $preferences.voice.allow)

                Section {
                    Button("Reset") { preferences = currentPreferences }
                    Button("Save (and notify contacts)") { savePreferences() }
                }
                .disabled(currentPreferences == preferences)
            }
        }
    }

    private func featureSection(_ feature: ChatFeature, _ allowFeature: Binding<FeatureAllowed>) -> some View {
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

    private func savePreferences() {
        Task {
            do {
                var p = fromLocalProfile(profile)
                p.preferences = fullPreferencesToPreferences(preferences)
                if let newProfile = try await apiUpdateProfile(profile: p) {
                    await MainActor.run {
                        if let profileId = chatModel.currentUser?.profile.profileId {
                            chatModel.currentUser?.profile = toLocalProfile(profileId, newProfile, "")
                            chatModel.currentUser?.fullPreferences = preferences
                        }
                        currentPreferences = preferences
                    }
                }
            } catch {
                logger.error("PreferencesView apiUpdateProfile error: \(responseError(error))")
            }
        }
    }
}

struct PreferencesView_Previews: PreviewProvider {
    static var previews: some View {
        PreferencesView(
            profile: LocalProfile(profileId: 1, displayName: "alice", fullName: "", localAlias: ""),
            preferences: FullPreferences.sampleData,
            currentPreferences: FullPreferences.sampleData
        )
    }
}
