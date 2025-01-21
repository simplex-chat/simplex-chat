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
    @EnvironmentObject var theme: AppTheme
    @State var profile: LocalProfile
    @State var preferences: FullPreferences
    @State var currentPreferences: FullPreferences

    var body: some View {
        VStack {
            List {
                timedMessagesFeatureSection($preferences.timedMessages.allow)
                featureSection(.fullDelete, $preferences.fullDelete.allow)
                featureSection(.reactions, $preferences.reactions.allow)
                featureSection(.voice, $preferences.voice.allow)
                featureSection(.calls, $preferences.calls.allow)

                Section {
                    Button("Reset") { preferences = currentPreferences }
                    Button("Save (and notify contacts)") { savePreferences() }
                }
                .disabled(currentPreferences == preferences)
            }
        }
        .onDisappear {
            if currentPreferences != preferences {
                showAlert(
                    title: NSLocalizedString("Your chat preferences", comment: "alert title"),
                    message: NSLocalizedString("Chat preferences were changed.", comment: "alert message"),
                    buttonTitle: NSLocalizedString("Save", comment: "alert button"),
                    buttonAction: savePreferences,
                    cancelButton: true
                )
            }
        }
    }

    private func featureSection(_ feature: ChatFeature, _ allowFeature: Binding<FeatureAllowed>) -> some View {
        Section {
            settingsRow(feature.icon, color: theme.colors.secondary) {
                Picker(feature.text, selection: allowFeature) {
                    ForEach(FeatureAllowed.values) { allow in
                        Text(allow.text)
                    }
                }
                .frame(height: 36)
            }
        }
        footer: { featureFooter(feature, allowFeature).foregroundColor(theme.colors.secondary) }

    }

    private func timedMessagesFeatureSection(_ allowFeature: Binding<FeatureAllowed>) -> some View {
        Section {
            let allow = Binding(
                get: { allowFeature.wrappedValue == .always || allowFeature.wrappedValue == .yes },
                set: { yes, _ in allowFeature.wrappedValue = yes ? .yes : .no }
            )
            settingsRow(ChatFeature.timedMessages.icon, color: theme.colors.secondary) {
                Toggle(ChatFeature.timedMessages.text, isOn: allow)
            }
        }
        footer: { featureFooter(.timedMessages, allowFeature).foregroundColor(theme.colors.secondary) }
    }

    private func featureFooter(_ feature: ChatFeature, _ allowFeature: Binding<FeatureAllowed>) -> some View {
        Text(feature.allowDescription(allowFeature.wrappedValue))
    }

    private func savePreferences() {
        Task {
            do {
                var p = fromLocalProfile(profile)
                p.preferences = fullPreferencesToPreferences(preferences)
                if let (newProfile, updatedContacts) = try await apiUpdateProfile(profile: p) {
                    await MainActor.run {
                        chatModel.updateCurrentUser(newProfile, preferences)
                        updatedContacts.forEach(chatModel.updateContact)
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
