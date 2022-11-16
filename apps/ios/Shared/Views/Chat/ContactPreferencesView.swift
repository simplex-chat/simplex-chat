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
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var contact: Contact
    @State var allowFullDeletion: ContactFeatureAllowed
    @State var currentAllowFullDeletion: ContactFeatureAllowed
    @State var allowVoice: ContactFeatureAllowed
    @State var currentAllowVoice: ContactFeatureAllowed

    var body: some View {
        let user: User = chatModel.currentUser!

        VStack {
            List {
                featureSection(.fullDelete, user.fullPreferences.fullDelete.allow, contact.mergedPreferences.fullDelete, $allowFullDeletion)
                featureSection(.voice, user.fullPreferences.voice.allow, contact.mergedPreferences.voice, $allowVoice)

                Section {
                    Button("Reset", role: .destructive) { resetPreferences() }
                    Button("Save (and notify contact)") { savePreferences() }
                }
                .disabled(!preferencesChanged())
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
            infoRow("Contact allows", pref.contactPreference.allow.text)
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

    private func preferencesChanged() -> Bool {
        currentAllowFullDeletion != allowFullDeletion || currentAllowVoice != allowVoice
    }

    private func resetPreferences() {
        allowFullDeletion = currentAllowFullDeletion
        allowVoice = currentAllowVoice
    }

    private func setPreferences() {
        currentAllowFullDeletion = allowFullDeletion
        currentAllowVoice = allowVoice
    }

    private func savePreferences() {
        Task {
            do {
                if let toContact = try await apiSetContactPrefs(contactId: contact.contactId, preferences: collectPreferences()) {
                    await MainActor.run {
                        contact = toContact
                        chatModel.updateContact(toContact)
                        setPreferences()
                        dismiss()
                    }
                }
            } catch {
                logger.error("ContactPreferencesView apiSetContactPrefs error: \(responseError(error))")
            }
        }
    }

    private func collectPreferences() -> Preferences {
        let allowFullDeletionPref = contactFeatureAllowedToPref(allowFullDeletion)
        let allowVoicePref = contactFeatureAllowedToPref(allowVoice)
        return Preferences(fullDelete: allowFullDeletionPref, voice: allowVoicePref)
    }
}

struct ContactPreferencesView_Previews: PreviewProvider {
    static var previews: some View {
        ContactPreferencesView(
            contact: Binding.constant(Contact.sampleData),
            allowFullDeletion: ContactFeatureAllowed.userDefault(.no),
            currentAllowFullDeletion: ContactFeatureAllowed.userDefault(.no),
            allowVoice: ContactFeatureAllowed.userDefault(.yes),
            currentAllowVoice: ContactFeatureAllowed.userDefault(.yes)
        )
    }
}
