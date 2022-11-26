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
    @Binding var contact: Contact
    @State var featuresAllowed: ContactFeaturesAllowed
    @State var currentFeaturesAllowed: ContactFeaturesAllowed

    var body: some View {
        let user: User = chatModel.currentUser!

        VStack {
            List {
                // featureSection(.fullDelete, user.fullPreferences.fullDelete.allow, contact.mergedPreferences.fullDelete, $featuresAllowed.fullDelete)
                featureSection(.voice, user.fullPreferences.voice.allow, contact.mergedPreferences.voice, $featuresAllowed.voice)

                Section {
                    Button("Reset") { featuresAllowed = currentFeaturesAllowed }
                    Button("Save (and notify contact)") { savePreferences() }
                }
                .disabled(currentFeaturesAllowed == featuresAllowed)
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

    private func savePreferences() {
        Task {
            do {
                let prefs = contactFeaturesAllowedToPrefs(featuresAllowed)
                if let toContact = try await apiSetContactPrefs(contactId: contact.contactId, preferences: prefs) {
                    await MainActor.run {
                        contact = toContact
                        chatModel.updateContact(toContact)
                        currentFeaturesAllowed = featuresAllowed
                    }
                }
            } catch {
                logger.error("ContactPreferencesView apiSetContactPrefs error: \(responseError(error))")
            }
        }
    }
}

struct ContactPreferencesView_Previews: PreviewProvider {
    static var previews: some View {
        ContactPreferencesView(
            contact: Binding.constant(Contact.sampleData),
            featuresAllowed: ContactFeaturesAllowed.sampleData,
            currentFeaturesAllowed: ContactFeaturesAllowed.sampleData
        )
    }
}
