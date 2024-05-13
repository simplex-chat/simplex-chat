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
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var chatModel: ChatModel
    @Binding var contact: Contact
    @State var featuresAllowed: ContactFeaturesAllowed
    @State var currentFeaturesAllowed: ContactFeaturesAllowed
    @State private var showSaveDialogue = false

    var body: some View {
        let user: User = chatModel.currentUser!

        VStack {
            List {
                timedMessagesFeatureSection()
                featureSection(.fullDelete, user.fullPreferences.fullDelete.allow, contact.mergedPreferences.fullDelete, $featuresAllowed.fullDelete)
                featureSection(.reactions, user.fullPreferences.reactions.allow, contact.mergedPreferences.reactions, $featuresAllowed.reactions)
                featureSection(.voice, user.fullPreferences.voice.allow, contact.mergedPreferences.voice, $featuresAllowed.voice)
                featureSection(.calls, user.fullPreferences.calls.allow, contact.mergedPreferences.calls, $featuresAllowed.calls)

                Section {
                    Button("Reset") { featuresAllowed = currentFeaturesAllowed }
                    Button("Save and notify contact") { savePreferences() }
                }
                .disabled(currentFeaturesAllowed == featuresAllowed)
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if currentFeaturesAllowed == featuresAllowed {
                dismiss()
            } else {
                showSaveDialogue = true
            }
        })
        .confirmationDialog("Save preferences?", isPresented: $showSaveDialogue) {
            Button("Save and notify contact") {
                savePreferences()
                dismiss()
            }
            Button("Exit without saving") { dismiss() }
        }
    }

    private func featureSection(_ feature: ChatFeature, _ userDefault: FeatureAllowed, _ pref: ContactUserPreference<SimplePreference>, _ allowFeature: Binding<ContactFeatureAllowed>) -> some View {
        let enabled = FeatureEnabled.enabled(
            asymmetric: feature.asymmetric,
            user: SimplePreference(allow: allowFeature.wrappedValue.allowed),
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
        }
        header: { featureHeader(feature, enabled) }
        footer: { featureFooter(feature, enabled) }
    }

    private func timedMessagesFeatureSection() -> some View {
        let pref = contact.mergedPreferences.timedMessages
        let enabled = FeatureEnabled.enabled(
            asymmetric: ChatFeature.timedMessages.asymmetric,
            user: TimedMessagesPreference(allow: featuresAllowed.timedMessagesAllowed ? .yes : .no),
            contact: pref.contactPreference
        )
        return Section {
            Toggle("You allow", isOn: $featuresAllowed.timedMessagesAllowed)
                .onChange(of: featuresAllowed.timedMessagesAllowed) { allow in
                    if allow {
                        if featuresAllowed.timedMessagesTTL == nil {
                            featuresAllowed.timedMessagesTTL = 86400
                        }
                    } else {
                        featuresAllowed.timedMessagesTTL = currentFeaturesAllowed.timedMessagesTTL
                    }
                }
            infoRow("Contact allows", pref.contactPreference.allow.text)
            if featuresAllowed.timedMessagesAllowed {
                DropdownCustomTimePicker(
                    selection: $featuresAllowed.timedMessagesTTL,
                    label: "Delete after",
                    dropdownValues: TimedMessagesPreference.ttlValues,
                    customPickerConfirmButtonText: "Select",
                    customPickerDescription: "Delete after"
                )
                .frame(height: 36)
            } else if pref.contactPreference.allow == .yes || pref.contactPreference.allow == .always {
                infoRow("Delete after", timeText(pref.contactPreference.ttl))
            }
        }
        header: { featureHeader(.timedMessages, enabled) }
        footer: { featureFooter(.timedMessages, enabled) }
    }

    private func featureHeader(_ feature: ChatFeature, _ enabled: FeatureEnabled) -> some View {
        HStack {
            Image(systemName: feature.iconFilled)
                .foregroundColor(enabled.forUser ? .green : enabled.forContact ? .yellow : .red)
            Text(feature.text)
        }
    }

    private func featureFooter(_ feature: ChatFeature, _ enabled: FeatureEnabled) -> some View {
        Text(feature.enabledDescription(enabled))
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
