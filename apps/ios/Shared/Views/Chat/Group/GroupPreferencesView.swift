//
//  GroupPreferencesView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 16.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let featureRoles: [(role: GroupMemberRole?, text: LocalizedStringKey)] = [
    (nil, "all members"),
    (.admin, "admins"),
    (.owner, "owners")
]

struct GroupPreferencesView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var groupInfo: GroupInfo
    @Binding var preferences: FullGroupPreferences
    var currentPreferences: FullGroupPreferences
    let creatingGroup: Bool
    let savePreferences: () -> Void
    @State private var showSaveDialogue = false

    var body: some View {
        let saveText: LocalizedStringKey = creatingGroup ? "Save" : "Save and notify group members"
        VStack {
            List {
                featureSection(.timedMessages, $preferences.timedMessages.enable)
                featureSection(.fullDelete, $preferences.fullDelete.enable)
                featureSection(.directMessages, $preferences.directMessages.enable, $preferences.directMessages.role)
                featureSection(.reactions, $preferences.reactions.enable)
                featureSection(.voice, $preferences.voice.enable, $preferences.voice.role)
                featureSection(.files, $preferences.files.enable, $preferences.files.role)
                featureSection(.simplexLinks, $preferences.simplexLinks.enable, $preferences.simplexLinks.role)
                featureSection(.reports, $preferences.reports.enable)
                featureSection(.history, $preferences.history.enable)

                if groupInfo.isOwner {
                    Section {
                        Button("Reset") { preferences = currentPreferences }
                        Button(saveText) { savePreferences() }
                    }
                    .disabled(currentPreferences == preferences)
                }
            }
        }
        .onChange(of: preferences.timedMessages.enable) { enable in
            if enable == .on {
                if preferences.timedMessages.ttl == nil {
                    preferences.timedMessages.ttl = 86400
                }
            } else {
                preferences.timedMessages.ttl = currentPreferences.timedMessages.ttl
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if currentPreferences == preferences {
                dismiss()
            } else {
                showSaveDialogue = true
            }
        })
        .confirmationDialog("Save preferences?", isPresented: $showSaveDialogue) {
            Button(saveText) {
                savePreferences()
                dismiss()
            }
            Button("Exit without saving") {
                preferences = currentPreferences
                dismiss()
            }
        }
    }

    private func featureSection(_ feature: GroupFeature, _ enableFeature: Binding<GroupFeatureEnabled>, _ enableForRole: Binding<GroupMemberRole?>? = nil) -> some View {
        Section {
            let color: Color = enableFeature.wrappedValue == .on ? .green : theme.colors.secondary
            let icon = enableFeature.wrappedValue == .on ? feature.iconFilled : feature.icon
            let timedOn = feature == .timedMessages && enableFeature.wrappedValue == .on
            if groupInfo.isOwner {
                let enable = Binding(
                    get: { enableFeature.wrappedValue == .on },
                    set: { on, _ in enableFeature.wrappedValue = on ? .on : .off }
                )
                settingsRow(icon, color: color) {
                    Toggle(feature.text, isOn: enable)
                }
                .disabled(feature == .reports) // remove in 6.4
                if timedOn {
                    DropdownCustomTimePicker(
                        selection: $preferences.timedMessages.ttl,
                        label: "Delete after",
                        dropdownValues: TimedMessagesPreference.ttlValues,
                        customPickerConfirmButtonText: "Select",
                        customPickerDescription: "Delete after"
                    )
                    .frame(height: 36)
                }
                if enableFeature.wrappedValue == .on, let enableForRole {
                    Picker("Enabled for", selection: enableForRole) {
                        ForEach(featureRoles, id: \.role) { fr in
                            Text(fr.text)
                        }
                    }
                    .frame(height: 36)
                }
            } else {
                settingsRow(icon, color: color) {
                    infoRow(Text(feature.text), enableFeature.wrappedValue.text)
                }
                if timedOn {
                    infoRow("Delete after", timeText(preferences.timedMessages.ttl))
                }
                if enableFeature.wrappedValue == .on, let enableForRole {
                    HStack {
                        Text("Enabled for").foregroundColor(theme.colors.secondary)
                        Spacer()
                        Text(
                            featureRoles.first(where: { fr in fr.role == enableForRole.wrappedValue })?.text
                            ?? "all members"
                        )
                        .foregroundColor(theme.colors.secondary)
                    }
                }
            }
        } footer: {
            Text(feature.enableDescription(enableFeature.wrappedValue, groupInfo.isOwner))
                .foregroundColor(theme.colors.secondary)
        }
        .onChange(of: enableFeature.wrappedValue) { enabled in
            if case .off = enabled {
                enableForRole?.wrappedValue = nil
            }
        }
    }
}

struct GroupPreferencesView_Previews: PreviewProvider {
    static var previews: some View {
        GroupPreferencesView(
            groupInfo: Binding.constant(GroupInfo.sampleData),
            preferences: Binding.constant(FullGroupPreferences.sampleData),
            currentPreferences: FullGroupPreferences.sampleData,
            creatingGroup: false,
            savePreferences: {}
        )
    }
}
