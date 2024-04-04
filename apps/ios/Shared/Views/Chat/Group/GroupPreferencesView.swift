//
//  GroupPreferencesView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 16.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupPreferencesView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var chatModel: ChatModel
    @Binding var groupInfo: GroupInfo
    @State var preferences: FullGroupPreferences
    @State var currentPreferences: FullGroupPreferences
    let creatingGroup: Bool
    @State private var showSaveDialogue = false

    var body: some View {
        let saveText: LocalizedStringKey = creatingGroup ? "Save" : "Save and notify group members"
        VStack {
            List {
                featureSection(.timedMessages, $preferences.timedMessages.enable)
                featureSection(.fullDelete, $preferences.fullDelete.enable)
                featureSection(.directMessages, $preferences.directMessages.enable)
                featureSection(.reactions, $preferences.reactions.enable)
                featureSection(.voice, $preferences.voice.enable)
                featureSection(.files, $preferences.files.enable)
                featureSection(.history, $preferences.history.enable)

                if groupInfo.canEdit {
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
            Button("Exit without saving") { dismiss() }
        }
    }

    private func featureSection(_ feature: GroupFeature, _ enableFeature: Binding<GroupFeatureEnabled>) -> some View {
        Section {
            let color: Color = enableFeature.wrappedValue == .on ? .green : .secondary
            let icon = enableFeature.wrappedValue == .on ? feature.iconFilled : feature.icon
            let timedOn = feature == .timedMessages && enableFeature.wrappedValue == .on
            if groupInfo.canEdit {
                let enable = Binding(
                    get: { enableFeature.wrappedValue == .on },
                    set: { on, _ in enableFeature.wrappedValue = on ? .on : .off }
                )
                settingsRow(icon, color: color) {
                    Toggle(feature.text, isOn: enable)
                }
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
            } else {
                settingsRow(icon, color: color) {
                    infoRow(Text(feature.text), enableFeature.wrappedValue.text)
                }
                if timedOn {
                    infoRow("Delete after", timeText(preferences.timedMessages.ttl))
                }
            }
        } footer: {
            Text(feature.enableDescription(enableFeature.wrappedValue, groupInfo.canEdit))
        }
    }

    private func savePreferences() {
        Task {
            do {
                var gp = groupInfo.groupProfile
                gp.groupPreferences = toGroupPreferences(preferences)
                let gInfo = try await apiUpdateGroup(groupInfo.groupId, gp)
                await MainActor.run {
                    groupInfo = gInfo
                    chatModel.updateGroup(gInfo)
                    currentPreferences = preferences
                }
            } catch {
                logger.error("GroupPreferencesView apiUpdateGroup error: \(responseError(error))")
            }
        }
    }
}

struct GroupPreferencesView_Previews: PreviewProvider {
    static var previews: some View {
        GroupPreferencesView(
            groupInfo: Binding.constant(GroupInfo.sampleData),
            preferences: FullGroupPreferences.sampleData,
            currentPreferences: FullGroupPreferences.sampleData,
            creatingGroup: false
        )
    }
}
