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
                featureSection(.fullDelete, $preferences.fullDelete.enable)
                featureSection(.directMessages, $preferences.directMessages.enable)
                featureSection(.voice, $preferences.voice.enable)

                if groupInfo.canEdit {
                    Section {
                        Button("Reset") { preferences = currentPreferences }
                        Button(saveText) { savePreferences() }
                    }
                    .disabled(currentPreferences == preferences)
                }
            }
        }
        .modifier(BackButton {
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
            if (groupInfo.canEdit) {
                settingsRow(icon, color: color) {
                    Picker(feature.text, selection: enableFeature) {
                        ForEach(GroupFeatureEnabled.values) { enable in
                            Text(enable.text)
                        }
                    }
                    .frame(height: 36)
                }
            }
            else {
                settingsRow(icon, color: color) {
                    infoRow(feature.text, enableFeature.wrappedValue.text)
                }
            }
        } footer: {
            Text(feature.enableDescription(enableFeature.wrappedValue, groupInfo.canEdit))
                .frame(height: 36, alignment: .topLeading)
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
