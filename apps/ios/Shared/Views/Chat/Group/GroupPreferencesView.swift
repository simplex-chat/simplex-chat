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
    @EnvironmentObject var chatModel: ChatModel
    @Binding var groupInfo: GroupInfo
    @State var preferences: FullGroupPreferences
    @State var currentPreferences: FullGroupPreferences

    var body: some View {
        VStack {
            List {
                featureSection(.fullDelete, $preferences.fullDelete.enable)
                featureSection(.voice, $preferences.voice.enable)

                if groupInfo.canEdit {
                    Section {
                        Button("Reset") { preferences = currentPreferences }
                        Button("Save (and notify group members)") { savePreferences() }
                    }
                    .disabled(currentPreferences == preferences)
                }
            }
        }
    }

    private func featureSection(_ feature: Feature, _ enableFeature: Binding<GroupFeatureEnabled>) -> some View {
        Section {
            if (groupInfo.canEdit) {
                settingsRow(feature.icon) {
                    Picker(feature.text, selection: enableFeature) {
                        ForEach(GroupFeatureEnabled.values) { enable in
                            Text(enable.text)
                        }
                    }
                    .frame(height: 36)
                }
            }
            else {
                settingsRow(feature.icon) {
                    infoRow(feature.text, enableFeature.wrappedValue.text)
                }
            }
        } footer: {
            Text(feature.enableGroupPrefDescription(enableFeature.wrappedValue, groupInfo.canEdit))
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
            currentPreferences: FullGroupPreferences.sampleData
        )
    }
}
