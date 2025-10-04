//
//  MemberAdmissionView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.04.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let memberCriterias: [(criteria: MemberCriteria?, text: LocalizedStringKey)] = [
    (nil, "off"),
    (.all, "all")
]

struct MemberAdmissionView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var groupInfo: GroupInfo
    @Binding var admission: GroupMemberAdmission
    var currentAdmission: GroupMemberAdmission
    let creatingGroup: Bool
    let saveAdmission: () -> Void
    @State private var showSaveDialogue = false

    var body: some View {
        let saveText: LocalizedStringKey = creatingGroup ? "Save" : "Save and notify group members"
        VStack {
            List {
                admissionSection(
                    NSLocalizedString("Review members", comment: "admission stage"),
                    NSLocalizedString("Review members before admitting (\"knocking\").", comment: "admission stage description"),
                    $admission.review
                )

                if groupInfo.isOwner {
                    Section {
                        Button("Reset") { admission = currentAdmission }
                        Button(saveText) { saveAdmission() }
                    }
                    .disabled(currentAdmission == admission)
                }
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            if currentAdmission == admission {
                dismiss()
            } else {
                showSaveDialogue = true
            }
        })
        .confirmationDialog("Save admission settings?", isPresented: $showSaveDialogue) {
            Button(saveText) {
                saveAdmission()
                dismiss()
            }
            Button("Exit without saving") {
                admission = currentAdmission
                dismiss()
            }
        }
    }

    private func admissionSection(_ admissionStageStr: String, _ admissionStageDescrStr: String, _ memberCriteria: Binding<MemberCriteria?>) -> some View {
        Section {
            if groupInfo.isOwner {
                Picker(admissionStageStr, selection: memberCriteria) {
                    ForEach(memberCriterias, id: \.criteria) { mc in
                        Text(mc.text)
                    }
                }
                .frame(height: 36)
            } else {
                infoRow(Text(admissionStageStr), memberCriteria.wrappedValue?.text ?? NSLocalizedString("off", comment: "member criteria value"))
            }
        } footer: {
            Text(admissionStageDescrStr)
                .foregroundColor(theme.colors.secondary)
        }
    }
}

#Preview {
    MemberAdmissionView(
        groupInfo: Binding.constant(GroupInfo.sampleData),
        admission: Binding.constant(GroupMemberAdmission.sampleData),
        currentAdmission: GroupMemberAdmission.sampleData,
        creatingGroup: false,
        saveAdmission: {}
    )
}
