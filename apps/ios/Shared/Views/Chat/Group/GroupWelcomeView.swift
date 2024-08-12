//
//  GroupWelcomeView.swift
//  SimpleX (iOS)
//
//  Created by Avently on 21/03/2022.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupWelcomeView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var groupInfo: GroupInfo
    @State var groupProfile: GroupProfile
    @State var welcomeText: String
    @State private var editMode = true
    @FocusState private var keyboardVisible: Bool
    @State private var showSaveDialog = false

    let maxByteCount = 1200

    var body: some View {
        VStack {
            if groupInfo.canEdit {
                editorView()
                    .modifier(BackButton(disabled: Binding.constant(false)) {
                        if welcomeTextUnchanged() {
                            dismiss()
                        } else {
                            showSaveDialog = true
                        }
                    })
                    .confirmationDialog(
                        welcomeTextFitsLimit() ? "Save welcome message?" : "Welcome message is too long",
                        isPresented: $showSaveDialog
                    ) {
                        if welcomeTextFitsLimit() {
                            Button("Save and update group profile") { save() }
                        }
                        Button("Exit without saving") { dismiss() }
                    }
            } else {
                List {
                    Section {
                        textPreview()
                        copyButton()
                    }
                }
            }
        }
        .onAppear {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                keyboardVisible = true
            }
        }
    }

    private func textPreview() -> some View {
        messageText(welcomeText, parseSimpleXMarkdown(welcomeText), nil, showSecrets: false, secondaryColor: theme.colors.secondary)
            .frame(minHeight: 130, alignment: .topLeading)
            .frame(maxWidth: .infinity, alignment: .leading)
    }

    private func editorView() -> some View {
        List {
            Section {
                if editMode {
                    ZStack {
                        Group {
                            if welcomeText.isEmpty {
                                TextEditor(text: Binding.constant(NSLocalizedString("Enter welcome message…", comment: "placeholder")))
                                    .foregroundColor(theme.colors.secondary)
                                    .disabled(true)
                            }
                            TextEditor(text: $welcomeText)
                                .focused($keyboardVisible)
                        }
                        .padding(.horizontal, -5)
                        .padding(.top, -8)
                        .frame(height: 130, alignment: .topLeading)
                        .frame(maxWidth: .infinity, alignment: .leading)
                    }
                } else {
                    textPreview()
                }

                Button {
                    editMode = !editMode
                    keyboardVisible = editMode
                } label: {
                    if editMode {
                        Label ("Preview", systemImage: "character")
                    } else {
                        Label ("Edit", systemImage: "pencil")
                    }
                }
                .disabled(welcomeText.isEmpty)
                copyButton()
            } footer: {
                Text(!welcomeTextFitsLimit() ? "Message too large" : "")
                    .foregroundColor(.red)
            }

            Section {
                saveButton()
            }
        }
    }

    private func copyButton() -> some View {
        Button {
            UIPasteboard.general.string = welcomeText
        } label: {
            Label ("Copy", systemImage: "doc.on.doc")
        }
    }

    private func saveButton() -> some View {
        Button("Save and update group profile") {
            save()
        }
        .disabled(welcomeTextUnchanged() || !welcomeTextFitsLimit())
    }

    private func welcomeTextUnchanged() -> Bool {
        welcomeText == groupInfo.groupProfile.description || (welcomeText == "" && groupInfo.groupProfile.description == nil)
    }

    private func welcomeTextFitsLimit() -> Bool {
        chatJsonLength(welcomeText) <= maxByteCount
    }

    private func save() {
        Task {
            do {
                var welcome: String? = welcomeText.trimmingCharacters(in: .whitespacesAndNewlines)
                if welcome?.count == 0 {
                    welcome = nil
                }
                groupProfile.description = welcome
                let gInfo = try await apiUpdateGroup(groupInfo.groupId, groupProfile)
                await MainActor.run {
                    groupInfo = gInfo
                    ChatModel.shared.updateGroup(gInfo)
                    dismiss()
                }
            } catch let error {
                logger.error("apiUpdateGroup error: \(responseError(error))")
            }
        }
    }
}

struct GroupWelcomeView_Previews: PreviewProvider {
    static var previews: some View {
        GroupProfileView(groupInfo: Binding.constant(GroupInfo.sampleData), groupProfile: GroupProfile.sampleData)
    }
}
