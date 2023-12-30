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
    @EnvironmentObject private var m: ChatModel
    var groupId: Int64
    @Binding var groupInfo: GroupInfo
    @State private var welcomeText: String = ""
    @State private var editMode = true
    @FocusState private var keyboardVisible: Bool
    @State private var showSaveDialog = false

    var body: some View {
        VStack {
            if groupInfo.canEdit {
                editorView()
                    .modifier(BackButton {
                        if welcomeText == groupInfo.groupProfile.description || (welcomeText == "" && groupInfo.groupProfile.description == nil) {
                            dismiss()
                        } else {
                            showSaveDialog = true
                        }
                    })
                    .confirmationDialog("Save welcome message?", isPresented: $showSaveDialog) {
                        Button("Save and update group profile") {
                            save()
                            dismiss()
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
            welcomeText = groupInfo.groupProfile.description ?? ""
            keyboardVisible = true
        }
    }

    private func textPreview() -> some View {
        messageText(welcomeText, parseSimpleXMarkdown(welcomeText), nil, showSecrets: false)
            .allowsHitTesting(false)
            .frame(minHeight: 140, alignment: .topLeading)
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
                                    .foregroundColor(.secondary)
                                    .disabled(true)
                            }
                            TextEditor(text: $welcomeText)
                                .focused($keyboardVisible)
                        }
                        .padding(.horizontal, -5)
                        .padding(.top, -8)
                        .frame(height: 140, alignment: .topLeading)
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
        .disabled(welcomeText == groupInfo.groupProfile.description || (welcomeText == "" && groupInfo.groupProfile.description == nil))
    }

    private func save() {
        Task {
            do {
                var welcome: String? = welcomeText.trimmingCharacters(in: .whitespacesAndNewlines)
                if welcome?.count == 0 {
                    welcome = nil
                }
                var groupProfileUpdated = groupInfo.groupProfile
                groupProfileUpdated.description = welcome
                groupInfo = try await apiUpdateGroup(groupId, groupProfileUpdated)
                m.updateGroup(groupInfo)
                welcomeText = welcome ?? ""
            } catch let error {
                logger.error("apiUpdateGroup error: \(responseError(error))")
            }
        }
    }
}

struct GroupWelcomeView_Previews: PreviewProvider {
    static var previews: some View {
        GroupWelcomeView(groupId: 1, groupInfo: Binding.constant(GroupInfo.sampleData))
    }
}
