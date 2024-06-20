//
//  ProfilePrivacyView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 17/03/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct HiddenProfileView: View {
    @State var user: User
    @Binding var profileHidden: Bool
    @EnvironmentObject private var m: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var hidePassword = ""
    @State private var confirmHidePassword = ""
    @State private var saveErrorAlert = false
    @State private var savePasswordError: String?

    var body: some View {
        List {
            Text("Hide profile")
                .font(.title)
                .bold()
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .listRowBackground(Color.clear)

            Section() {
                ProfilePreview(profileOf: user)
                    .padding(.leading, -8)
            }

            Section {
                PassphraseField(key: $hidePassword, placeholder: "Password to show", valid: passwordValid, showStrength: true)
                PassphraseField(key: $confirmHidePassword, placeholder: "Confirm password", valid: confirmValid)

                settingsRow("lock") {
                    Button("Save profile password") {
                        Task {
                            do {
                                let u = try await apiHideUser(user.userId, viewPwd: hidePassword)
                                await MainActor.run {
                                    m.updateUser(u)
                                    dismiss()
                                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                                        withAnimation { profileHidden = true }
                                    }
                                }
                            } catch let error {
                                saveErrorAlert = true
                                savePasswordError = responseError(error)
                            }
                        }
                    }
                }
                .disabled(saveDisabled)
            } header: {
                Text("Hidden profile password")
            } footer: {
                Text("To reveal your hidden profile, enter a full password into a search field in **Your chat profiles** page.")
                    .font(.body)
                    .padding(.top, 8)
            }
        }
        .alert(isPresented: $saveErrorAlert) {
            Alert(
                title: Text("Error saving user password"),
                message: Text(savePasswordError ?? "")
            )
        }
        .modifier(ThemedBackground())
    }

    var passwordValid: Bool { hidePassword == hidePassword.trimmingCharacters(in: .whitespaces) }

    var confirmValid: Bool { confirmHidePassword == "" || hidePassword == confirmHidePassword }

    var saveDisabled: Bool { hidePassword == "" || !passwordValid || confirmHidePassword == "" || !confirmValid }
}

struct ProfilePrivacyView_Previews: PreviewProvider {
    static var previews: some View {
        HiddenProfileView(user: User.sampleData, profileHidden: Binding.constant(false))
    }
}
