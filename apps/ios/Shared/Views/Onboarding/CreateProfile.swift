//
//  CreateProfile.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CreateProfile: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.dismiss) var dismiss
    @State private var displayName: String = ""
    @State private var fullName: String = ""
    @FocusState private var focusDisplayName
    @FocusState private var focusFullName
    @State private var alert: CreateProfileAlert?

    private enum CreateProfileAlert: Identifiable {
        case duplicateUserError
        case createUserError(error: LocalizedStringKey)

        var id: String {
            switch self {
            case .duplicateUserError: return "duplicateUserError"
            case .createUserError: return "createUserError"
            }
        }
    }

    var body: some View {
        VStack(alignment: .leading) {
            Text("Create your profile")
                .font(.largeTitle)
                .bold()
                .padding(.bottom, 4)
                .frame(maxWidth: .infinity)
            Text("Your profile, contacts and delivered messages are stored on your device.")
                .padding(.bottom, 4)
            Text("The profile is only shared with your contacts.")
                .padding(.bottom)
            ZStack(alignment: .topLeading) {
                if !validDisplayName(displayName) {
                    Image(systemName: "exclamationmark.circle")
                        .foregroundColor(.red)
                        .padding(.top, 4)
                }
                textField("Display name", text: $displayName)
                    .focused($focusDisplayName)
                    .submitLabel(.next)
                    .onSubmit {
                        if canCreateProfile() { focusFullName = true }
                        else { focusDisplayName = true }
                    }
            }
            textField("Full name (optional)", text: $fullName)
                .focused($focusFullName)
                .submitLabel(.go)
                .onSubmit {
                    if canCreateProfile() { createProfile() }
                    else { focusFullName = true }
                }

            Spacer()

            HStack {
                if m.users.isEmpty {
                    Button {
                        hideKeyboard()
                        withAnimation {
                            m.onboardingStage = .step1_SimpleXInfo
                        }
                    } label: {
                        HStack {
                            Image(systemName: "lessthan")
                            Text("About SimpleX")
                        }
                    }
                }

                Spacer()

                HStack {
                    Button {
                        createProfile()
                    } label: {
                        Text("Create")
                        Image(systemName: "greaterthan")
                    }
                    .disabled(!canCreateProfile())
                }
            }
        }
        .onAppear() {
            focusDisplayName = true
            setLastVersionDefault()
        }
        .alert(item: $alert) { a in
            switch a {
            case .duplicateUserError: return duplicateUserAlert
            case let .createUserError(err): return creatUserErrorAlert(err)
            }
        }
        .padding()
        .keyboardPadding()
    }

    func textField(_ placeholder: LocalizedStringKey, text: Binding<String>) -> some View {
        TextField(placeholder, text: text)
            .textInputAutocapitalization(.never)
            .disableAutocorrection(true)
            .padding(.leading, 28)
            .padding(.bottom)
    }

    func createProfile() {
        hideKeyboard()
        let profile = Profile(
            displayName: displayName,
            fullName: fullName
        )
        do {
            m.currentUser = try apiCreateActiveUser(profile)
            if m.users.isEmpty {
                try startChat()
                withAnimation {
                    onboardingStageDefault.set(.step3_CreateSimpleXAddress)
                    m.onboardingStage = .step3_CreateSimpleXAddress
                }
            } else {
                onboardingStageDefault.set(.onboardingComplete)
                m.onboardingStage = .onboardingComplete
                dismiss()
                m.users = try listUsers()
                try getUserChatData()
            }
        } catch let error {
            switch error as? ChatResponse {
            case .chatCmdError(_, .errorStore(.duplicateName)),
                 .chatCmdError(_, .error(.userExists)):
                if m.currentUser == nil {
                    AlertManager.shared.showAlert(duplicateUserAlert)
                } else {
                    alert = .duplicateUserError
                }
            default:
                let err: LocalizedStringKey = "Error: \(responseError(error))"
                if m.currentUser == nil {
                    AlertManager.shared.showAlert(creatUserErrorAlert(err))
                } else {
                    alert = .createUserError(error: err)
                }
            }
            logger.error("Failed to create user or start chat: \(responseError(error))")
        }
    }

    func canCreateProfile() -> Bool {
        displayName != "" && validDisplayName(displayName)
    }

    private var duplicateUserAlert: Alert {
        Alert(
            title: Text("Duplicate display name!"),
            message: Text("You already have a chat profile with the same display name. Please choose another name.")
        )
    }

    private func creatUserErrorAlert(_ err: LocalizedStringKey) -> Alert {
        Alert(
            title: Text("Error creating profile!"),
            message: Text(err)
        )
    }
}

func validDisplayName(_ name: String) -> Bool {
    name.firstIndex(of: " ") == nil && name.first != "@" && name.first != "#"
}

struct CreateProfile_Previews: PreviewProvider {
    static var previews: some View {
        CreateProfile()
    }
}
