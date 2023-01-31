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
        case duplicateUserError(err: LocalizedStringKey)

        var id: String {
            switch self {
            case let .duplicateUserError(err): return "error \(err)"
            }
        }
    }

    var body: some View {
        VStack(alignment: .leading) {
            Text("Create your profile")
                .font(.largeTitle)
                .padding(.bottom, 4)
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
            case let .duplicateUserError(err: err):
                return Alert(
                    title: Text("Duplicate display name!"),
                    message: Text(err)
                )
            }
        }
        .padding()
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
                withAnimation { m.onboardingStage = .step3_SetNotificationsMode }
            } else {
                dismiss()
                m.users = try listUsers()
                try getUserChatData()
            }
        } catch {
            switch error {
            case ChatResponse.chatCmdError(_, .errorStore(storeError: .duplicateName)),
                 ChatResponse.chatCmdError(_, .error(errorType: .userExists)):
                alert = .duplicateUserError(err: "You already have a chat profile with the same display name. Please choose another name.")
                AlertManager.shared.showAlertMsg(
                    title: "Duplicate display name!",
                    message: "You already have a chat profile with the same display name. Please choose another name."
                )
            default:
                alert = .duplicateUserError(err: "Error: \(String(describing: error))")
                AlertManager.shared.showAlertMsg(
                    title: "Duplicate display name!",
                    message: "Error: \(String(describing: error))"
                )
            }
            logger.error("Failed to create user or start chat: \(responseError(error))")
        }
    }

    func canCreateProfile() -> Bool {
        displayName != "" && validDisplayName(displayName)
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
