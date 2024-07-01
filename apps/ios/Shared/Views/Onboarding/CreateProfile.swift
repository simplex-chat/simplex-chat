//
//  CreateProfile.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum UserProfileAlert: Identifiable {
    case duplicateUserError
    case invalidDisplayNameError
    case createUserError(error: LocalizedStringKey)
    case invalidNameError(validName: String)

    var id: String {
        switch self {
        case .duplicateUserError: return "duplicateUserError"
        case .invalidDisplayNameError: return "invalidDisplayNameError"
        case .createUserError: return "createUserError"
        case let .invalidNameError(validName): return "invalidNameError \(validName)"
        }
    }
}

struct CreateProfile: View {
    @Environment(\.dismiss) var dismiss
    @State private var displayName: String = ""
    @FocusState private var focusDisplayName
    @State private var alert: UserProfileAlert?

    var body: some View {
        List {
            Section {
                TextField("Enter your name…", text: $displayName)
                    .focused($focusDisplayName)
                Button {
                    createProfile(displayName, showAlert: { alert = $0 }, dismiss: dismiss)
                } label: {
                    Label("Create profile", systemImage: "checkmark")
                }
                .disabled(!canCreateProfile(displayName))
            } header: {
                HStack {
                    Text("Your profile")
                    let name = displayName.trimmingCharacters(in: .whitespaces)
                    let validName = mkValidName(name)
                    if name != validName {
                        Spacer()
                        Image(systemName: "exclamationmark.circle")
                            .foregroundColor(.red)
                            .onTapGesture {
                                alert = .invalidNameError(validName: validName)
                            }
                    }
                }
                .frame(height: 20)
            } footer: {
                VStack(alignment: .leading, spacing: 8) {
                    Text("Your profile, contacts and delivered messages are stored on your device.")
                    Text("The profile is only shared with your contacts.")
                }
                .frame(maxWidth: .infinity, alignment: .leading)
            }
        }
        .navigationTitle("Create your profile")
        .modifier(ThemedBackground(grouped: true))
        .alert(item: $alert) { a in userProfileAlert(a, $displayName) }
        .onAppear() {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                focusDisplayName = true
            }
        }
        .keyboardPadding()
    }
}

struct CreateFirstProfile: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss
    @State private var displayName: String = ""
    @FocusState private var focusDisplayName

    var body: some View {
        VStack(alignment: .leading) {
            Group {
                Text("Create your profile")
                    .font(.largeTitle)
                    .bold()
                Text("Your profile, contacts and delivered messages are stored on your device.")
                    .foregroundColor(theme.colors.secondary)
                Text("The profile is only shared with your contacts.")
                    .foregroundColor(theme.colors.secondary)
                    .padding(.bottom)
            }
            .padding(.bottom)

            ZStack(alignment: .topLeading) {
                let name = displayName.trimmingCharacters(in: .whitespaces)
                let validName = mkValidName(name)
                if name != validName {
                    Button {
                        showAlert(.invalidNameError(validName: validName))
                    } label: {
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                    }
                } else {
                    Image(systemName: "exclamationmark.circle").foregroundColor(.clear)
                }
                TextField("Enter your name…", text: $displayName)
                    .focused($focusDisplayName)
                    .padding(.leading, 32)
            }
            .padding(.bottom)
            Spacer()
            onboardingButtons()
        }
        .onAppear() {
            focusDisplayName = true
            setLastVersionDefault()
        }
        .padding()
        .frame(maxWidth: .infinity, alignment: .leading)
        .keyboardPadding()
    }

    func onboardingButtons() -> some View {
        HStack {
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

            Spacer()

            Button {
                createProfile(displayName, showAlert: showAlert, dismiss: dismiss)
            } label: {
                HStack {
                    Text("Create")
                    Image(systemName: "greaterthan")
                }
            }
            .disabled(!canCreateProfile(displayName))
        }
    }

    private func showAlert(_ alert: UserProfileAlert) {
        AlertManager.shared.showAlert(userProfileAlert(alert, $displayName))
    }
}

private func createProfile(_ displayName: String, showAlert: (UserProfileAlert) -> Void, dismiss: DismissAction) {
    hideKeyboard()
    let profile = Profile(
        displayName: displayName.trimmingCharacters(in: .whitespaces),
        fullName: ""
    )
    let m = ChatModel.shared
    do {
        AppChatState.shared.set(.active)
        m.currentUser = try apiCreateActiveUser(profile)
        // .isEmpty check is redundant here, but it makes it clearer what is going on
        if m.users.isEmpty || m.users.allSatisfy({ $0.user.hidden }) {
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
                showAlert(.duplicateUserError)
            }
        case .chatCmdError(_, .error(.invalidDisplayName)):
            if m.currentUser == nil {
                AlertManager.shared.showAlert(invalidDisplayNameAlert)
            } else {
                showAlert(.invalidDisplayNameError)
            }
        default:
            let err: LocalizedStringKey = "Error: \(responseError(error))"
            if m.currentUser == nil {
                AlertManager.shared.showAlert(creatUserErrorAlert(err))
            } else {
                showAlert(.createUserError(error: err))
            }
        }
        logger.error("Failed to create user or start chat: \(responseError(error))")
    }
}

private func canCreateProfile(_ displayName: String) -> Bool {
    let name = displayName.trimmingCharacters(in: .whitespaces)
    return name != "" && mkValidName(name) == name
}

func userProfileAlert(_ alert: UserProfileAlert, _ displayName: Binding<String>) -> Alert {
    switch alert {
    case .duplicateUserError: return duplicateUserAlert
    case .invalidDisplayNameError: return invalidDisplayNameAlert
    case let .createUserError(err): return creatUserErrorAlert(err)
    case let .invalidNameError(name): return createInvalidNameAlert(name, displayName)
    }
}

private var duplicateUserAlert: Alert {
    Alert(
        title: Text("Duplicate display name!"),
        message: Text("You already have a chat profile with the same display name. Please choose another name.")
    )
}

private var invalidDisplayNameAlert: Alert {
    Alert(
        title: Text("Invalid display name!"),
        message: Text("This display name is invalid. Please choose another name.")
    )
}

private func creatUserErrorAlert(_ err: LocalizedStringKey) -> Alert {
    Alert(
        title: Text("Error creating profile!"),
        message: Text(err)
    )
}

func createInvalidNameAlert(_ name: String, _ displayName: Binding<String>) -> Alert {
    name == ""
    ? Alert(title: Text("Invalid name!"))
    : Alert(
        title: Text("Invalid name!"),
        message: Text("Correct name to \(name)?"),
        primaryButton: .default(
            Text("Ok"),
            action: { displayName.wrappedValue = name }
        ),
        secondaryButton: .cancel()
    )
}

func validDisplayName(_ name: String) -> Bool {
    mkValidName(name.trimmingCharacters(in: .whitespaces)) == name
}

func mkValidName(_ s: String) -> String {
    var c = s.cString(using: .utf8)!
    return fromCString(chat_valid_name(&c)!)
}

struct CreateProfile_Previews: PreviewProvider {
    static var previews: some View {
        CreateProfile()
    }
}
