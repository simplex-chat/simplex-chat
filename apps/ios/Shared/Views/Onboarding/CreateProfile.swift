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
    @EnvironmentObject var theme: AppTheme
    @State private var displayName: String = ""
    @FocusState private var focusDisplayName
    @State private var alert: UserProfileAlert?

    var body: some View {
        List {
            Section {
                TextField("Enter your name…", text: $displayName)
                    .focused($focusDisplayName)
                Button {
                    createProfile()
                } label: {
                    Label("Create profile", systemImage: "checkmark")
                }
                .disabled(!canCreateProfile(displayName))
            } header: {
                HStack {
                    Text("Your profile")
                        .foregroundColor(theme.colors.secondary)

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
                    Text("Your profile is stored on your device and only shared with your contacts.")
                }
                .foregroundColor(theme.colors.secondary)
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
    }

    private func createProfile() {
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
                    onboardingStageDefault.set(.step3_ChooseServerOperators)
                    m.onboardingStage = .step3_ChooseServerOperators
                }
            } else {
                onboardingStageDefault.set(.onboardingComplete)
                m.onboardingStage = .onboardingComplete
                dismiss()
                m.users = try listUsers()
                try getUserChatData()
            }
        } catch let error {
            showCreateProfileAlert(showAlert: { alert = $0 }, error)
        }
    }
}

struct CreateFirstProfile: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss
    @State private var displayName: String = ""
    @FocusState private var focusDisplayName
    @State private var nextStepNavLinkActive = false

    var body: some View {
        let v = VStack(alignment: .leading, spacing: 16) {
            VStack(alignment: .center, spacing: 16) {
                Text("Create profile")
                    .font(.largeTitle)
                    .bold()
                    .multilineTextAlignment(.center)

                Text("Your profile is stored on your device and only shared with your contacts.")
                    .font(.callout)
                    .foregroundColor(theme.colors.secondary)
                    .multilineTextAlignment(.center)
            }
            .fixedSize(horizontal: false, vertical: true)
            .frame(maxWidth: .infinity) // Ensures it takes up the full width
            .padding(.horizontal, 10)
            .onTapGesture { focusDisplayName = false }

            HStack {
                let name = displayName.trimmingCharacters(in: .whitespaces)
                let validName = mkValidName(name)
                ZStack(alignment: .trailing) {
                    TextField("Enter your name…", text: $displayName)
                        .focused($focusDisplayName)
                        .padding(.horizontal)
                        .padding(.trailing, 20)
                        .padding(.vertical, 10)
                        .background(
                            RoundedRectangle(cornerRadius: 10, style: .continuous)
                                .fill(Color(uiColor: .tertiarySystemFill))
                        )
                    if name != validName {
                        Button {
                            showAlert(.invalidNameError(validName: validName))
                        } label: {
                            Image(systemName: "exclamationmark.circle")
                                .foregroundColor(.red)
                                .padding(.horizontal, 10)
                        }
                    }
                }
            }
            .padding(.top)

            Spacer()

            VStack(spacing: 10) {
                createProfileButton()
                if !focusDisplayName {
                    onboardingButtonPlaceholder()
                }
            }
        }
        .onAppear() {
            if #available(iOS 16, *) {
                focusDisplayName = true
            } else {
                // it does not work before animation completes on iOS 15
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                    focusDisplayName = true
                }
            }
        }
        .padding(.horizontal, 25)
        .padding(.bottom, 25)
        .frame(maxWidth: .infinity, alignment: .leading)
        if #available(iOS 16, *) {
            return v.padding(.top, 10)
        } else {
            return v.padding(.top, 75).ignoresSafeArea(.all, edges: .top)
        }
    }

    func createProfileButton() -> some View {
        ZStack {
            Button {
                createProfile()
            } label: {
                Text("Create profile")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: !canCreateProfile(displayName)))
            .disabled(!canCreateProfile(displayName))

            NavigationLink(isActive: $nextStepNavLinkActive) {
                nextStepDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func showAlert(_ alert: UserProfileAlert) {
        AlertManager.shared.showAlert(userProfileAlert(alert, $displayName))
    }

    private func nextStepDestinationView() -> some View {
        OnboardingConditionsView()
            .navigationBarBackButtonHidden(true)
            .modifier(ThemedBackground())
    }

    private func createProfile() {
        hideKeyboard()
        let profile = Profile(
            displayName: displayName.trimmingCharacters(in: .whitespaces),
            fullName: ""
        )
        let m = ChatModel.shared
        do {
            AppChatState.shared.set(.active)
            m.currentUser = try apiCreateActiveUser(profile)
            try startChat(onboarding: true)
            onboardingStageDefault.set(.step3_ChooseServerOperators)
            nextStepNavLinkActive = true
        } catch let error {
            showCreateProfileAlert(showAlert: showAlert, error)
        }
    }
}

private func showCreateProfileAlert(
    showAlert: (UserProfileAlert) -> Void,
    _ error: Error
) {
    let m = ChatModel.shared
    switch error as? ChatError {
    case .errorStore(.duplicateName),
         .error(.userExists):
        if m.currentUser == nil {
            AlertManager.shared.showAlert(duplicateUserAlert)
        } else {
            showAlert(.duplicateUserError)
        }
    case .error(.invalidDisplayName):
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
