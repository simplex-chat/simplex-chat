//
//  CreateProfile.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/navigation.md

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

let MAX_BIO_LENGTH_BYTES = 160

struct CreateProfile: View {
    @Environment(\.colorScheme) var colorScheme
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var theme: AppTheme
    @State private var displayName: String = ""
    @State private var profileBio: String = ""
    @FocusState private var focusDisplayName
    @State private var alert: UserProfileAlert?
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var profileImage: String? = nil

    var body: some View {
        List {
            Group {
                HStack(spacing: 0) {
                    Spacer(minLength: 0)
                    ZStack(alignment: .center) {
                        ZStack(alignment: .topTrailing) {
                            ProfileImage(imageStr: profileImage, size: 128)
                            if profileImage != nil {
                                Button {
                                    profileImage = nil
                                } label: {
                                    Image(systemName: "multiply")
                                        .resizable()
                                        .aspectRatio(contentMode: .fit)
                                        .frame(width: 12)
                                }
                            }
                        }

                        editImageButton { showChooseSource = true }
                            .buttonStyle(BorderlessButtonStyle())
                    }
                    .padding(.horizontal, 10) // Offsets transparent space built into 3D asset
                    Spacer(minLength: 0)
                    #if SIMPLEX_ASSETS
                    Image(colorScheme == .light ? "create-profile" : "create-profile-light")
                        .resizable()
                        .scaledToFit()
                        .frame(height: 140)
                    // No trailing spacer — asset image has empty space on the right
                    #endif
                }
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .listRowInsets(EdgeInsets(top: 8, leading: 0, bottom: 0, trailing: 0))

            Section {
                ZStack(alignment: .leading) {
                    let name = displayName.trimmingCharacters(in: .whitespaces)
                    if name != mkValidName(name) {
                        Button {
                            alert = .invalidNameError(validName: mkValidName(name))
                        } label: {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    } else {
                        Image(systemName: "pencil").foregroundColor(theme.colors.secondary)
                    }
                    TextField("Enter your name…", text: $displayName)
                        .padding(.leading, 36)
                        .focused($focusDisplayName)
                }
                ZStack(alignment: .leading) {
                    Image(systemName: "pencil").foregroundColor(theme.colors.secondary)
                    TextField("Bio", text: $profileBio)
                        .padding(.leading, 36)
                }
                Button(action: createProfile) {
                    settingsRow("checkmark", color: theme.colors.primary) { Text("Create profile") }
                }
                .disabled(!canCreateProfile(displayName) || !bioFitsLimit())
            } footer: {
                VStack(alignment: .leading, spacing: 8) {
                    Text("Your profile is stored on your device and only shared with your contacts.")
                }
                .foregroundColor(theme.colors.secondary)
                .frame(maxWidth: .infinity, alignment: .leading)
            }
            .compactSectionSpacing()
        }
        .navigationTitle("Create your profile")
        .modifier(ThemedBackground(grouped: true))
        .alert(item: $alert) { a in userProfileAlert(a, $displayName) }
        .confirmationDialog("Profile image", isPresented: $showChooseSource, titleVisibility: .visible) {
            Button("Take picture") {
                showTakePhoto = true
            }
            Button("Choose from library") {
                showImagePicker = true
            }
        }
        .fullScreenCover(isPresented: $showTakePhoto) {
            ZStack {
                Color.black.edgesIgnoringSafeArea(.all)
                CameraImagePicker(image: $chosenImage)
            }
        }
        .sheet(isPresented: $showImagePicker) {
            LibraryImagePicker(image: $chosenImage) { _ in
                await MainActor.run {
                    showImagePicker = false
                }
            }
        }
        .onChange(of: chosenImage) { image in
            Task {
                let resized: String? = if let image {
                    await resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
                } else {
                    nil
                }
                await MainActor.run { profileImage = resized }
            }
        }
        .onAppear() {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                focusDisplayName = true
            }
        }
    }

    private func bioFitsLimit() -> Bool {
        chatJsonLength(profileBio) <= MAX_BIO_LENGTH_BYTES
    }

    private func createProfile() {
        hideKeyboard()
        let shortDescr: String? = if profileBio.isEmpty { nil } else { profileBio }
        let profile = Profile(
            displayName: displayName.trimmingCharacters(in: .whitespaces),
            fullName: "",
            shortDescr: shortDescr,
            image: profileImage
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
    @Environment(\.colorScheme) var colorScheme: ColorScheme
    @State private var displayName: String = ""
    @FocusState private var focusDisplayName
    @State private var nextStepNavLinkActive = false
    @State private var showMigrateSheet = false
    var body: some View {
        let spacing: CGFloat = 10
        let topPadding: CGFloat = 8
        let padding: CGFloat = 25
        GeometryReader { g in
            let v = ScrollView {
                VStack(alignment: .center, spacing: spacing) {
                    #if SIMPLEX_ASSETS
                    Image(colorScheme == .light ? "your-profile" : "your-profile-light")
                        .resizable()
                        .scaledToFit()
                        .frame(maxWidth: .infinity)
                    #else
                    ZStack {
                        let gp = OnboardingCardView.gradientPoints(aspectRatio: 1.0, scale: colorScheme == .light ? 1.2 : 1.5)
                        LinearGradient(
                            stops: colorScheme == .light ? OnboardingCardView.lightStops : OnboardingCardView.darkStops,
                            startPoint: gp.start,
                            endPoint: gp.end
                        )
                        Image(systemName: "person.crop.rectangle")
                            .font(.system(size: 72))
                            .foregroundColor(theme.colors.primary)
                    }
                    .aspectRatio(1.0, contentMode: .fit)
                    .clipShape(RoundedRectangle(cornerRadius: 24))
                    .padding(.horizontal, 25)
                    .frame(maxWidth: .infinity)
                    #endif

                    Text("Your profile")
                        .font(.largeTitle)
                        .bold()
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    Text("On your phone, not on servers.")
                        .font(.title3)
                        .fontWeight(.medium)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    Text("No account. No phone. No email. No ID.\nThe most secure encryption.")
                        .font(.footnote)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    profileNameField()
                        .padding(.top)
                        .padding(.bottom, 5)

                    Spacer(minLength: 0)

                    createProfileButton()
                        .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
                }
                .padding(.horizontal, padding)
                .padding(.top, topPadding)
                .padding(.bottom, padding)
                .frame(minHeight: g.size.height)
            }
            .onTapGesture { focusDisplayName = false }
            .sheet(isPresented: $showMigrateSheet, onDismiss: { m.migrationState = nil }) {
                NavigationView {
                    MigrateToDevice(migrationState: $m.migrationState)
                        .navigationTitle("Migrate here")
                        .modifier(ThemedBackground(grouped: true))
                }
            }
            if #available(iOS 17, *) {
                v.scrollBounceBehavior(.basedOnSize).defaultScrollAnchor(.bottom)
            } else if #available(iOS 16.4, *) {
                v.scrollBounceBehavior(.basedOnSize)
            } else {
                v
            }
        }
        .toolbar {
            ToolbarItem(placement: .navigationBarTrailing) {
                Button {
                    if m.migrationState == nil {
                        m.migrationState = .pasteOrScanLink
                    }
                    showMigrateSheet = true
                } label: {
                    HStack(spacing: 4) {
                        Image(systemName: "tray.and.arrow.down")
                        Text("Migrate")
                            .fontWeight(.medium)
                    }
                }
            }
        }
        .onAppear() {
            if #available(iOS 16, *) {
                focusDisplayName = true
            } else {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                    focusDisplayName = true
                }
            }
        }
        .frame(maxHeight: .infinity)
    }

    private func profileNameField() -> some View {
        let name = displayName.trimmingCharacters(in: .whitespaces)
        let validName = mkValidName(name)
        return ZStack(alignment: .trailing) {
            TextField("Enter profile name...", text: $displayName)
                .focused($focusDisplayName)
                .padding(.horizontal)
                .padding(.trailing, name != validName ? 20 : 0)
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

    private func createProfileButton() -> some View {
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
        YourNetworkView()
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
