//
//  UserProfile.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserProfile: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @AppStorage(DEFAULT_PROFILE_IMAGE_CORNER_RADIUS) private var radius = defaultProfileImageCorner
    @State private var profile = Profile(displayName: "", fullName: "")
    @State private var currentProfileHash: Int?
    // Modals
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var alert: UserProfileAlert?
    @FocusState private var focusDisplayName

    var body: some View {
        List {
            Group {
                if profile.image != nil {
                    ZStack(alignment: .bottomTrailing) {
                        ZStack(alignment: .topTrailing) {
                            profileImageView(profile.image)
                                .onTapGesture { showChooseSource = true }
                            overlayButton("multiply", edge: .top) { profile.image = nil }
                        }
                        overlayButton("camera", edge: .bottom) { showChooseSource = true }
                    }
                } else {
                    ZStack(alignment: .center) {
                        profileImageView(profile.image)
                        editImageButton { showChooseSource = true }
                    }
                }
            }
            .frame(maxWidth: .infinity, alignment: .center)
            .listRowBackground(Color.clear)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            .padding(.top)
            .contentShape(Rectangle())

            Section {
                HStack {
                    TextField("Enter your name…", text: $profile.displayName)
                        .focused($focusDisplayName)
                    if !validDisplayName(profile.displayName) {
                        Button {
                            alert = .invalidNameError(validName: mkValidName(profile.displayName))
                        } label: {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    }
                }
                if let user = chatModel.currentUser, showFullName(user) {
                    TextField("Full name (optional)", text: $profile.fullName)
                }
            } footer: {
                Text("Your profile is stored on your device and shared only with your contacts. SimpleX servers cannot see your profile.")
            }

            Section {
                Button(action: getCurrentProfile) {
                    Text("Reset")
                }
                .disabled(currentProfileHash == profile.hashValue)
                Button(action: saveProfile) {
                    Text("Save (and notify contacts)")
                }
                .disabled(!canSaveProfile)
            }
        }
        // Lifecycle
        .onAppear {
            getCurrentProfile()
        }
        .onDisappear {
            if canSaveProfile {
                showAlert(
                    title: NSLocalizedString("Save your profile?", comment: "alert title"),
                    message: NSLocalizedString("Your profile was changed. If you save it, the updated profile will be sent to all your contacts.", comment: "alert message"),
                    buttonTitle: NSLocalizedString("Save (and notify contacts)", comment: "alert button"),
                    buttonAction: saveProfile,
                    cancelButton: true
                )
            }
        }
        .onChange(of: chosenImage) { image in
            Task {
                let resized: String? = if let image {
                    await resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
                } else {
                    nil
                }
                await MainActor.run { profile.image = resized }
            }
        }
        // Modals
        .confirmationDialog("Profile image", isPresented: $showChooseSource, titleVisibility: .visible) {
            Button("Take picture") {
                showTakePhoto = true
            }
            Button("Choose from library") {
                showImagePicker = true
            }
            if UIPasteboard.general.hasImages {
                Button("Paste image") {
                    chosenImage = UIPasteboard.general.image
                }
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
        .alert(item: $alert) { a in userProfileAlert(a, $profile.displayName) }
    }

    private func overlayButton(
        _ systemName: String,
        edge: Edge.Set,
        action: @escaping () -> Void
    ) -> some View {
        Image(systemName: systemName)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(height: 12)
            .foregroundColor(theme.colors.primary)
            .padding(6)
            .frame(width: 36, height: 36, alignment: .center)
            .background(radius >= 20 ? Color.clear : theme.colors.background.opacity(0.5))
            .clipShape(Circle())
            .contentShape(Circle())
            .padding([.trailing, edge], -12)
            .onTapGesture(perform: action)
    }

    private func showFullName(_ user: User) -> Bool {
        user.profile.fullName != "" && user.profile.fullName != user.profile.displayName
    }
    
    private var canSaveProfile: Bool {
        currentProfileHash != profile.hashValue &&
        profile.displayName.trimmingCharacters(in: .whitespaces) != "" &&
        validDisplayName(profile.displayName)
    }

    private func saveProfile() {
        focusDisplayName = false
        Task {
            do {
                profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
                if let (newProfile, _) = try await apiUpdateProfile(profile: profile) {
                    await MainActor.run {
                        chatModel.updateCurrentUser(newProfile)
                        getCurrentProfile()
                    }
                } else {
                    alert = .duplicateUserError
                }
            } catch {
                logger.error("UserProfile apiUpdateProfile error: \(responseError(error))")
            }
        }
    }

    private func getCurrentProfile() {
        if let user = chatModel.currentUser {
            profile = fromLocalProfile(user.profile)
            currentProfileHash = profile.hashValue
        }
    }
}

func profileImageView(_ imageStr: String?) -> some View {
    ProfileImage(imageStr: imageStr, size: 192)
}

func editImageButton(action: @escaping () -> Void) -> some View {
    Button {
        action()
    } label: {
        Image(systemName: "camera")
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: 48)
    }
}
