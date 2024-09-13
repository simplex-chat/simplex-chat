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
    private enum Focus: Hashable {
        case fullName
        case displayName
    }

    @EnvironmentObject var theme: AppTheme
    @FocusState private var focus: Focus?
    @State private var profile = Profile(displayName: "", fullName: "")
    @State private var currentProfileHash: Int?
    // Modals
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var alert: UserProfileAlert?

    var body: some View {
        List {
            Section {
                if showFullName { TextField("Full name", text: $profile.fullName) }
                TextField("Enter your name…", text: $profile.displayName)
                Button(action: saveProfile) {
                    Label("Save and notify contacts", systemImage: "checkmark")
                }.disabled(!canSaveProfile)
            } header: {
                ProfileImage(imageStr: profile.image, size: 128)
                    .padding(4)
                    .overlay {
                        if profile.image != nil {
                            overlayButton("xmark", alignmnet: .topTrailing) { profile.image = nil }
                            overlayButton("camera", alignmnet: .bottomTrailing) { showChooseSource = true }
                        } else {
                            editImageButton { showChooseSource = true }
                        }
                    }
                    .onTapGesture { showChooseSource = true }
                    .frame(maxWidth: .infinity, alignment: .center)
                    .padding(.bottom)
            } footer: {
                Text("Your profile is stored on your device and shared only with your contacts.\nSimpleX servers cannot see your profile.")
            }
        }
        // Lifecycle
        .task {
            if let user = ChatModel.shared.currentUser {
                profile = fromLocalProfile(user.profile)
                currentProfileHash = profile.hashValue
            }
        }
        .onChange(of: chosenImage) { image in
            if let image {
                profile.image = resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
            } else {
                profile.image = nil
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

    @ViewBuilder
    private func overlayButton(
        _ systemName: String,
        alignmnet: Alignment,
        action: @escaping () -> Void
    ) -> some View {
        Image(systemName: systemName)
            .foregroundStyle(Color.accentColor)
            .font(.system(size: 18, weight: .medium))
            .padding(6)
            .background(Color(.systemBackground).opacity(0.8))
            .clipShape(Circle())
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: alignmnet)
            .onTapGesture(perform: action)
    }

    // MARK: Computed
    private func validNewProfileName(_ user: User) -> Bool {
        profile.displayName == user.profile.displayName || validDisplayName(profile.displayName.trimmingCharacters(in: .whitespaces))
    }

    private var showFullName: Bool {
        profile.fullName != "" &&
        profile.fullName != profile.displayName
    }

    private var canSaveProfile: Bool {
        currentProfileHash != profile.hashValue &&
        profile.displayName.trimmingCharacters(in: .whitespaces) != "" &&
        validDisplayName(profile.displayName)
    }

    private func saveProfile() {
        focus = nil
        Task {
            do {
                profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
                if let (newProfile, _) = try await apiUpdateProfile(profile: profile) {
                    DispatchQueue.main.async {
                        ChatModel.shared.updateCurrentUser(newProfile)
                        profile = newProfile
                        currentProfileHash = newProfile.hashValue
                    }

                } else {
                    alert = .duplicateUserError
                }
            } catch {
                logger.error("UserProfile apiUpdateProfile error: \(responseError(error))")
            }
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
