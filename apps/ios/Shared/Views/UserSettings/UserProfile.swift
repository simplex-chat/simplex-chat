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
    @EnvironmentObject var ss: SaveableSettings
    @AppStorage(DEFAULT_PROFILE_IMAGE_CORNER_RADIUS) private var radius = defaultProfileImageCorner
    @State private var profile = Profile(displayName: "", fullName: "")
    @State private var currentProfileHash: Int?
    @State private var loaded = false
    @State private var shortDescr = ""
    @State private var description = ""
    // Modals
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var alert: UserProfileAlert?
    @FocusState private var focusDisplayName

    var body: some View {
        List {
            EditProfileImage(profileImage: $profile.image, iconName: "person.crop.circle.fill", showChooseSource: $showChooseSource)
                .padding(.top)

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
                HStack {
                    TextField("Bio", text: $shortDescr)
                    if !bioFitsLimit() {
                        Button {
                            showAlert(NSLocalizedString("Bio too large", comment: "alert title"))
                        } label: {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    }
                }
                NavigationLink {
                    ProfileDescriptionEditor(description: $description)
                        .navigationTitle("Description")
                        .modifier(ThemedBackground(grouped: true))
                } label: {
                    Text(description.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty ? "Add description" : "Edit description")
                }
            } footer: {
                Text("Your profile is stored on your device and shared only with your contacts. SimpleX servers cannot see your profile.")
            }

            Section {
                Button(action: getCurrentProfile) {
                    Text("Reset")
                }
                .disabled(
                    currentProfileHash == profile.hashValue &&
                    (profile.shortDescr ?? "") == shortDescr.trimmingCharacters(in: .whitespaces) &&
                    (profile.description ?? "") == description.trimmingCharacters(in: .whitespacesAndNewlines)
                )
                Button(action: saveProfile) {
                    Text("Save (and notify contacts)")
                }
                .disabled(!canSaveProfile)
            }
        }
        // Lifecycle
        .onAppear {
            // load once — returning from the description editor re-fires onAppear and would discard edits
            if !loaded {
                getCurrentProfile()
                loaded = true
            }
        }
        .onChange(of: editSnapshot) { _ in updateProfileSaver() }
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

    private func showFullName(_ user: User) -> Bool {
        user.profile.fullName != "" && user.profile.fullName != user.profile.displayName
    }

    private func bioFitsLimit() -> Bool {
        chatJsonLength(shortDescr) <= MAX_BIO_LENGTH_BYTES
    }

    private var canSaveProfile: Bool {
        (
            currentProfileHash != profile.hashValue ||
            (chatModel.currentUser?.profile.shortDescr ?? "") != shortDescr.trimmingCharacters(in: .whitespaces) ||
            (chatModel.currentUser?.profile.description ?? "") != description.trimmingCharacters(in: .whitespacesAndNewlines)
        ) &&
        profile.displayName.trimmingCharacters(in: .whitespaces) != "" &&
        validDisplayName(profile.displayName) &&
        bioFitsLimit()
    }

    private func saveProfile() {
        focusDisplayName = false
        Task {
            do {
                profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
                profile.shortDescr = shortDescr.trimmingCharacters(in: .whitespaces)
                let d = description.trimmingCharacters(in: .whitespacesAndNewlines)
                profile.description = d.isEmpty ? nil : d
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
            shortDescr = profile.shortDescr ?? ""
            description = profile.description ?? ""
        }
    }

    private var editSnapshot: [String] {
        [profile.displayName, profile.fullName, profile.image ?? "", shortDescr, description]
    }

    private func updateProfileSaver() {
        guard loaded, canSaveProfile else {
            ss.profileSave = nil
            return
        }
        var edited = profile
        edited.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
        edited.shortDescr = shortDescr.trimmingCharacters(in: .whitespaces)
        let d = description.trimmingCharacters(in: .whitespacesAndNewlines)
        edited.description = d.isEmpty ? nil : d
        ss.profileSave = {
            Task {
                do {
                    if let (newProfile, _) = try await apiUpdateProfile(profile: edited) {
                        await MainActor.run { ChatModel.shared.updateCurrentUser(newProfile) }
                    }
                } catch {
                    logger.error("UserProfile save on dismiss error: \(responseError(error))")
                }
            }
        }
    }
}

struct EditProfileImage: View {
    @EnvironmentObject var theme: AppTheme
    @AppStorage(DEFAULT_PROFILE_IMAGE_CORNER_RADIUS) private var radius = defaultProfileImageCorner
    @Binding var profileImage: String?
    var iconName: String
    @Binding var showChooseSource: Bool

    var body: some View {
        Group {
            if profileImage != nil {
                ZStack(alignment: .bottomTrailing) {
                    ZStack(alignment: .topTrailing) {
                        ProfileImage(imageStr: profileImage, size: 160)
                            .onTapGesture { showChooseSource = true }
                        overlayButton("multiply", edge: .top) { profileImage = nil }
                    }
                    overlayButton("camera", edge: .bottom) { showChooseSource = true }
                }
            } else {
                ZStack(alignment: .center) {
                    ProfileImage(imageStr: profileImage, iconName: iconName, size: 160)
                    editImageButton { showChooseSource = true }
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
        .listRowBackground(Color.clear)
        .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
        .contentShape(Rectangle())
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

struct ProfileDescriptionEditor: View {
    @EnvironmentObject var theme: AppTheme
    @Binding var description: String
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        List {
            Section {
                if #available(iOS 16.0, *) {
                    TextField("Enter description (optional)", text: $description, axis: .vertical)
                        .lineLimit(6...12)
                        .focused($keyboardVisible)
                } else {
                    // iOS 15 has no vertically-growing TextField (axis:) — fixed-height editor instead
                    ZStack {
                        Group {
                            if description.isEmpty {
                                TextEditor(text: Binding.constant(NSLocalizedString("Enter description (optional)", comment: "placeholder")))
                                    .foregroundColor(theme.colors.secondary)
                                    .disabled(true)
                            }
                            TextEditor(text: $description)
                                .focused($keyboardVisible)
                        }
                        .padding(.horizontal, -5)
                        .padding(.top, -8)
                        .frame(height: 130, alignment: .topLeading)
                        .frame(maxWidth: .infinity, alignment: .leading)
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
}
