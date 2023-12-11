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
    @State private var profile = Profile(displayName: "", fullName: "")
    @State private var editProfile = false
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var alert: UserProfileAlert?
    @FocusState private var focusDisplayName

    var body: some View {
        let user: User = chatModel.currentUser!

        return VStack(alignment: .leading) {
            Text("Your profile is stored on your device and shared only with your contacts.\nSimpleX servers cannot see your profile.")
                .padding(.bottom)

            if editProfile {
                ZStack(alignment: .center) {
                    ZStack(alignment: .topTrailing) {
                        profileImageView(profile.image)
                        if user.image != nil {
                            Button {
                                profile.image = nil
                            } label: {
                                Image(systemName: "multiply")
                                    .resizable()
                                    .aspectRatio(contentMode: .fit)
                                    .frame(width: 12)
                            }
                        }
                    }

                    editImageButton { showChooseSource = true }
                }
                .frame(maxWidth: .infinity, alignment: .center)

                VStack(alignment: .leading) {
                    ZStack(alignment: .leading) {
                        if !validNewProfileName(user) {
                            Button {
                                alert = .invalidNameError(validName: mkValidName(profile.displayName))
                            } label: {
                                Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                            }
                        } else {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.clear)
                        }
                        profileNameTextEdit("Profile name", $profile.displayName)
                            .focused($focusDisplayName)
                    }
                    .padding(.bottom)
                    if showFullName(user) {
                        profileNameTextEdit("Full name (optional)", $profile.fullName)
                            .padding(.bottom)
                    }
                    HStack(spacing: 20) {
                        Button("Cancel") { editProfile = false }
                        Button("Save (and notify contacts)") { saveProfile() }
                            .disabled(!canSaveProfile(user))
                    }
                }
                .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
            } else {
                ZStack(alignment: .center) {
                    profileImageView(user.image)
                        .onTapGesture { startEditingImage(user) }

                    if user.image == nil {
                        editImageButton { startEditingImage(user) }
                    }
                }
                .frame(maxWidth: .infinity, alignment: .center)

                VStack(alignment: .leading) {
                    profileNameView("Profile name:", user.profile.displayName)
                    if showFullName(user) {
                        profileNameView("Full name:", user.profile.fullName)
                    }
                    Button("Edit") {
                        profile = fromLocalProfile(user.profile)
                        editProfile = true
                        focusDisplayName = true
                    }
                }
                .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
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
        .onChange(of: chosenImage) { image in
            if let image = image {
                profile.image = resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
            } else {
                profile.image = nil
            }
        }
        .alert(item: $alert) { a in userProfileAlert(a, $profile.displayName) }
    }

    func profileNameTextEdit(_ label: LocalizedStringKey, _ name: Binding<String>) -> some View {
        TextField(label, text: name)
            .padding(.leading, 32)
    }

    func profileNameView(_ label: LocalizedStringKey, _ name: String) -> some View {
        HStack {
            Text(label)
            Text(name).fontWeight(.bold)
        }
        .padding(.bottom)
    }

    func startEditingImage(_ user: User) {
        profile = fromLocalProfile(user.profile)
        editProfile = true
        showChooseSource = true
    }

    private func validNewProfileName(_ user: User) -> Bool {
        profile.displayName == user.profile.displayName || validDisplayName(profile.displayName.trimmingCharacters(in: .whitespaces))
    }

    private func showFullName(_ user: User) -> Bool {
        user.profile.fullName != "" && user.profile.fullName != user.profile.displayName
    }

    private func canSaveProfile(_ user: User) -> Bool {
        profile.displayName.trimmingCharacters(in: .whitespaces) != "" && validNewProfileName(user)
    }

    func saveProfile() {
        Task {
            do {
                profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
                if let (newProfile, _) = try await apiUpdateProfile(profile: profile) {
                    DispatchQueue.main.async {
                        chatModel.updateCurrentUser(newProfile)
                        profile = newProfile
                    }
                    editProfile = false
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
    ProfileImage(imageStr: imageStr)
        .aspectRatio(1, contentMode: .fit)
        .frame(maxWidth: 192, maxHeight: 192)
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

struct UserProfile_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel1 = ChatModel()
        chatModel1.currentUser = User.sampleData
        let chatModel2 = ChatModel()
        chatModel2.currentUser = User.sampleData
        chatModel2.currentUser?.profile.image = "data:image/jpg;base64,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"
        return Group {
            UserProfile()
                .environmentObject(chatModel1)
            UserProfile()
                .environmentObject(chatModel2)
        }
    }
}
