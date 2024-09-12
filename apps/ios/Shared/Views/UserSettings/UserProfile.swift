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
    @State private var profile = Profile(displayName: "", fullName: "")
    @State private var currentProfileHash: Int?
    @FocusState private var focus
    // Modals
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showFilePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var alert: UserProfileAlert?

    var body: some View {
        List {
            Section {
                Text("""
Your profile is stored on your device and shared only with your contacts.
SimpleX servers cannot see your profile.
"""
                )
                .listRowBackground(Color.clear)
                .listRowInsets(EdgeInsets())
            }
            Section {
                VStack {
                    ProfileImage(imageStr: profile.image, size: 128)
                        .onTapGesture { showChooseSource = true }
                        .padding(4)
                        .overlay {
                            if profile.image != nil {
                                Image(systemName: "xmark")
                                    .foregroundStyle(Color.red)
                                    .padding(8)
                                    .background(Material.bar)
                                    .clipShape(Circle())
                                    .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topTrailing)
                                    .onTapGesture { profile.image = nil }
                            }
                        }
                    Text(profile.image != nil ? "Edit" : "Add")
                        .foregroundStyle(Color.accentColor)
                }
                .onTapGesture { showChooseSource = true }
                .frame(maxWidth: .infinity, alignment: .center)
                if showFullName {
                    nameField("Full name", text: $profile.fullName)
                }
                nameField("Profile name", text: $profile.displayName)
                Button("Save and notify contacts", action: saveProfile).disabled(!canSaveProfile)
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
            Button("Choose file") {
                showFilePicker = true
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
        .fileImporter(isPresented: $showFilePicker, allowedContentTypes: [.image]) { result in
            Task {
                if case let .success(url) = result, url.startAccessingSecurityScopedResource() {
                    if let data = try? Data(contentsOf: url),
                       let image = UIImage(data: data) {
                        await MainActor.run { chosenImage = image }
                    }
                    url.stopAccessingSecurityScopedResource()
                }
            }
        }
        .alert(item: $alert) { a in userProfileAlert(a, $profile.displayName) }
    }

    @ViewBuilder
    private func nameField(
        _ title: LocalizedStringKey,
        text: Binding<String>
    ) -> some View {
        let isValid = validDisplayName(text.wrappedValue)
        VStack(alignment: .leading, spacing: 4) {
            HStack {
                Text(title).foregroundStyle(.secondary).font(.caption)
                Spacer()
                Image(systemName: "exclamationmark.circle")
                    .foregroundColor(.red)
                    .opacity(isValid ? 0 : 1)
                    .onTapGesture {
                        alert = .invalidNameError(validName: mkValidName(profile.displayName))
                    }
            }
            TextField(title, text: text)
                .padding(.vertical, 8)
                .padding(.horizontal, 12)
                .overlay {
                    RoundedRectangle(cornerRadius: 8, style: .continuous)
                        .stroke(isValid ? Color(.tertiaryLabel) : Color.red)
                }
        }.listRowSeparator(.hidden)
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
