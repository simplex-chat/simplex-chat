//
//  GroupProfileView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 29/07/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum GroupProfileAlert: Identifiable {
    case saveError(err: String)
    case invalidName(validName: String)

    var id: String {
        switch self {
        case let .saveError(err): return "saveError \(err)"
        case let .invalidName(validName): return "invalidName \(validName)"
        }
    }
}

struct GroupProfileView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var groupInfo: GroupInfo
    @State var groupProfile: GroupProfile
    @State private var shortDescr: String = ""
    @State private var currentProfileHash: Int?
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var alert: GroupProfileAlert?
    @FocusState private var focusDisplayName

    var body: some View {
        List {
            EditProfileImage(profileImage: $groupProfile.image, showChooseSource: $showChooseSource)
                .if(!focusDisplayName) { $0.padding(.top) }

            Section {
                HStack {
                    TextField("Group display name", text: $groupProfile.displayName)
                        .focused($focusDisplayName)
                    if !validNewProfileName {
                        Button {
                            alert = .invalidName(validName: mkValidName(groupProfile.displayName))
                        } label: {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    }
                }
                let fullName = groupInfo.groupProfile.fullName
                if fullName != "" && fullName != groupProfile.displayName {
                    TextField("Group full name (optional)", text: $groupProfile.fullName)
                }
                HStack {
                    TextField("Short description", text: $shortDescr)
                    if !shortDescrFitsLimit() {
                        Button {
                            showAlert(NSLocalizedString("Description too large", comment: "alert title"))
                        } label: {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    }
                }
            } footer: {
                Text("Group profile is stored on members' devices, not on the servers.")
            }

            Section {
                Button("Reset") {
                    groupProfile = groupInfo.groupProfile
                    shortDescr = groupInfo.groupProfile.shortDescr ?? ""
                    currentProfileHash = groupProfile.hashValue
                }
                .disabled(
                    currentProfileHash == groupProfile.hashValue &&
                    (groupInfo.groupProfile.shortDescr ?? "") == shortDescr.trimmingCharacters(in: .whitespaces)
                )
                Button("Save group profile", action: saveProfile)
                .disabled(!canUpdateProfile)
            }
        }
        .confirmationDialog("Group image", isPresented: $showChooseSource, titleVisibility: .visible) {
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
            Task {
                let resized: String? = if let image {
                    await resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
                } else {
                    nil
                }
                await MainActor.run { groupProfile.image = resized }
            }
        }
        .onAppear {
            currentProfileHash = groupProfile.hashValue
            shortDescr = groupInfo.groupProfile.shortDescr ?? ""
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                withAnimation { focusDisplayName = true }
            }
        }
        .onDisappear {
            if canUpdateProfile {
                showAlert(
                    title: NSLocalizedString("Save group profile?", comment: "alert title"),
                    message: NSLocalizedString("Group profile was changed. If you save it, the updated profile will be sent to group members.", comment: "alert message"),
                    buttonTitle: NSLocalizedString("Save (and notify members)", comment: "alert button"),
                    buttonAction: saveProfile,
                    cancelButton: true
                )
            }
        }
        .alert(item: $alert) { a in
            switch a {
            case let .saveError(err):
                return Alert(
                    title: Text("Error saving group profile"),
                    message: Text(err)
                )
            case let .invalidName(name):
                return createInvalidNameAlert(name, $groupProfile.displayName)
            }
        }
        .navigationBarTitle("Group profile")
        .modifier(ThemedBackground(grouped: true))
        .navigationBarTitleDisplayMode(focusDisplayName ? .inline : .large)
    }

    private var canUpdateProfile: Bool {
        (
            currentProfileHash != groupProfile.hashValue ||
            (groupProfile.shortDescr ?? "") != shortDescr.trimmingCharacters(in: .whitespaces)
        ) &&
        groupProfile.displayName.trimmingCharacters(in: .whitespaces) != "" &&
        validNewProfileName &&
        shortDescrFitsLimit()
    }

    private var validNewProfileName: Bool {
        groupProfile.displayName == groupInfo.groupProfile.displayName
            || validDisplayName(groupProfile.displayName.trimmingCharacters(in: .whitespaces))
    }

    private func shortDescrFitsLimit() -> Bool {
        chatJsonLength(shortDescr) <= MAX_BIO_LENGTH_BYTES
    }

    func saveProfile() {
        Task {
            do {
                groupProfile.displayName = groupProfile.displayName.trimmingCharacters(in: .whitespaces)
                groupProfile.fullName = groupProfile.fullName.trimmingCharacters(in: .whitespaces)
                groupProfile.shortDescr = shortDescr.trimmingCharacters(in: .whitespaces)
                let gInfo = try await apiUpdateGroup(groupInfo.groupId, groupProfile)
                await MainActor.run {
                    currentProfileHash = groupProfile.hashValue
                    groupInfo = gInfo
                    chatModel.updateGroup(gInfo)
                    dismiss()
                }
            } catch let error {
                let err = responseError(error)
                alert = .saveError(err: err)
                logger.error("GroupProfile apiUpdateGroup error: \(err)")
            }
        }
    }
}

struct GroupProfileView_Previews: PreviewProvider {
    static var previews: some View {
        GroupProfileView(
            groupInfo: Binding.constant(GroupInfo.sampleData),
            groupProfile: GroupProfile.sampleData
        )
    }
}
