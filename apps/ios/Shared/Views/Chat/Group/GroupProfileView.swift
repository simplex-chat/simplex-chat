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
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var alert: GroupProfileAlert?
    @FocusState private var focusDisplayName

    var body: some View {
        return VStack(alignment: .leading) {
            Text("Group profile is stored on members' devices, not on the servers.")
                .padding(.vertical)

            ZStack(alignment: .center) {
                ZStack(alignment: .topTrailing) {
                    profileImageView(groupProfile.image)
                    if groupProfile.image != nil {
                        Button {
                            groupProfile.image = nil
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
                ZStack(alignment: .topLeading) {
                    if !validNewProfileName() {
                        Button {
                            alert = .invalidName(validName: mkValidName(groupProfile.displayName))
                        } label: {
                            Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                        }
                    } else {
                        Image(systemName: "exclamationmark.circle").foregroundColor(.clear)
                    }
                    profileNameTextEdit("Group display name", $groupProfile.displayName)
                        .focused($focusDisplayName)
                }
                .padding(.bottom)
                let fullName = groupInfo.groupProfile.fullName
                if fullName != "" && fullName != groupProfile.displayName {
                    profileNameTextEdit("Group full name (optional)", $groupProfile.fullName)
                        .padding(.bottom)
                }
                HStack(spacing: 20) {
                    Button("Cancel") { dismiss() }
                    Button("Save group profile") { saveProfile() }
                        .disabled(!canUpdateProfile())
                }
            }
            .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)

        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
        .confirmationDialog("Group image", isPresented: $showChooseSource, titleVisibility: .visible) {
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
            if let image = image {
                groupProfile.image = resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
            } else {
                groupProfile.image = nil
            }
        }
        .onAppear {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                focusDisplayName = true
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
        .contentShape(Rectangle())
        .onTapGesture { hideKeyboard() }
    }

    private func canUpdateProfile() -> Bool {
        groupProfile.displayName.trimmingCharacters(in: .whitespaces) != "" && validNewProfileName()
    }

    private func validNewProfileName() -> Bool {
        groupProfile.displayName == groupInfo.groupProfile.displayName
            || validDisplayName(groupProfile.displayName.trimmingCharacters(in: .whitespaces))
    }

    func profileNameTextEdit(_ label: LocalizedStringKey, _ name: Binding<String>) -> some View {
        TextField(label, text: name)
            .padding(.leading, 32)
    }

    func saveProfile() {
        Task {
            do {
                groupProfile.displayName = groupProfile.displayName.trimmingCharacters(in: .whitespaces)
                let gInfo = try await apiUpdateGroup(groupInfo.groupId, groupProfile)
                await MainActor.run {
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
        GroupProfileView(groupInfo: Binding.constant(GroupInfo.sampleData), groupProfile: GroupProfile.sampleData)
    }
}
