//
//  GroupProfileView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 29/07/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupProfileView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    var groupId: Int64
    @State var groupProfile: GroupProfile
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var showSaveErrorAlert = false
    @State private var saveGroupError: String? = nil
    @FocusState private var focusDisplayName

    var body: some View {
        return VStack(alignment: .leading) {
            Text("Group profile is stored on members' devices, not on the servers.")
                .padding(.bottom)

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
                ZStack(alignment: .leading) {
                    if !validDisplayName(groupProfile.displayName) {
                        Image(systemName: "exclamationmark.circle")
                            .foregroundColor(.red)
                            .padding(.bottom, 10)
                    }
                    profileNameTextEdit("Group display name", $groupProfile.displayName)
                        .focused($focusDisplayName)
                }
                profileNameTextEdit("Group full name (optional)", $groupProfile.fullName)
                HStack(spacing: 20) {
                    Button("Cancel") { dismiss() }
                    Button("Save group profile") { saveProfile() }
                        .disabled(groupProfile.displayName == "" || !validDisplayName(groupProfile.displayName))
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
            LibraryImagePicker(image: $chosenImage) {
                didSelectItem in showImagePicker = false
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
        .alert(isPresented: $showSaveErrorAlert) {
            Alert(
                title: Text("Error saving group profile"),
                message: Text("\(saveGroupError ?? "Unexpected error")")
            )
        }
        .contentShape(Rectangle())
        .onTapGesture {
            UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
        }
    }

    func profileNameTextEdit(_ label: String, _ name: Binding<String>) -> some View {
        TextField(label, text: name)
            .textInputAutocapitalization(.never)
            .disableAutocorrection(true)
            .padding(.bottom)
            .padding(.leading, 28)
    }

    func saveProfile() {
        Task {
            do {
                let gInfo = try await apiUpdateGroup(groupId, groupProfile)
                await MainActor.run {
                    chatModel.updateGroup(gInfo)
                    dismiss()
                }
            } catch let error {
                let err = responseError(error)
                saveGroupError = err
                showSaveErrorAlert = true
                logger.error("UserProfile apiUpdateProfile error: \(err)")
            }
        }
    }
}

struct GroupProfileView_Previews: PreviewProvider {
    static var previews: some View {
        GroupProfileView(groupId: 1, groupProfile: GroupProfile.sampleData)
    }
}
