//
//  AddGroupView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 13.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct AddGroupView: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var chat: Chat?
    @State private var groupInfo: GroupInfo?
    @State private var profile = GroupProfile(displayName: "", fullName: "")
    @FocusState private var focusDisplayName
    @FocusState private var focusFullName
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil

    var body: some View {
        if let chat = chat, let groupInfo = groupInfo {
            AddGroupMembersView(chat: chat,
                                groupInfo: groupInfo,
                                showSkip: true) { _ in
                dismiss()
                DispatchQueue.main.async {
                    m.chatId = groupInfo.id
                }
            }
        } else {
            createGroupView()
        }
    }

    func createGroupView() -> some View {
        VStack(alignment: .leading) {
            Text("Create secret group")
                .font(.largeTitle)
                .padding(.vertical, 4)
            Text("The group is fully decentralized – it is visible only to the members.")
                .padding(.bottom, 4)
                .padding(.bottom)

            ZStack(alignment: .center) {
                ZStack(alignment: .topTrailing) {
                    profileImageView(profile.image)
                    if profile.image != nil {
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
            .padding(.bottom, 4)

            ZStack(alignment: .topLeading) {
                if !validDisplayName(profile.displayName) {
                    Image(systemName: "exclamationmark.circle")
                        .foregroundColor(.red)
                        .padding(.top, 4)
                }
                textField("Group display name", text: $profile.displayName)
                    .focused($focusDisplayName)
                    .submitLabel(.next)
                    .onSubmit {
                        if canCreateProfile() { focusFullName = true }
                        else { focusDisplayName = true }
                    }
            }
            textField("Group full name (optional)", text: $profile.fullName)
                .focused($focusFullName)
                .submitLabel(.go)
                .onSubmit {
                    if canCreateProfile() { createGroup() }
                    else { focusFullName = true }
                }

            Spacer()

            Button {
                createGroup()
            } label: {
                Text("Create")
                Image(systemName: "greaterthan")
            }
            .disabled(!canCreateProfile())
            .frame(maxWidth: .infinity, alignment: .trailing)
        }
        .onAppear() {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                focusDisplayName = true
            }
        }
        .padding()
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
                profile.image = resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)
            } else {
                profile.image = nil
            }
        }
        .contentShape(Rectangle())
        .onTapGesture { hideKeyboard() }
    }

    func textField(_ placeholder: LocalizedStringKey, text: Binding<String>) -> some View {
        TextField(placeholder, text: text)
            .textInputAutocapitalization(.never)
            .disableAutocorrection(true)
            .padding(.leading, 28)
            .padding(.bottom)
    }

    func createGroup() {
        hideKeyboard()
        do {
            let gInfo = try apiNewGroup(profile)
            Task {
                let groupMembers = await apiListMembers(gInfo.groupId)
                await MainActor.run {
                    ChatModel.shared.groupMembers = groupMembers
                }
            }
            let c = Chat(chatInfo: .group(groupInfo: gInfo), chatItems: [])
            m.addChat(c)
            withAnimation {
                groupInfo = gInfo
                chat = c
            }
        } catch {
            dismiss()
            AlertManager.shared.showAlert(
                Alert(
                    title: Text("Error creating group"),
                    message: Text(responseError(error))
                )
            )
        }
    }

    func canCreateProfile() -> Bool {
        profile.displayName != "" && validDisplayName(profile.displayName)
    }
}

func hideKeyboard() {
    UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
}

struct AddGroupView_Previews: PreviewProvider {
    static var previews: some View {
        AddGroupView()
    }
}
