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
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var showInvalidNameAlert = false

    var body: some View {
        if let chat = chat, let groupInfo = groupInfo {
            AddGroupMembersViewCommon(
                chat: chat,
                groupInfo: groupInfo,
                creatingGroup: true,
                showFooterCounter: false
            ) { _ in
                dismiss()
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                    m.chatId = groupInfo.id
                }
            }
        } else {
            createGroupView().keyboardPadding()
        }
    }

    func createGroupView() -> some View {
        VStack(alignment: .leading) {
            Text("Create secret group")
                .font(.largeTitle)
                .padding(.vertical, 4)
            Text("The group is fully decentralized – it is visible only to the members.")
                .padding(.bottom, 4)

            HStack {
                Image(systemName: "info.circle").foregroundColor(.secondary).font(.footnote)
                Spacer().frame(width: 8)
                Text("Your chat profile will be sent to group members").font(.footnote)
            }
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
                let name = profile.displayName.trimmingCharacters(in: .whitespaces)
                if name != mkValidName(name) {
                    Button {
                        showInvalidNameAlert = true
                    } label: {
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                    }
                } else {
                    Image(systemName: "exclamationmark.circle").foregroundColor(.clear)
                }
                textField("Enter group name…", text: $profile.displayName)
                    .focused($focusDisplayName)
                    .submitLabel(.go)
                    .onSubmit {
                        if canCreateProfile() { createGroup() }
                    }
            }
            .padding(.bottom)

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
        .alert(isPresented: $showInvalidNameAlert) {
            createInvalidNameAlert(mkValidName(profile.displayName), $profile.displayName)
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
            .padding(.leading, 32)
    }

    func createGroup() {
        hideKeyboard()
        do {
            profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
            let gInfo = try apiNewGroup(profile)
            Task {
                let groupMembers = await apiListMembers(gInfo.groupId)
                await MainActor.run {
                    m.groupMembers = groupMembers.map { GMember.init($0) }
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
        let name = profile.displayName.trimmingCharacters(in: .whitespaces)
        return name != "" && validDisplayName(name)
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
