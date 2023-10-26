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
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false
    @State private var chat: Chat?
    @State private var groupInfo: GroupInfo?
    @State private var profile = GroupProfile(displayName: "", fullName: "")
    @FocusState private var focusDisplayName
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var showInvalidNameAlert = false
    @State private var groupLink: String?
    @State private var groupLinkMemberRole: GroupMemberRole = .member

    var body: some View {
        if let chat = chat, let groupInfo = groupInfo {
            if !groupInfo.membership.memberIncognito {
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
                GroupLinkView(
                    groupId: groupInfo.groupId,
                    groupLink: $groupLink,
                    groupLinkMemberRole: $groupLinkMemberRole,
                    showTitle: true,
                    creatingGroup: true
                ) {
                    dismiss()
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        m.chatId = groupInfo.id
                    }
                }
            }
        } else {
            createGroupView().keyboardPadding()
        }
    }

    func createGroupView() -> some View {
        List {
            Group {
                Text("Create secret group")
                    .font(.largeTitle)
                    .bold()
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.bottom)
                    .onTapGesture { hideKeyboard() }

                ZStack(alignment: .center) {
                    ZStack(alignment: .topTrailing) {
                        ProfileImage(imageStr: profile.image, color: Color(uiColor: .secondarySystemGroupedBackground))
                            .aspectRatio(1, contentMode: .fit)
                            .frame(maxWidth: 128, maxHeight: 128)
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
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

            Section {
                groupNameTextField()
                Button {
                    createGroup()
                } label: {
                    settingsRow("checkmark") { Text("Create") }
                }
                .disabled(!canCreateProfile())
                IncognitoToggle(incognitoEnabled: $incognitoDefault)
            } footer: {
                Group {
                    sharedGroupProfileInfo(incognitoDefault)
                    + Text(String("\n\n"))
                    + Text("The group is fully decentralized – it is visible only to the members.")
                }
                .onTapGesture { hideKeyboard() }
            }
        }
        .onAppear() {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                focusDisplayName = true
            }
        }
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
    }

    func groupNameTextField() -> some View {
        ZStack(alignment: .leading) {
            let name = profile.displayName.trimmingCharacters(in: .whitespaces)
            if name != mkValidName(name) {
                Button {
                    showInvalidNameAlert = true
                } label: {
                    Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                }
            } else {
                Image(systemName: "pencil").foregroundColor(.secondary)
            }
            textField("Enter group name…", text: $profile.displayName)
                .focused($focusDisplayName)
                .submitLabel(.go)
                .onSubmit {
                    if canCreateProfile() { createGroup() }
                }
        }
    }

    func textField(_ placeholder: LocalizedStringKey, text: Binding<String>) -> some View {
        TextField(placeholder, text: text)
            .padding(.leading, 32)
    }

    func sharedGroupProfileInfo(_ incognito: Bool) -> Text {
        let name = ChatModel.shared.currentUser?.displayName ?? ""
        return Text(
            incognito
            ? "A new random profile will be shared with group members."
            : "Your profile **\(name)** will be shared with group members."
        )
    }

    func createGroup() {
        hideKeyboard()
        do {
            profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
            let gInfo = try apiNewGroup(incognito: incognitoDefault, groupProfile: profile)
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
