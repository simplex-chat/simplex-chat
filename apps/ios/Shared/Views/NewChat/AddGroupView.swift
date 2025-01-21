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
    @EnvironmentObject var theme: AppTheme
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
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        dismissAllSheets(animated: true) {
                            ItemsModel.shared.loadOpenChat(groupInfo.id)
                        }
                    }
                }
                .navigationBarTitleDisplayMode(.inline)
            } else {
                GroupLinkView(
                    groupId: groupInfo.groupId,
                    groupLink: $groupLink,
                    groupLinkMemberRole: $groupLinkMemberRole,
                    showTitle: false,
                    creatingGroup: true
                ) {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        dismissAllSheets(animated: true) {
                            ItemsModel.shared.loadOpenChat(groupInfo.id)
                        }
                    }
                }
                .navigationBarTitle("Group link")
            }
        } else {
            createGroupView()
        }
    }

    func createGroupView() -> some View {
        List {
            Group {
                ZStack(alignment: .center) {
                    ZStack(alignment: .topTrailing) {
                        ProfileImage(imageStr: profile.image, size: 128)
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
                        .buttonStyle(BorderlessButtonStyle()) // otherwise whole "list row" is clickable
                }
                .frame(maxWidth: .infinity, alignment: .center)
            }
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

            Section {
                groupNameTextField()
                Button(action: createGroup) {
                    settingsRow("checkmark", color: theme.colors.primary) { Text("Create group") }
                }
                .disabled(!canCreateProfile())
                IncognitoToggle(incognitoEnabled: $incognitoDefault)
            } footer: {
                VStack(alignment: .leading, spacing: 4) {
                    sharedGroupProfileInfo(incognitoDefault)
                    Text("Fully decentralized – visible only to members.")
                }
                .foregroundColor(theme.colors.secondary)
                .frame(maxWidth: .infinity, alignment: .leading)
                .onTapGesture(perform: hideKeyboard)
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
            LibraryImagePicker(image: $chosenImage) { _ in
                await MainActor.run {
                    showImagePicker = false
                }
            }
        }
        .alert(isPresented: $showInvalidNameAlert) {
            createInvalidNameAlert(mkValidName(profile.displayName), $profile.displayName)
        }
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
        .modifier(ThemedBackground(grouped: true))
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
                Image(systemName: "pencil").foregroundColor(theme.colors.secondary)
            }
            textField("Enter group name…", text: $profile.displayName)
                .focused($focusDisplayName)
                .submitLabel(.continue)
                .onSubmit {
                    if canCreateProfile() { createGroup() }
                }
        }
    }

    func textField(_ placeholder: LocalizedStringKey, text: Binding<String>) -> some View {
        TextField(placeholder, text: text)
            .padding(.leading, 36)
    }

    func sharedGroupProfileInfo(_ incognito: Bool) -> Text {
        let name = ChatModel.shared.currentUser?.displayName ?? ""
        return Text(
            incognito
            ? "A new random profile will be shared."
            : "Your profile **\(name)** will be shared."
        )
    }

    func createGroup() {
        hideKeyboard()
        do {
            profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
            profile.groupPreferences = GroupPreferences(history: GroupPreference(enable: .on))
            let gInfo = try apiNewGroup(incognito: incognitoDefault, groupProfile: profile)
            Task {
                let groupMembers = await apiListMembers(gInfo.groupId)
                await MainActor.run {
                    m.groupMembers = groupMembers.map { GMember.init($0) }
                    m.populateGroupMembersIndexes()
                }
            }
            let c = Chat(chatInfo: .group(groupInfo: gInfo), chatItems: [])
            m.addChat(c)
            withAnimation {
                groupInfo = gInfo
                chat = c
            }
        } catch {
            dismissAllSheets(animated: true) {
                AlertManager.shared.showAlert(
                    Alert(
                        title: Text("Error creating group"),
                        message: Text(responseError(error))
                    )
                )
            }
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
