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
    @Binding var openedSheet: NewChatAction?
    @EnvironmentObject var m: ChatModel
    @State private var displayName: String = ""
    @State private var fullName: String = ""
    @FocusState private var focusDisplayName
    @FocusState private var focusFullName

    var body: some View {
        VStack(alignment: .leading) {
            Text("Create group")
                .font(.largeTitle)
                .padding(.bottom, 4)
            ZStack(alignment: .topLeading) {
                if !validDisplayName(displayName) {
                    Image(systemName: "exclamationmark.circle")
                        .foregroundColor(.red)
                        .padding(.top, 4)
                }
                textField("Display name", text: $displayName)
                    .focused($focusDisplayName)
                    .submitLabel(.next)
                    .onSubmit {
                        if canCreateProfile() { focusFullName = true }
                        else { focusDisplayName = true }
                    }
            }
            textField("Full name (optional)", text: $fullName)
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
        }
        .onAppear() {
            focusDisplayName = true
        }
        .padding()
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
        let groupProfile = GroupProfile(
            displayName: displayName,
            fullName: fullName
        )
        do {
            let groupInfo = try apiNewGroup(groupProfile)
            m.addChat(Chat(chatInfo: .group(groupInfo: groupInfo), chatItems: []))
            openedSheet = nil
            DispatchQueue.main.async {
                m.chatId = groupInfo.id
            }
        } catch {
            fatalError("Failed to create group: \(responseError(error))")
        }
    }

    func hideKeyboard() {
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
    }

    func canCreateProfile() -> Bool {
        displayName != "" && validDisplayName(displayName)
    }
}

struct AddGroupView_Previews: PreviewProvider {
    static var previews: some View {
        @State var openedSheet: NewChatAction? = nil
        return AddGroupView(openedSheet: $openedSheet)
    }
}
