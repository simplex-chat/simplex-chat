//
//  CreateProfile.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct CreateProfile: View {
    @EnvironmentObject var m: ChatModel
    @State private var displayName: String = ""
    @State private var fullName: String = ""
    @FocusState private var focusDisplayName
    @FocusState private var focusFullName

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .leading) {
                Text("Create your profile")
                    .font(.largeTitle)
                    .padding(.bottom, 4)
                Text("Your profile, contacts and delivered messages are stored on your device.")
                    .padding(.bottom, 4)
                Text("The profile is only shared with your contacts.")
                    .padding(.bottom)
                ZStack(alignment: .topLeading) {
                    if !validDisplayName(displayName) {
                        Image(systemName: "exclamationmark.circle")
                            .foregroundColor(.red)
                            .padding(.top, 4)
                    }
                    TextField("Display name", text: $displayName)
                        .textInputAutocapitalization(.never)
                        .disableAutocorrection(true)
                        .focused($focusDisplayName)
                        .padding(.leading, 28)
                        .padding(.bottom, 2)
                        .submitLabel(.next)
                        .onSubmit {
                            if canCreateProfile() { focusFullName = true }
                            else { focusDisplayName = true }
                        }
                        .toolbar {
                            ToolbarItem(placement: .keyboard) {
                                Button("Create") { createProfile() }
                                .disabled(!canCreateProfile())
                            }
                        }
                }
                .padding(.bottom)
                TextField("Full name (optional)", text: $fullName)
                    .textInputAutocapitalization(.never)
                    .disableAutocorrection(true)
                    .focused($focusFullName)
                    .padding(.leading, 28)
                    .padding(.bottom)
                    .submitLabel(.go)
                    .onSubmit {
                        if canCreateProfile() { createProfile() }
                        else { focusFullName = true }
                    }
                    .toolbar {
                        ToolbarItem(placement: .keyboard) {
                            Button("Create") { createProfile() }
                            .disabled(!canCreateProfile())
                        }
                    }

                VStack(alignment: .leading) {
                    Button("Create") { createProfile() }
                    .disabled(!canCreateProfile())
                    .padding(.bottom)

                    Spacer()

                    Button {
                        hideKeyboard()
                        withAnimation {
                            m.onboardingStep = .step1_SimpleXInfo
                        }
                    } label: {
                        Image(systemName: "lessthan")
                        Text("About SimpleX")
                    }
                }
                .padding(.bottom, 8)
            }
            .onAppear() {
                focusDisplayName = true
            }
        }
        .padding()
    }

    func createProfile() {
        hideKeyboard()
        let profile = Profile(
            displayName: displayName,
            fullName: fullName
        )
        do {
            m.currentUser = try apiCreateActiveUser(profile)
            startChat()
            withAnimation {
                m.onboardingStep = m.appOpenUrl == nil
                                    ? .step3a_MakeConnection
                                    : .step3b_ConnectViaLink
            }

        } catch {
            fatalError("Failed to create user: \(error)")
        }
    }

    func hideKeyboard() {
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
    }

    func validDisplayName(_ name: String) -> Bool {
        name.firstIndex(of: " ") == nil
    }

    func canCreateProfile() -> Bool {
        validDisplayName(displayName) && displayName != ""
    }
}

struct CreateProfile_Previews: PreviewProvider {
    static var previews: some View {
        CreateProfile()
    }
}
