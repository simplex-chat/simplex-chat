//
//  CreateProfile.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChatSDK

struct CreateProfile: View {
    @EnvironmentObject var m: ChatModel
    @State private var displayName: String = ""
    @State private var fullName: String = ""
    @FocusState private var focusDisplayName
    @FocusState private var focusFullName

    var body: some View {
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
                    if canCreateProfile() { Task { await createProfile() } }
                    else { focusFullName = true }
                }

            Spacer()

            HStack {
                Button {
                    hideKeyboard()
                    withAnimation { m.onboardingStage = .step1_SimpleXInfo }
                } label: {
                    HStack {
                        Image(systemName: "lessthan")
                        Text("About SimpleX")
                    }
                }

                Spacer()

                HStack {
                    Button {
                        Task { await createProfile() }
                    } label: {
                        Text("Create")
                        Image(systemName: "greaterthan")
                    }
                    .disabled(!canCreateProfile())
                }
            }
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

    func createProfile() async {
        hideKeyboard()
        let profile = Profile(
            displayName: displayName,
            fullName: fullName
        )
        do {
            let user = try await apiCreateActiveUser(profile)
            await MainActor.run { m.currentUser = user }
            await startChat()
            DispatchQueue.main.async {
                withAnimation { m.onboardingStage = .step3_MakeConnection }
            }
        } catch {
            fatalError("Failed to create user: \(error)")
        }
    }

    func hideKeyboard() {
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
    }

    func canCreateProfile() -> Bool {
        displayName != "" && validDisplayName(displayName)
    }
}

func validDisplayName(_ name: String) -> Bool {
    name.firstIndex(of: " ") == nil
}

struct CreateProfile_Previews: PreviewProvider {
    static var previews: some View {
        CreateProfile()
    }
}
