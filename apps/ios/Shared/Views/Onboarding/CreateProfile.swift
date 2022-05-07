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
    @State var displayName: String = ""
    @State var fullName: String = ""

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .leading) {
                Image("logo")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: g.size.width * 0.7)
                    .padding(.bottom)
                Text("Create profile")
                    .font(.largeTitle)
                    .padding(.bottom, 4)
                Text("(shared only with your contacts)")
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
                        .padding(.leading, 28)
                        .padding(.bottom, 2)
                }
                .padding(.bottom)
                TextField("Full name (optional)", text: $fullName)
                    .textInputAutocapitalization(.never)
                    .disableAutocorrection(true)
                    .padding(.leading, 28)
                    .padding(.bottom, 48)

                HStack {
                    Button {
                        m.onboardingStep = .step1_SimpleXInfo
                    } label: {
                        Image(systemName: "lessthan")
                        Text("Back")
                            .font(.title2)
                    }

                    Spacer()

                    Button {
                        let profile = Profile(
                            displayName: displayName,
                            fullName: fullName
                        )
                        do {
                            m.currentUser = try apiCreateActiveUser(profile)
                            if m.appOpenUrl == nil {
                                m.onboardingStep = .step3a_MakeConnection
                            } else {
                                m.onboardingStep = .step3b_ConnectViaLink
                            }

                        } catch {
                            fatalError("Failed to create user: \(error)")
                        }
                    } label: {
                        Text("Create")
                            .font(.title2)
                        Image(systemName: "greaterthan")
                    }
                    .disabled(!validDisplayName(displayName) || displayName == "")
                }
            }
            .padding()
        }
    }

    func validDisplayName(_ name: String) -> Bool {
        name.firstIndex(of: " ") == nil
    }
}

struct CreateProfile_Previews: PreviewProvider {
    static var previews: some View {
        CreateProfile()
    }
}
