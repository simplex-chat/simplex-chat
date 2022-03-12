//
//  UserProfile.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct UserProfile: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var profile = Profile(displayName: "", fullName: "")
    @State private var editProfile: Bool = false

    var body: some View {
        let user: User = chatModel.currentUser!
        let color = Color(uiColor: .tertiarySystemGroupedBackground)

        return VStack(alignment: .leading) {
            Text("Your profile is stored on your device and shared only with your contacts.\nSimpleX servers cannot see your profile.")
                .padding(.bottom)
            
            ZStack(alignment: .center) {
                Image(systemName: "person.crop.circle.fill")
                    .resizable()
                    .foregroundColor(color)
                    .frame(width: 192, height: 192)
                // if editing or no_image {
                Button {
                    // add or replace image
                } label: {
                    Image(systemName: "camera")
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(width: 48)
                }
                // }
            }
            .frame(maxWidth: .infinity, alignment: .center)
            
            if editProfile {
                VStack(alignment: .leading) {
                    TextField("Display name", text: $profile.displayName)
                        .textInputAutocapitalization(.never)
                        .disableAutocorrection(true)
                        .padding(.bottom)
                    TextField("Full name (optional)", text: $profile.fullName)
                        .textInputAutocapitalization(.never)
                        .disableAutocorrection(true)
                        .padding(.bottom)
                    HStack(spacing: 20) {
                        Button("Cancel") { editProfile = false }
                        Button("Save (and notify contacts)") { saveProfile() }
                    }
                }
                .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
            } else {
                VStack(alignment: .leading) {
                    HStack {
                        Text("Display name:")
                        Text(user.profile.displayName)
                            .fontWeight(.bold)
                    }
                    .padding(.bottom)
                    HStack {
                        Text("Full name:")
                        Text(user.profile.fullName)
                            .fontWeight(.bold)
                    }
                    .padding(.bottom)
                    Button("Edit") {
                        profile = user.profile
                        editProfile = true
                    }
                }
                .frame(maxWidth: .infinity, minHeight: 120, alignment: .leading)
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
    }

    func saveProfile() {
        Task {
            do {
                if let newProfile = try await apiUpdateProfile(profile: profile) {
                    DispatchQueue.main.async {
                        chatModel.currentUser?.profile = newProfile
                        profile = newProfile
                    }
                }
            } catch {
                logger.error("UserProfile apiUpdateProfile error: \(error.localizedDescription)")
            }
            editProfile = false
        }
    }
}

struct UserProfile_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = User.sampleData
        return UserProfile()
            .environmentObject(chatModel)
    }
}
