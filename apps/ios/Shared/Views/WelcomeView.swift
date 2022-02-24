//
//  WelcomeView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 18/01/2022.
//

import SwiftUI

func displayNameError(_ name: String) -> String {
    self.rangeOfCharacter(from: .whitespacesAndNewlines) == nil
        ? ""
        : "Display name cannot contain whitespace."
}

struct WelcomeView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var displayName: String = ""
    @State var fullName: String = ""

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .leading) {
                Image("logo")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: g.size.width * 0.7)
                    .padding(.vertical)
                Text("You control your chat!")
                    .font(.title)
                    .padding(.bottom)
                Text("The messaging and application platform protecting your privacy and security.")
                    .padding(.bottom, 8)
                Text("We don't store any of your contacts or messages (once delivered) on the servers.")
                    .padding(.bottom, 24)
                Text("Create profile")
                    .font(.largeTitle)
                    .padding(.bottom)
                Text("Your profile is stored on your device and shared only with your contacts.")
                    .padding(.bottom)
                TextField("Display name", text: $displayName)
                    .textInputAutocapitalization(.never)
                    .disableAutocorrection(true)
                    .padding(.bottom)
                Text(displayNameError(displayName))
                    .foregroundColor(.red)
                TextField("Full name (optional)", text: $fullName)
                    .textInputAutocapitalization(.never)
                    .disableAutocorrection(true)
                    .padding(.bottom)
                Button("Create") {
                    let profile = Profile(
                        displayName: displayName,
                        fullName: fullName
                    )
                    do {
                        let user = try apiCreateActiveUser(profile)
                        chatModel.currentUser = user
                    } catch {
                        fatalError("Failed to create user: \(error)")
                    }
                }
            }
        }
        .padding()
    }
}

struct WelcomeView_Previews: PreviewProvider {
    static var previews: some View {
        WelcomeView()
    }
}
