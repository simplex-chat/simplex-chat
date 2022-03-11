//
//  WelcomeView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 18/01/2022.
//

import SwiftUI

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
                Text("The messaging and application platform 100% private by design!")
                    .padding(.bottom, 8)
                Text("Your profile, contacts or messages (once delivered) are only stored locally on your device.")
                    .padding(.bottom, 8)
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
                .disabled(!validDisplayName(displayName) || displayName == "")
            }
        }
        .padding()
    }

    func validDisplayName(_ name: String) -> Bool {
        name.firstIndex(of: " ") == nil
    }
}

struct WelcomeView_Previews: PreviewProvider {
    static var previews: some View {
        WelcomeView()
    }
}
