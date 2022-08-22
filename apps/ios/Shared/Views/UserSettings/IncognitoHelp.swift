//
//  IncognitoHelp.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 22.08.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct IncognitoHelp: View {
    var body: some View {
        VStack(alignment: .leading) {
            Text("Incognito mode")
                .font(.largeTitle)
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading, spacing: 24) {
                    Text("Incognito mode protects the privacy of your main profile name and image — instead, your new random profile is sent to contacts and groups.")
                    Text("It allows having many anonymous connections without any shared data between them in a single chat profile.")
                    Text("To find the profile used for an incognito connection, tap the contact or group name on top of the chat.")
                }
                .padding(.bottom)

                VStack(alignment: .leading, spacing: 8) {
                    Text("Incognito groups")
                        .font(.title2)
                        .padding(.top)
                    Text("When you join a group incognito, your new member profile is created and shared with all members.")
                    Group {
                        Text("Your are incognito in a group when:")
                            .padding(.top)
                        textListItem("•", "the group is created in incognito mode,")
                        textListItem("•", "you or the member who invited you joined in incognito mode,")
                        textListItem("•", "you have an incognito connection with the member who invited you.")
                    }
                    Group {
                        Text("Risks and limitations:")
                            .padding(.top)
                        textListItem("•", "It is not allowed to invite contacts with whom you have an incognito connection to a group where you use your main profile – otherwise they might find out your main profile.")
                        textListItem("•", "There is a risk to have your main profile shared, if you have contacts who know your main profile in a group where you use an incognito profile. Before you invite or join group with such contacts a warning will be shown.")

                    }
                }
                .padding(.bottom)
            }
        }
        .frame(maxWidth: .infinity)
        .padding()
    }
}

struct IncognitoHelp_Previews: PreviewProvider {
    static var previews: some View {
        IncognitoHelp()
    }
}
