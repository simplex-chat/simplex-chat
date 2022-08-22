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
        VStack(alignment: .leading, spacing: 8) {
            Text("Incognito mode")
                .font(.largeTitle)
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                        Text("Incognito mode allows you to connect to new contacts and join groups without sharing your main profile — a new random profile is generated and sent to a person or group you're connecting to.")
                        Text("The profile name used for an incognito conversation can be found in chat information page — tap the contact or group name.")
                    }
                    .padding(.bottom)

                    Group {
                        Text("Incognito groups")
                            .font(.title2)
                            .padding(.top)
                        textListItem("•", "When you create or join a group incognito, the same random profile is shared with all group members.")
                        textListItem("•", "If you joined the group with your main profile, you will not be able to invite the contacts with whom you've shared an incognito profile — to avoid sharing your main profile with them.")
                        textListItem("•", "If a contact who invites you to a group uses a random profile for this group, you will also join the group using a new random profile. This implies that if a group's creator used a random profile, the whole group will be incognito. You will also join group incognito if you've connected incognito to the contact who invites you, or if you enable Incognito mode before joining.")
                        Text("Keep in mind that connecting incognito with a group member who knows your main profile implies their agreement to use your new random profile for this group. This means that if a contact who invites you to a group knows your main profile (i.e. you've connected with this contact outside of Incognito mode), it is not possible to enforce for this contact to not share your main profile to the rest of the group.")
                        Text("If such contact uses a client of older version (lower than 3.2) or uses a malicious client, they may not respect your incognito membership and share your main profile with other members. Same goes for inviting contacts with whom you've connected outside of Incognito mode; contacts with whom you've connected incognito and are \"safe\" to invite without the possibility of them leaking your main profile will be marked with an Incognito mode icon when adding new members.")
                        Text("You will see an alert each time you accept a group invitation from or send a group invitation to a contact with whom you've connected non incognito, and can ask your contact to check their client and its version and decide whether to proceed based on your trust for this contact.")
                    }
                    .padding(.bottom)
                }
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
