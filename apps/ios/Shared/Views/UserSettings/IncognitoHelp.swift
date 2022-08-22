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
                        Text("Turning Incognito mode on allows you to connect to new contacts and join groups without sharing your main profile. When incognito mode is enabled, for each new conversation a new random profile is generated and sent to a person or group you're connecting to.")
                        Text("When accepting contact requests received via a contact address, you can decide to connect using a random profile by enabling Incognito mode, even if you've created the address itself outside of Incognito mode.")
                        Text("To learn which profile is used in an incognito conversation, open chat information page — for direct contacts this profile is available under \"Your random profile\", and for groups it's your group membership profile.")
                    }
                    .padding(.bottom)

                    Group {
                        Text("Groups")
                            .font(.title2)
                            .padding(.top)
                        Text("To minimize user confusion, we've made several decisions regarding how groups operate in Incognito mode:")
                        textListItem("•", "When you create a group in Incognito mode or join incognito, the same random profile is shared with all group members;")
                        textListItem("•", "If your membership in a group is not incognito (i.e. with your main profile), inviting contacts with whom you've connected incognito to this group is prohibited to avoid the possibility of other members sharing or mentioning your main profile to the new member - such contacts will be disabled when inviting group members;")
                        textListItem("•", "If a contact who invites you to a group uses a random profile for this group, you will also join the group using a new random profile. This implies that if a group's creator used a random profile, the whole group will be incognito. You will also join group incognito if you've connected incognito to the contact who invites you, or if you enable Incognito mode before joining.")
                        Text("Keep in mind that connecting incognito with a group member who knows your main profile implies their agreement to use your new random profile for this group. This means that if a contact who invites you to a group knows your main profile (i.e. you've connected with this contact outside of Incognito mode), it is not possible to enforce for this contact to not share your main profile to the rest of the group.")
                        Text("If such contact uses a client of older version (lower than 3.2) or uses a malicious client, they may not respect your incognito membership and share your main profile with other members. Same goes for inviting contacts with whom you've connected outside of Incognito mode; contacts with whom you've connected incognito and are \"safe\" to invite without the possibility of them leaking your main profile will be marked with an Incognito mode icon when adding new members.")
                        Text("You will see an alert each time you accept a group invitation from or send a group invitation to a contact with whom you've connected non incognito, and can ask your contact to check their client and its version and decide whether to proceed based on your trust for this contact.")
                    }
                    .padding(.bottom)
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding()
    }
}

struct IncognitoHelp_Previews: PreviewProvider {
    static var previews: some View {
        IncognitoHelp()
    }
}
