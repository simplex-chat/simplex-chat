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
                    Text("Incognito mode allows you to connect to new contacts and join groups without sharing your main profile — a new random profile is generated and sent to a person or group you're connecting to.")
                    Text("This allows you to have incognito conversations where you don't share your main profile without fully switching context.")
                    Text("The profile name used for an incognito conversation can be found in chat information page — tap the contact or group name.")
                }
                .padding(.bottom)

                VStack(alignment: .leading, spacing: 8) {
                    Text("Incognito groups")
                        .font(.title2)
                        .padding(.top)
                    Group {
                        Text("When you join incognito:")
                            .padding(.top)
                        textListItem("•", "If the group member who invites you is incognito in group.")
                        textListItem("•", "If you're connected incognito to the group member who invites you.")
                        textListItem("•", "If you have Incognito mode enabled.")
                    }
                    Group {
                        Text("What actions are not allowed:")
                            .padding(.top)
                        textListItem("•", "Inviting contacts with whom you've shared an incognito profile to a group where you use your main profile.")
                    }
                    Group {
                        Text("What actions show warnings:")
                            .padding(.top)
                        textListItem("•", "Inviting contacts with whom you've shared your main profile to a group where you use an incognito profile.")
                        textListItem("•", "Accepting a group invitation incognito from a contact with whom you've shared your main profile.")
                        textListItem("•", "In both above cases if your contact uses SimpleX app older than v3.2 or some other client, they may share your main profile instead of a random incognito profile with other members.")
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
