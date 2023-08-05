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
                .bold()
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                    Text("Incognito mode protects your privacy by using a new random profile for each contact.")
                    Text("It allows having many anonymous connections without any shared data between them in a single chat profile.")
                    Text("When you share an incognito profile with somebody, this profile will be used for the groups they invite you to.")
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
