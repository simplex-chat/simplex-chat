//
//  ChangeDBPasswordView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 04/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChangeDBPasswordView: View {
    @EnvironmentObject private var m: ChatModel
    @State private var dbKey = ""

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text(m.chatDbKey == "" ? "Set database password" : "Change database password")
                .font(.title)
            TextField("Enter database password", text: $dbKey)
                .disableAutocorrection(true)
                .autocapitalization(.none)
            HStack {
                Button("Save") {
                }
                if m.chatDbKey != "" {
                    Button("Remove") {
                    }
                }
            }
            Spacer()
        }
        .padding()
    }
}

struct ChangeDBPasswordView_Previews: PreviewProvider {
    static var previews: some View {
        ChangeDBPasswordView()
    }
}
