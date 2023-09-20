//
//  ContextInvitingContactMemberView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 18.09.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ContextInvitingContactMemberView: View {
    @Environment(\.colorScheme) var colorScheme

    var body: some View {
        HStack {
            Image(systemName: "message")
                .foregroundColor(.secondary)
            Text("Send direct message to connect")
        }
        .padding(12)
        .frame(minHeight: 50)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(colorScheme == .light ? sentColorLight : sentColorDark)
        .padding(.top, 8)
    }
}

struct ContextInvitingContactMemberView_Previews: PreviewProvider {
    static var previews: some View {
        ContextInvitingContactMemberView()
    }
}
