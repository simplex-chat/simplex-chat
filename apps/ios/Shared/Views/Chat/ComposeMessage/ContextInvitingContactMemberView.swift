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
            Text("Send an invitation to join directly")
        }
        .padding(12)
        .frame(minHeight: 50)
        .frame(maxWidth: .infinity)
        .background(colorScheme == .light ? sentColorLight : sentColorDark)
        .padding(.top, 8)
    }
}

struct ContextInvitingContactMemberView_Previews: PreviewProvider {
    static var previews: some View {
        ContextInvitingContactMemberView()
    }
}
