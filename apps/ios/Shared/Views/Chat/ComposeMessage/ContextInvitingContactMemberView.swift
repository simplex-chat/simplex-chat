//
//  ContextInvitingContactMemberView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 18.09.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ContextInvitingContactMemberView: View {
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        HStack {
            Image(systemName: "message")
                .foregroundColor(theme.colors.secondary)
            Text("Send direct message to connect")
        }
        .padding(12)
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(theme.appColors.sentMessage)
    }
}

struct ContextInvitingContactMemberView_Previews: PreviewProvider {
    static var previews: some View {
        ContextInvitingContactMemberView()
    }
}
