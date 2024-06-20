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
                .foregroundColor(.secondary)
            Text("Send direct message to connect")
        }
        .padding(12)
        .frame(minHeight: 50)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(theme.appColors.sentMessage)
        .padding(.top, 8)
    }
}

struct ContextInvitingContactMemberView_Previews: PreviewProvider {
    static var previews: some View {
        ContextInvitingContactMemberView()
    }
}
