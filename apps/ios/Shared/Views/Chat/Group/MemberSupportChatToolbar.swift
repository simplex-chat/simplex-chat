//
//  MemberSupportChatToolbar.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 01.05.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct MemberSupportChatToolbar: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var theme: AppTheme
    var groupMember: GroupMember
    var imageSize: CGFloat = 32
    
    var body: some View {
        return HStack {
            MemberProfileImage(groupMember, size: imageSize)
                .padding(.trailing, 4)
            let t = Text(groupMember.chatViewName).font(.headline)
            (groupMember.verified ? memberVerifiedShield + t : t)
                .lineLimit(1)
        }
        .foregroundColor(theme.colors.onBackground)
        .frame(width: 220)
    }

    private var memberVerifiedShield: Text {
        (Text(Image(systemName: "checkmark.shield")) + textSpace)
            .font(.caption)
            .foregroundColor(theme.colors.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }
}

#Preview {
    MemberSupportChatToolbar(
        groupMember: GroupMember.sampleData
    )
    .environmentObject(CurrentColors.toAppTheme())
}
