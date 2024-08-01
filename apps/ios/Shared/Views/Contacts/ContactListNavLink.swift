//
//  ContactListNavLink.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 01/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactPreview: View {
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme

    var textColor: Color {
        let contactType = chatContactType(chat: chat)
        
        switch contactType {
        case .card, .request:
            return .accentColor
        case .recent:
            if chat.chatInfo.incognito {
                return .indigo
            }
        default:
            return theme.colors.onBackground
        }
        
        return theme.colors.onBackground
    }

    var body: some View {
        VStack(alignment: .leading) {
            Text(chat.chatInfo.chatViewName)
                .lineLimit(1)
                .foregroundColor(textColor)
        }
    }
}

struct ContactListNavLink: View {
    @ObservedObject var chat: Chat
    
    var body: some View {
        ContactPreview(chat: chat)
    }
}
