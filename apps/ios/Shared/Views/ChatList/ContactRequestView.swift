//
//  ContactRequestView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 02/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactRequestView: View {
    var contactRequest: UserContactRequest
    @ObservedObject var chat: Chat

    var body: some View {
        return HStack(spacing: 8) {
            ChatInfoImage(chat: chat)
                .frame(width: 63, height: 63)
            VStack(alignment: .leading, spacing: 4) {
                HStack(alignment: .top) {
                    Text(contactRequest.chatViewName)
                        .font(.title3)
                        .fontWeight(.bold)
                        .foregroundColor(.blue)
                        .padding(.leading, 8)
                        .padding(.top, 4)
                        .frame(maxHeight: .infinity, alignment: .topLeading)
                    Spacer()
                    formatTimestampText(contactRequest.updatedAt)
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.top, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
                Text("wants to connect to you!")
                    .frame(minHeight: 44, maxHeight: 44, alignment: .topLeading)
                    .padding([.leading, .trailing], 8)
                    .padding(.bottom, 4)
                    .padding(.top, 1)
            }
        }
    }
}

struct ContactRequestView_Previews: PreviewProvider {
    static var previews: some View {
        ContactRequestView(contactRequest: UserContactRequest.sampleData, chat: Chat(chatInfo: ChatInfo.sampleData.contactRequest , chatItems: []))
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
