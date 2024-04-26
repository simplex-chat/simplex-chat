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
    @EnvironmentObject var chatModel: ChatModel
    var contactRequest: UserContactRequest
    @ObservedObject var chat: Chat

    var body: some View {
        HStack(spacing: 8) {
            ChatInfoImage(chat: chat, size: 63)
                .padding(.leading, 4)
            VStack(alignment: .leading, spacing: 0) {
                HStack(alignment: .top) {
                    Text(contactRequest.chatViewName)
                        .font(.title3)
                        .fontWeight(.bold)
                        .foregroundColor(.accentColor)
                        .padding(.leading, 8)
                        .frame(alignment: .topLeading)
                    Spacer()
                    formatTimestampText(contactRequest.updatedAt)
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.top, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
                .padding(.bottom, 2)

                Text("wants to connect to you!")
                    .frame(alignment: .topLeading)
                    .padding([.leading, .trailing], 8)
                
                Spacer()
            }
            .frame(maxHeight: .infinity)
        }
    }
}

struct ContactRequestView_Previews: PreviewProvider {
    static var previews: some View {
        ContactRequestView(contactRequest: UserContactRequest.sampleData, chat: Chat(chatInfo: ChatInfo.sampleData.contactRequest , chatItems: []))
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
