//
//  ContactRequestView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 02/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ContactRequestView: View {
    var contactRequest: UserContactRequest

    var body: some View {
        return HStack(spacing: 8) {
            Image(systemName: "person.crop.circle.fill")
                .resizable()
                .foregroundColor(Color(uiColor: .secondarySystemBackground))
                .frame(width: 63, height: 63)
                .padding(.leading, 4)
            VStack(alignment: .leading, spacing: 4) {
                HStack(alignment: .top) {
                    Text(ChatInfo.contactRequest(contactRequest: contactRequest).chatViewName)
                        .font(.title3)
                        .fontWeight(.bold)
                        .foregroundColor(.blue)
                        .padding(.leading, 8)
                        .padding(.top, 4)
                        .frame(maxHeight: .infinity, alignment: .topLeading)
                    Spacer()
                    timestampText(contactRequest.createdAt)
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
        ContactRequestView(contactRequest: UserContactRequest.sampleData)
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
