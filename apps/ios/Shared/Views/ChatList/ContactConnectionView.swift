//
//  ContactConnectionView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactConnectionView: View {
    var contactConnection: PendingContactConnection
    
    var body: some View {
        HStack(spacing: 8) {
            Image(systemName: contactConnection.initiated ? "link.badge.plus" : "link")
                .resizable()
                .foregroundColor(Color(uiColor: .secondarySystemBackground))
                .scaledToFill()
                .frame(width: 48, height: 48)
                .frame(width: 63, height: 63)
                .padding(.leading, 4)
            VStack(alignment: .leading, spacing: 0) {
                HStack(alignment: .top) {
                    Text(contactConnection.chatViewName)
                        .font(.title3)
                        .fontWeight(.bold)
                        .foregroundColor(.secondary)
                        .padding(.leading, 8)
                        .frame(alignment: .topLeading)
                    Spacer()
                    formatTimestampText(contactConnection.updatedAt)
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.top, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
                .padding(.bottom, 2)

                Text(contactConnection.description)
                    .frame(alignment: .topLeading)
                    .padding([.leading, .trailing], 8)

                Spacer()
            }
            .frame(maxHeight: .infinity)
        }
    }
}

struct ContactConnectionView_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionView(contactConnection: PendingContactConnection.getSampleData())
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
