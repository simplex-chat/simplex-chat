//
//  ContactConnectionView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ContactConnectionView: View {
    var contactConnection: PendingContactConnection
    
    var body: some View {
        let initiated = contactConnection.pccConnStatus.initiated ?? false
        HStack(spacing: 8) {
            Image(systemName: initiated ? "link.badge.plus" : "link")
                .resizable()
                .foregroundColor(Color(uiColor: .secondarySystemBackground))
                .scaledToFill()
                .frame(width: 48, height: 48)
                .frame(width: 63, height: 63)
                .padding(.leading, 4)
            VStack(alignment: .leading, spacing: 4) {
                HStack(alignment: .top) {
                    Text(contactConnection.chatViewName)
                        .font(.title3)
                        .fontWeight(.bold)
                        .foregroundColor(.secondary)
                        .padding(.leading, 8)
                        .padding(.top, 4)
                        .frame(maxHeight: .infinity, alignment: .topLeading)
                    Spacer()
                    timestampText(contactConnection.updatedAt)
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.top, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
                Text(contactConnection.description)
                    .frame(minHeight: 44, maxHeight: 44, alignment: .topLeading)
                    .padding([.leading, .trailing], 8)
                    .padding(.bottom, 4)
                    .padding(.top, 1)
            }
        }
    }
}

struct ContactConnectionView_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionView(contactConnection: PendingContactConnection.getSampleData())
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
