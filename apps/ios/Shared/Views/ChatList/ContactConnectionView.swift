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
            ZStack(alignment: .bottomTrailing) {
                Image(systemName: contactConnection.initiated ? "link.badge.plus" : "link")
                    .resizable()
                    .foregroundColor(Color(uiColor: .secondarySystemBackground))
                    .scaledToFill()
                    .frame(width: 48, height: 48)
                    .frame(width: 63, height: 63)
                    .padding(.leading, 4)
                incognitoIcon()
                    .padding([.bottom, .trailing], 1)
            }
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
                    formatTimestampText(contactConnection.updatedAt)
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

    @ViewBuilder private func incognitoIcon() -> some View {
        if contactConnection.incognito {
            Image(systemName: "theatermasks.circle.fill")
                .foregroundColor(.indigo)
                .background(Circle().foregroundColor(Color(uiColor: .systemBackground)))
        } else {
            EmptyView()
        }
    }
}

struct ContactConnectionView_Previews: PreviewProvider {
    static var previews: some View {
        ContactConnectionView(contactConnection: PendingContactConnection.getSampleData())
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
