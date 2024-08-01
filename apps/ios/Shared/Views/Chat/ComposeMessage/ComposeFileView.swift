//
//  ComposeFileView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 04.05.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ComposeFileView: View {
    @EnvironmentObject var theme: AppTheme
    let fileName: String
    let cancelFile: (() -> Void)
    let cancelEnabled: Bool

    var body: some View {
        HStack(alignment: .center, spacing: 4) {
            Image(systemName: "doc.fill")
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 30, height: 30)
                .foregroundColor(Color(uiColor: .tertiaryLabel))
                .padding(.leading, 4)
            Text(fileName)
            Spacer()
            if cancelEnabled {
                Button { cancelFile() } label: {
                    Image(systemName: "multiply")
                }
            }
        }
        .padding(.vertical, 1)
        .padding(.trailing, 12)
        .frame(height: 54)
        .background(theme.appColors.sentMessage)
        .frame(maxWidth: .infinity)
    }
}
