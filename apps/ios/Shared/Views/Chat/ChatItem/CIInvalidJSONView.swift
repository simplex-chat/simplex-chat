//
//  CIInvalidJSONView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 29.12.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIInvalidJSONView: View {
    @EnvironmentObject var theme: AppTheme
    var json: Data?
    @State private var showJSON = false
    
    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            Text("invalid data")
                .foregroundColor(.red)
                .italic()
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .textSelection(.disabled)
        .simultaneousGesture(TapGesture().onEnded { showJSON = true })
        .appSheet(isPresented: $showJSON) {
            invalidJSONView(dataToString(json))
        }
    }
}

func invalidJSONView(_ json: String) -> some View {
    VStack(alignment: .leading, spacing: 16) {
        Button { // this is used in the sheet, Button works here
            showShareSheet(items: [json])
        } label: {
            Image(systemName: "square.and.arrow.up")
        }
        .frame(maxWidth: .infinity, alignment: .trailing)
        ScrollView {
            Text(json)
        }
    }
    .frame(maxHeight: .infinity)
    .padding()
    .modifier(ThemedBackground())
}

struct CIInvalidJSONView_Previews: PreviewProvider {
    static var previews: some View {
        CIInvalidJSONView(json: "{}".data(using: .utf8)!)
    }
}
