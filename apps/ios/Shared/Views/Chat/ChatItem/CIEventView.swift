//
//  CIEventView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 20.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIEventView: View {
    var eventText: Text

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            eventText
        }
        .padding(.leading, 6)
        .padding(.bottom, 6)
        .textSelection(.disabled)
    }
}

struct CIEventView_Previews: PreviewProvider {
    static var previews: some View {
        CIEventView(eventText: Text("event happened"))
    }
}
