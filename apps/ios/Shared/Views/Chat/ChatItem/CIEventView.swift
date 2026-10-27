//
//  CIEventView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 20.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/chat-view.md

import SwiftUI
import SimpleXChat

// Spec: spec/client/chat-view.md#CIEventView
struct CIEventView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.terminalLayout) private var terminal
    var eventText: Text

    var body: some View {
        eventText
        .if(terminal) { $0.modifier(TerminalLineBox(UIFont.preferredFont(forTextStyle: .caption1), theme.colors.secondary, theme)) }
        .padding(terminal ? terminalEventPadding : EdgeInsets(top: 4, leading: 6, bottom: 4, trailing: 6))
        .textSelection(.disabled)
        .lineLimit(terminal ? nil : 4)
    }
}

struct CIEventView_Previews: PreviewProvider {
    static var previews: some View {
        CIEventView(eventText: Text("event happened"))
    }
}
