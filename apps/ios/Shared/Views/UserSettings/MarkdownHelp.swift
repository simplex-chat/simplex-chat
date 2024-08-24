//
//  MarkdownHelp.swift
//  SimpleX
//
//  Created by Evgeny on 24/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct MarkdownHelp: View {
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("You can use markdown to format messages:")
                .padding(.bottom)
            mdFormat("*bold*", Text("bold").bold())
            mdFormat("_italic_", Text("italic").italic())
            mdFormat("~strike~", Text("strike").strikethrough())
            mdFormat("`a + b`", Text("`a + b`").font(.body.monospaced()))
            mdFormat("!1 colored!", Text("colored").foregroundColor(.red) + Text(" (") + color("1", .red) + color("2", .green) + color("3", .blue) + color("4", .yellow) + color("5", .cyan) + Text("6").foregroundColor(.purple) + Text(")"))
            (
                mdFormat("#secret#", Text("secret")
                    .foregroundColor(.clear)
                    .underline(color: theme.colors.onBackground) + Text(" (can be copied)"))
            )
            .textSelection(.enabled)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
    }
}

private func mdFormat(_ format: LocalizedStringKey, _ example: Text) -> some View {
    HStack {
        Text(format).frame(width: 120, alignment: .leading)
        example
    }
}

private func color(_ s: String, _ c: Color) -> Text {
    Text(s).foregroundColor(c) + Text(", ")
}

struct MarkdownHelp_Previews: PreviewProvider {
    static var previews: some View {
        MarkdownHelp()
    }
}
