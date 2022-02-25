//
//  MarkdownHelp.swift
//  SimpleX
//
//  Created by Evgeny on 24/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct MarkdownHelp: View {
    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("You can use markdown to format messages:")
                .padding(.bottom)
            mdFormat("*bold*", Text("bold text").bold())
            mdFormat("_italic_", Text("italic text").italic())
            mdFormat("~strike~", Text("strikethrough text").strikethrough())
            mdFormat("`code`", Text("`a = b + c`").font(.body.monospaced()))
            mdFormat("!1 colored!", Text("red text").foregroundColor(.red) + Text(" (") + color("1", .red) + color("2", .green) + color("3", .blue) + color("4", .yellow) + color("5", .cyan) + Text("6").foregroundColor(.purple) + Text(")"))
            (
                mdFormat("#secret#", Text("secret text")
                    .foregroundColor(.clear)
                    .underline(color: .primary) + Text(" (can be copied)"))
            )
            .textSelection(.enabled)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding()
    }
}

private func mdFormat(_ format: String, _ example: Text) -> some View {
    HStack {
        Text(format).frame(width: 88, alignment: .leading)
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
